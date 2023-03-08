mod block_acceptor;
mod config;
mod error;
mod event;
mod local_tip_identifier;
mod metrics;
mod sync_identifier;
mod sync_instruction;
#[cfg(test)]
mod tests;

use std::{
    collections::{btree_map, BTreeMap, VecDeque},
    convert::TryInto,
    sync::Arc,
};

use datasize::DataSize;
use futures::FutureExt;
use itertools::Itertools;
use prometheus::Registry;
use tracing::{debug, error, info, warn};

use casper_types::{EraId, TimeDiff, Timestamp};

use crate::{
    components::{
        block_accumulator::{
            block_acceptor::{BlockAcceptor, ShouldStore},
            local_tip_identifier::LocalTipIdentifier,
            metrics::Metrics,
        },
        network::blocklist::BlocklistJustification,
        Component, ValidatorBoundComponent,
    },
    effect::{
        announcements::{
            BlockAccumulatorAnnouncement, FatalAnnouncement, MetaBlockAnnouncement,
            PeerBehaviorAnnouncement,
        },
        requests::{BlockAccumulatorRequest, BlockCompleteConfirmationRequest, StorageRequest},
        EffectBuilder, EffectExt, Effects,
    },
    fatal,
    types::{
        BlockHash, BlockSignatures, FinalitySignature, MetaBlock, MetaBlockState, NodeId,
        ValidatorMatrix,
    },
    NodeRng,
};

pub(crate) use config::Config;
pub(crate) use error::Error;
pub(crate) use event::Event;
pub(crate) use sync_identifier::SyncIdentifier;
pub(crate) use sync_instruction::SyncInstruction;

const COMPONENT_NAME: &str = "block_accumulator";

/// If a peer "informs" us about more than the expected number of new blocks times this factor,
/// they are probably spamming, and we refuse to create new block acceptors for them.
const PEER_RATE_LIMIT_MULTIPLIER: usize = 2;

/// A cache of pending blocks and finality signatures that are gossiped to this node.
///
/// Announces new blocks and finality signatures once they become valid.
#[derive(DataSize, Debug)]
pub(crate) struct BlockAccumulator {
    /// This component requires the era validator weights for every era
    /// it receives blocks and / or finality signatures for to verify that
    /// the received signatures are legitimate to the era and to calculate
    /// sufficient finality from collected finality signatures.
    validator_matrix: ValidatorMatrix,
    /// Each block_acceptor instance is responsible for combining
    /// potential blocks and their finality signatures. When we have
    /// collected sufficient finality weight's worth of signatures
    /// for a potential block, we accept the block and store it.
    block_acceptors: BTreeMap<BlockHash, BlockAcceptor>,
    /// Key is the parent block hash, value is the child block hash.
    /// Used to determine if we have awareness of the next block to be
    /// sync'd or executed.
    block_children: BTreeMap<BlockHash, BlockHash>,
    /// The height of the subjective local tip of the chain. This is used to
    /// keep track of whether blocks received from the network are relevant or not,
    /// and to determine if this node is close enough to the perceived tip of the
    /// network to transition to executing block for itself.
    local_tip: Option<LocalTipIdentifier>,
    /// Configured setting for how close to perceived tip local tip must be for
    /// this node to attempt block execution for itself.
    attempt_execution_threshold: u64,
    /// Configured setting for tolerating a lack of newly received block
    /// and / or finality signature data. If we last saw progress longer
    /// ago than this interval, we will poll the network to determine
    /// if we are caught up or have become isolated.
    dead_air_interval: TimeDiff,
    /// Configured setting for how often to purge dead state.
    purge_interval: TimeDiff,
    /// Configured setting for how many eras are considered to be recent.
    recent_era_interval: u64,
    /// Tracks activity and assists with perceived tip determination.
    last_progress: Timestamp,
    /// For each peer, a list of block hashes we first heard from them, and the timestamp when we
    /// created the block acceptor, from oldest to newest.
    peer_block_timestamps: BTreeMap<NodeId, VecDeque<(BlockHash, Timestamp)>>,
    /// The minimum time between a block and its child.
    min_block_time: TimeDiff,
    /// The number of validator slots.
    validator_slots: u32,
    #[data_size(skip)]
    metrics: Metrics,
}

impl BlockAccumulator {
    pub(crate) fn new(
        config: Config,
        validator_matrix: ValidatorMatrix,
        recent_era_interval: u64,
        min_block_time: TimeDiff,
        validator_slots: u32,
        registry: &Registry,
    ) -> Result<Self, prometheus::Error> {
        Ok(Self {
            validator_matrix,
            attempt_execution_threshold: config.attempt_execution_threshold,
            dead_air_interval: config.dead_air_interval,
            block_acceptors: Default::default(),
            block_children: Default::default(),
            last_progress: Timestamp::now(),
            purge_interval: config.purge_interval,
            local_tip: None,
            recent_era_interval,
            peer_block_timestamps: Default::default(),
            min_block_time,
            validator_slots,
            metrics: Metrics::new(registry)?,
        })
    }

    pub(crate) fn sync_instruction(&mut self, sync_identifier: SyncIdentifier) -> SyncInstruction {
        let had_no_local_tip = self.local_tip.is_none(); // first time thru, leap
        if let Some((block_height, era_id)) = sync_identifier.block_height_and_era() {
            self.register_local_tip(block_height, era_id);
        }
        if had_no_local_tip || self.should_leap(self.maybe_block_height(&sync_identifier)) {
            if had_no_local_tip {
                debug!("Block Accumulator: leap because accumulator is warming up");
            }
            return SyncInstruction::Leap {
                block_hash: sync_identifier.block_hash(),
            };
        }
        let block_hash = sync_identifier.block_hash();
        match sync_identifier.block_hash_to_sync(self.next_synchable_block_hash(block_hash)) {
            Some(sync_block_hash) => {
                self.reset_last_progress();
                SyncInstruction::BlockSync {
                    block_hash: sync_block_hash,
                }
            }
            None => {
                if self.is_stale() {
                    debug!(%block_hash, "Block Accumulator: leap because stale gossip");
                    SyncInstruction::Leap { block_hash }
                } else {
                    SyncInstruction::CaughtUp { block_hash }
                }
            }
        }
    }

    fn maybe_block_height(&self, sync_identifier: &SyncIdentifier) -> Option<u64> {
        // if the sync identifier doesn't have the tip, we may be able
        // to discover it from one of our acceptors
        match sync_identifier.block_height() {
            Some(height) => Some(height),
            None => {
                if let Some(block_acceptor) =
                    self.block_acceptors.get(&sync_identifier.block_hash())
                {
                    block_acceptor.block_height()
                } else {
                    None
                }
            }
        }
    }

    /// Drops all old block acceptors and tracks new local block height;
    /// subsequent attempts to register a block lower than tip will be rejected.
    pub(crate) fn register_local_tip(&mut self, height: u64, era_id: EraId) {
        let new_local_tip = match self.local_tip {
            Some(current) => current.height < height && current.era_id <= era_id,
            None => true,
        };
        if new_local_tip {
            self.purge();
            self.local_tip = Some(LocalTipIdentifier::new(height, era_id));
            info!(local_tip=?self.local_tip, "new local tip detected");
        }
    }

    /// Registers a peer with an existing acceptor, or creates a new one.
    ///
    /// If the era is outdated or the peer has already caused us to create more acceptors than
    /// expected, no new acceptor will be created.
    fn upsert_acceptor(
        &mut self,
        block_hash: BlockHash,
        maybe_era_id: Option<EraId>,
        maybe_sender: Option<NodeId>,
    ) {
        // If the acceptor already exists, just register the peer, if applicable.
        let entry = match self.block_acceptors.entry(block_hash) {
            btree_map::Entry::Occupied(entry) => {
                if let Some(sender) = maybe_sender {
                    entry.into_mut().register_peer(sender);
                }
                return;
            }
            btree_map::Entry::Vacant(entry) => entry,
        };

        // The acceptor doesn't exist. Don't create it if the item's era is not
        // provided or the item's era is older than the local tip era by more
        // than `recent_era_interval`.
        match (maybe_era_id, self.local_tip) {
            (Some(era_id), Some(local_tip))
                if era_id >= local_tip.era_id.saturating_sub(self.recent_era_interval) => {}
            (Some(_), None) => {}
            _ => {
                // If we created the event, it's safe to create the acceptor.
                if maybe_sender.is_some() {
                    debug!(?maybe_era_id, local_tip=?self.local_tip, "not creating acceptor");
                    return;
                }
            }
        }

        // Check that the sender isn't telling us about more blocks than expected.
        if let Some(sender) = maybe_sender {
            let block_timestamps = self.peer_block_timestamps.entry(sender).or_default();

            // Prune the timestamps, so the count reflects only the most recently added acceptors.
            let purge_interval = self.purge_interval;
            while block_timestamps
                .front()
                .map_or(false, |(_, timestamp)| timestamp.elapsed() > purge_interval)
            {
                block_timestamps.pop_front();
            }

            // Assume a block time of at least 1 millisecond, so we don't divide by zero.
            let min_block_time = self.min_block_time.max(TimeDiff::from_millis(1));
            let expected_blocks = (purge_interval / min_block_time) as usize;
            let max_block_count = PEER_RATE_LIMIT_MULTIPLIER.saturating_mul(expected_blocks);
            if block_timestamps.len() >= max_block_count {
                warn!(
                    ?sender, %block_hash,
                    "rejecting block hash from peer who sent us more than {} within {}",
                    max_block_count, self.purge_interval,
                );
                return;
            }
            block_timestamps.push_back((block_hash, Timestamp::now()));
        }

        entry.insert(BlockAcceptor::new(block_hash, maybe_sender));
        self.metrics.block_acceptors.inc();
    }

    fn register_block<REv>(
        &mut self,
        effect_builder: EffectBuilder<REv>,
        meta_block: MetaBlock,
        sender: Option<NodeId>,
    ) -> Effects<Event>
    where
        REv: From<StorageRequest>
            + From<PeerBehaviorAnnouncement>
            + From<BlockCompleteConfirmationRequest>
            + From<FatalAnnouncement>
            + Send,
    {
        let block_hash = meta_block.block.hash();
        debug!(%block_hash, "registering block");
        let era_id = meta_block.block.header().era_id();
        let block_height = meta_block.block.header().height();
        if self
            .local_tip
            .as_ref()
            .map_or(false, |local_tip| block_height < local_tip.height)
        {
            debug!(%block_hash, "ignoring outdated block");
            return Effects::new();
        }
        self.upsert_acceptor(*block_hash, Some(era_id), sender);

        let acceptor = match self.block_acceptors.get_mut(block_hash) {
            None => return Effects::new(),
            Some(acceptor) => acceptor,
        };

        match acceptor.register_block(meta_block, sender) {
            Ok(_) => match self.validator_matrix.validator_weights(era_id) {
                Some(evw) => {
                    let (should_store, faulty_senders) = acceptor.should_store_block(&evw);
                    self.store_block_and_finality_signatures(
                        effect_builder,
                        should_store,
                        faulty_senders,
                    )
                }
                None => Effects::new(),
            },
            Err(error) => match error {
                Error::InvalidGossip(ref gossip_error) => {
                    warn!(%gossip_error, "received invalid block");
                    effect_builder
                        .announce_block_peer_with_justification(
                            gossip_error.peer(),
                            BlocklistJustification::SentBadBlock { error },
                        )
                        .ignore()
                }
                Error::EraMismatch {
                    peer,
                    block_hash,
                    expected,
                    actual,
                } => {
                    warn!(
                        "era mismatch from {} for {}; expected: {} and actual: {}",
                        peer, block_hash, expected, actual
                    );
                    effect_builder
                        .announce_block_peer_with_justification(
                            peer,
                            BlocklistJustification::SentBadBlock { error },
                        )
                        .ignore()
                }
                ref error @ Error::BlockHashMismatch { .. } => {
                    error!(%error, "finality signature has mismatched block_hash; this is a bug");
                    Effects::new()
                }
                ref error @ Error::SufficientFinalityWithoutBlock { .. } => {
                    error!(%error, "should not have sufficient finality without block");
                    Effects::new()
                }
                Error::InvalidConfiguration => fatal!(
                    effect_builder,
                    "node has an invalid configuration, shutting down"
                )
                .ignore(),
                Error::BogusValidator(_) => {
                    error!(%error, "unexpected detection of bogus validator, this is a bug");
                    Effects::new()
                }
                Error::MetaBlockMerge(error) => {
                    error!(%error, "failed to merge meta blocks, this is a bug");
                    Effects::new()
                }
                Error::TooManySignatures { peer, limit } => effect_builder
                    .announce_block_peer_with_justification(
                        peer,
                        BlocklistJustification::SentTooManyFinalitySignatures {
                            max_allowed: limit,
                        },
                    )
                    .ignore(),
            },
        }
    }

    fn register_finality_signature<REv>(
        &mut self,
        effect_builder: EffectBuilder<REv>,
        finality_signature: FinalitySignature,
        sender: Option<NodeId>,
    ) -> Effects<Event>
    where
        REv: From<StorageRequest>
            + From<PeerBehaviorAnnouncement>
            + From<BlockCompleteConfirmationRequest>
            + From<FatalAnnouncement>
            + Send,
    {
        debug!(%finality_signature, "registering finality signature");
        let block_hash = finality_signature.block_hash;
        let era_id = finality_signature.era_id;
        self.upsert_acceptor(block_hash, Some(era_id), sender);

        let acceptor = match self.block_acceptors.get_mut(&block_hash) {
            Some(acceptor) => acceptor,
            // When there is no acceptor for it, this function returns
            // early, ignoring the signature.
            None => return Effects::new(),
        };

        match acceptor.register_finality_signature(finality_signature, sender, self.validator_slots)
        {
            Ok(Some(finality_signature)) => self.store_block_and_finality_signatures(
                effect_builder,
                ShouldStore::SingleSignature(finality_signature),
                None,
            ),
            Ok(None) => match self.validator_matrix.validator_weights(era_id) {
                Some(evw) => {
                    let (should_store, faulty_senders) = acceptor.should_store_block(&evw);
                    self.store_block_and_finality_signatures(
                        effect_builder,
                        should_store,
                        faulty_senders,
                    )
                }
                None => Effects::new(),
            },
            Err(error) => match error {
                Error::InvalidGossip(ref gossip_error) => {
                    warn!(%gossip_error, "received invalid finality_signature");
                    effect_builder
                        .announce_block_peer_with_justification(
                            gossip_error.peer(),
                            BlocklistJustification::SentBadFinalitySignature { error },
                        )
                        .ignore()
                }
                Error::EraMismatch {
                    peer,
                    block_hash,
                    expected,
                    actual,
                } => {
                    // the acceptor logic purges finality signatures that don't match
                    // the era validators, so in this case we can continue to
                    // use the acceptor
                    warn!(
                        "era mismatch from {} for {}; expected: {} and actual: {}",
                        peer, block_hash, expected, actual
                    );
                    effect_builder
                        .announce_block_peer_with_justification(
                            peer,
                            BlocklistJustification::SentBadFinalitySignature { error },
                        )
                        .ignore()
                }
                ref error @ Error::BlockHashMismatch { .. } => {
                    error!(%error, "finality signature has mismatched block_hash; this is a bug");
                    Effects::new()
                }
                ref error @ Error::SufficientFinalityWithoutBlock { .. } => {
                    error!(%error, "should not have sufficient finality without block");
                    Effects::new()
                }
                Error::InvalidConfiguration => fatal!(
                    effect_builder,
                    "node has an invalid configuration, shutting down"
                )
                .ignore(),
                Error::BogusValidator(_) => {
                    error!(%error, "unexpected detection of bogus validator, this is a bug");
                    Effects::new()
                }
                Error::MetaBlockMerge(error) => {
                    error!(%error, "failed to merge meta blocks, this is a bug");
                    Effects::new()
                }
                Error::TooManySignatures { peer, limit } => effect_builder
                    .announce_block_peer_with_justification(
                        peer,
                        BlocklistJustification::SentTooManyFinalitySignatures {
                            max_allowed: limit,
                        },
                    )
                    .ignore(),
            },
        }
    }

    fn register_stored<REv>(
        &self,
        effect_builder: EffectBuilder<REv>,
        maybe_meta_block: Option<MetaBlock>,
        maybe_block_signatures: Option<BlockSignatures>,
    ) -> Effects<Event>
    where
        REv: From<BlockAccumulatorAnnouncement>
            + From<BlockCompleteConfirmationRequest>
            + From<MetaBlockAnnouncement>
            + Send,
    {
        let mut effects = Effects::new();
        if let Some(meta_block) = maybe_meta_block {
            effects.extend(effect_builder.announce_meta_block(meta_block).ignore());
        };
        if let Some(block_signatures) = maybe_block_signatures {
            for finality_signature in block_signatures.finality_signatures() {
                effects.extend(
                    effect_builder
                        .announce_finality_signature_accepted(Box::new(finality_signature))
                        .ignore(),
                );
            }
        }
        effects
    }

    fn highest_usable_block_height(&self) -> Option<u64> {
        let mut ret = self.local_tip.map(|local_tip| local_tip.height);
        for block_acceptor in self.block_acceptors.values() {
            if false == block_acceptor.has_sufficient_finality() {
                continue;
            }
            match block_acceptor.block_height() {
                None => {
                    continue;
                }
                Some(acceptor_height) => {
                    if let Some(curr_height) = ret {
                        if acceptor_height <= curr_height {
                            continue;
                        }
                    }
                    ret = Some(acceptor_height);
                }
            };
        }
        ret
    }

    fn get_peers(&self, block_hash: BlockHash) -> Option<Vec<NodeId>> {
        self.block_acceptors
            .get(&block_hash)
            .map(|acceptor| acceptor.peers().iter().cloned().collect())
    }

    fn is_stale(&mut self) -> bool {
        // we expect to be receiving gossiped blocks from other nodes
        // if we haven't received any messages describing higher blocks
        // for more than the self.dead_air_interval config allows
        // we leap again to poll the network
        self.last_progress.elapsed() >= self.dead_air_interval
    }

    pub(crate) fn reset_last_progress(&mut self) {
        self.last_progress = Timestamp::now();
    }

    fn should_leap(&self, maybe_block_height: Option<u64>) -> bool {
        match (maybe_block_height, self.highest_usable_block_height()) {
            (None, _) => true,
            (Some(_), None) => true,
            (Some(block_height), Some(highest_usable_block_height)) => {
                let height_diff = highest_usable_block_height.saturating_sub(block_height);
                if height_diff > self.attempt_execution_threshold {
                    info!(
                        height_diff,
                        attempt_execution_threshold = self.attempt_execution_threshold,
                        "Block Accumulator: leap because height diff is larger than attempt execution threshold"
                    );
                    true
                } else {
                    false
                }
            }
        }
    }

    fn next_synchable_block_hash(&self, parent_block_hash: BlockHash) -> Option<BlockHash> {
        let child_hash = self.block_children.get(&parent_block_hash)?;
        let block_acceptor = self.block_acceptors.get(child_hash)?;
        if block_acceptor.has_sufficient_finality() {
            Some(block_acceptor.block_hash())
        } else {
            None
        }
    }

    fn purge(&mut self) {
        let now = Timestamp::now();
        let mut purged = vec![];
        let purge_interval = self.purge_interval;
        let maybe_local_tip_height = self.local_tip.map(|local_tip| local_tip.height);
        let attempt_execution_threshold = self.attempt_execution_threshold;
        self.block_acceptors.retain(|k, v| {
            if let (Some(acceptor_height), Some(local_tip_height)) =
                (v.block_height(), maybe_local_tip_height)
            {
                if acceptor_height >= local_tip_height.saturating_sub(attempt_execution_threshold)
                    && acceptor_height <= local_tip_height
                {
                    return true;
                }
            }
            let expired = now.saturating_diff(v.last_progress()) > purge_interval;
            if expired {
                purged.push(*k)
            }
            !expired
        });
        self.block_children
            .retain(|_parent, child| false == purged.contains(child));
        self.peer_block_timestamps.retain(|_, block_timestamps| {
            while block_timestamps
                .front()
                .map_or(false, |(_, timestamp)| timestamp.elapsed() > purge_interval)
            {
                block_timestamps.pop_front();
            }
            !block_timestamps.is_empty()
        });

        self.metrics
            .block_acceptors
            .set(self.block_acceptors.len().try_into().unwrap_or(i64::MIN));
        self.metrics
            .known_child_blocks
            .set(self.block_children.len().try_into().unwrap_or(i64::MIN));
    }

    fn update_block_children(&mut self, meta_block: &MetaBlock) {
        if let Some(parent_hash) = meta_block.block.parent() {
            if self
                .block_children
                .insert(*parent_hash, *meta_block.block.hash())
                .is_none()
            {
                self.metrics.known_child_blocks.inc();
            }
        }
    }

    fn store_block_and_finality_signatures<REv, I>(
        &mut self,
        effect_builder: EffectBuilder<REv>,
        should_store: ShouldStore,
        faulty_senders: I,
    ) -> Effects<Event>
    where
        REv: From<PeerBehaviorAnnouncement>
            + From<StorageRequest>
            + From<BlockCompleteConfirmationRequest>
            + Send,
        I: IntoIterator<Item = (NodeId, Error)>,
    {
        let mut effects = match should_store {
            ShouldStore::SufficientlySignedBlock {
                meta_block,
                block_signatures,
            } => {
                let block_hash = meta_block.block.hash();
                debug!(%block_hash, "storing block and finality signatures");
                self.update_block_children(&meta_block);
                // The block wasn't executed yet, so we just put it to storage. An `ExecutedBlock`
                // event will then re-trigger this flow and eventually mark it complete.
                let cloned_signatures = block_signatures.clone();
                effect_builder
                    .put_block_to_storage(Arc::clone(&meta_block.block))
                    .then(move |_| effect_builder.put_signatures_to_storage(cloned_signatures))
                    .event(move |_| Event::Stored {
                        maybe_meta_block: Some(meta_block),
                        maybe_block_signatures: Some(block_signatures),
                    })
            }
            ShouldStore::CompletedBlock {
                meta_block,
                block_signatures,
            } => {
                let block_hash = meta_block.block.hash();
                debug!(%block_hash, "storing finality signatures and marking block complete");
                self.update_block_children(&meta_block);
                // The block was already executed, which means it is stored and we have the global
                // state for it. As on this code path we also know it is sufficiently signed,
                // we mark it as complete.
                let block_height = meta_block.block.height();
                effect_builder
                    .put_signatures_to_storage(block_signatures.clone())
                    .then(move |_| effect_builder.mark_block_completed(block_height))
                    .event(move |_| Event::Stored {
                        maybe_meta_block: Some(meta_block),
                        maybe_block_signatures: Some(block_signatures),
                    })
            }
            ShouldStore::MarkComplete(meta_block) => {
                let block_hash = meta_block.block.hash();
                debug!(%block_hash, "marking block complete");
                let block_height = meta_block.block.height();
                effect_builder
                    .mark_block_completed(block_height)
                    .event(move |_| Event::Stored {
                        maybe_meta_block: Some(meta_block),
                        maybe_block_signatures: None,
                    })
            }
            ShouldStore::SingleSignature(signature) => {
                debug!(%signature, "storing finality signature");
                let mut block_signatures =
                    BlockSignatures::new(signature.block_hash, signature.era_id);
                block_signatures.insert_proof(signature.public_key.clone(), signature.signature);
                effect_builder
                    .put_finality_signature_to_storage(signature)
                    .event(move |_| Event::Stored {
                        maybe_meta_block: None,
                        maybe_block_signatures: Some(block_signatures),
                    })
            }
            ShouldStore::Nothing => {
                debug!("not storing block or finality signatures");
                Effects::new()
            }
        };
        effects.extend(faulty_senders.into_iter().flat_map(|(node_id, error)| {
            effect_builder
                .announce_block_peer_with_justification(
                    node_id,
                    BlocklistJustification::SentBadFinalitySignature { error },
                )
                .ignore()
        }));
        effects
    }
}

pub(crate) trait ReactorEvent:
    From<StorageRequest>
    + From<PeerBehaviorAnnouncement>
    + From<BlockAccumulatorAnnouncement>
    + From<BlockCompleteConfirmationRequest>
    + From<MetaBlockAnnouncement>
    + From<FatalAnnouncement>
    + Send
    + 'static
{
}

impl<REv> ReactorEvent for REv where
    REv: From<StorageRequest>
        + From<PeerBehaviorAnnouncement>
        + From<BlockAccumulatorAnnouncement>
        + From<BlockCompleteConfirmationRequest>
        + From<MetaBlockAnnouncement>
        + From<FatalAnnouncement>
        + Send
        + 'static
{
}

impl<REv: ReactorEvent> Component<REv> for BlockAccumulator {
    type Event = Event;

    fn handle_event(
        &mut self,
        effect_builder: EffectBuilder<REv>,
        _rng: &mut NodeRng,
        event: Self::Event,
    ) -> Effects<Self::Event> {
        match event {
            Event::Request(BlockAccumulatorRequest::GetPeersForBlock {
                block_hash,
                responder,
            }) => responder.respond(self.get_peers(block_hash)).ignore(),
            Event::RegisterPeer {
                block_hash,
                era_id,
                sender,
            } => {
                self.upsert_acceptor(block_hash, era_id, Some(sender));
                Effects::new()
            }
            Event::ReceivedBlock { block, sender } => {
                let meta_block = MetaBlock::new(block, vec![], MetaBlockState::new());
                self.register_block(effect_builder, meta_block, Some(sender))
            }
            Event::CreatedFinalitySignature { finality_signature } => {
                self.register_finality_signature(effect_builder, *finality_signature, None)
            }
            Event::ReceivedFinalitySignature {
                finality_signature,
                sender,
            } => {
                self.register_finality_signature(effect_builder, *finality_signature, Some(sender))
            }
            Event::ExecutedBlock { meta_block } => {
                let height = meta_block.block.header().height();
                let era_id = meta_block.block.header().era_id();
                self.register_local_tip(height, era_id);
                self.register_block(effect_builder, meta_block, None)
            }
            Event::Stored {
                maybe_meta_block,
                maybe_block_signatures,
            } => self.register_stored(effect_builder, maybe_meta_block, maybe_block_signatures),
        }
    }

    fn name(&self) -> &str {
        COMPONENT_NAME
    }
}

impl<REv: ReactorEvent> ValidatorBoundComponent<REv> for BlockAccumulator {
    fn handle_validators(
        &mut self,
        effect_builder: EffectBuilder<REv>,
        _: &mut NodeRng,
    ) -> Effects<Self::Event> {
        info!("BlockAccumulator: handling updated validator matrix");
        let validator_matrix = &self.validator_matrix; // Closure can't borrow all of self.
        let should_stores = self
            .block_acceptors
            .values_mut()
            .filter(|acceptor| false == acceptor.has_sufficient_finality())
            .filter_map(|acceptor| {
                let era_id = acceptor.era_id()?;
                let evw = validator_matrix.validator_weights(era_id)?;
                Some(acceptor.should_store_block(&evw))
            })
            .collect_vec();
        should_stores
            .into_iter()
            .flat_map(|(should_store, faulty_senders)| {
                self.store_block_and_finality_signatures(
                    effect_builder,
                    should_store,
                    faulty_senders,
                )
            })
            .collect()
    }
}
