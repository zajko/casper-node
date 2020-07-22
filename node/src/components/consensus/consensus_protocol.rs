use std::fmt::Debug;

use anyhow::Error;

use crate::{components::consensus::traits::ConsensusValueT, types::Timestamp};

mod protocol_state;
pub(crate) mod synchronizer;

pub(crate) use protocol_state::{ProtocolState, VertexTrait};

/// Information about the context in which a new block is created.
#[derive(Clone, Eq, PartialEq, Debug, Ord, PartialOrd)]
pub struct BlockContext {
    timestamp: Timestamp,
}

impl BlockContext {
    /// Constructs a new `BlockContext`
    pub(crate) fn new(timestamp: Timestamp) -> Self {
        BlockContext { timestamp }
    }

    /// The block's timestamp.
    pub(crate) fn timestamp(&self) -> Timestamp {
        self.timestamp
    }
}

#[derive(Debug)]
pub(crate) enum ConsensusProtocolResult<I, C: ConsensusValueT, VID> {
    CreatedGossipMessage(Vec<u8>),
    CreatedTargetedMessage(Vec<u8>, I),
    InvalidIncomingMessage(Vec<u8>, I, Error),
    ScheduleTimer(Timestamp),
    /// Request deploys for a new block, whose timestamp will be the given `u64`.
    /// TODO: Add more details that are necessary for block creation.
    CreateNewBlock(BlockContext),
    /// A block was finalized. The timestamp is from when the block was proposed.
    FinalizedBlock {
        value: C,
        new_equivocators: Vec<VID>,
        timestamp: Timestamp,
    },
    /// Request validation of the consensus value, contained in a message received from the given
    /// node.
    ///
    /// The domain logic should verify any intrinsic validity conditions of consensus values, e.g.
    /// that it has the expected structure, or that deploys that are mentioned by hash actually
    /// exist, and then call `ConsensusProtocol::resolve_validity`.
    ValidateConsensusValue(I, C),
}

/// An API for a single instance of the consensus.
pub(crate) trait ConsensusProtocol<I, C: ConsensusValueT, VID> {
    /// Handles an incoming message (like NewVote, RequestDependency).
    fn handle_message(
        &mut self,
        sender: I,
        msg: Vec<u8>,
    ) -> Result<Vec<ConsensusProtocolResult<I, C, VID>>, Error>;

    /// Triggers consensus' timer.
    fn handle_timer(
        &mut self,
        timerstamp: Timestamp,
    ) -> Result<Vec<ConsensusProtocolResult<I, C, VID>>, Error>;

    /// Proposes a new value for consensus.
    fn propose(
        &mut self,
        value: C,
        block_context: BlockContext,
    ) -> Result<Vec<ConsensusProtocolResult<I, C, VID>>, Error>;

    /// Marks the `value` as valid or invalid, based on validation requested via
    /// `ConsensusProtocolResult::ValidateConsensusvalue`.
    fn resolve_validity(
        &mut self,
        value: &C,
        valid: bool,
    ) -> Result<Vec<ConsensusProtocolResult<I, C, VID>>, Error>;
}

#[cfg(test)]
mod example {
    use serde::{Deserialize, Serialize};

    use super::{
        protocol_state::{ProtocolState, VertexTrait},
        synchronizer::DagSynchronizerState,
        BlockContext, ConsensusProtocol, ConsensusProtocolResult, Timestamp,
    };

    #[derive(Debug, Hash, PartialEq, Eq, Clone, PartialOrd, Ord)]
    struct VIdU64(u64);

    #[derive(Debug, Hash, PartialEq, Eq, Clone, Serialize, Deserialize)]
    struct DummyVertex {
        id: u64,
        proto_block: ProtoBlock,
    }

    impl VertexTrait for DummyVertex {
        type Id = VIdU64;
        type Value = ProtoBlock;

        fn id(&self) -> VIdU64 {
            VIdU64(self.id)
        }

        fn value(&self) -> Option<&ProtoBlock> {
            Some(&self.proto_block)
        }
    }

    #[derive(Debug, Hash, PartialEq, Eq, Clone, Serialize, Deserialize)]
    struct ProtoBlock(u64);

    #[derive(Debug)]
    struct Error;

    type CpResult<I> = Result<Vec<ConsensusProtocolResult<I, ProtoBlock, VIdU64>>, anyhow::Error>;

    impl<I, P: ProtocolState> ConsensusProtocol<I, ProtoBlock, VIdU64> for DagSynchronizerState<I, P> {
        fn handle_message(&mut self, _sender: I, _msg: Vec<u8>) -> CpResult<I> {
            unimplemented!()
        }

        fn handle_timer(&mut self, _timestamp: Timestamp) -> CpResult<I> {
            unimplemented!()
        }

        fn resolve_validity(&mut self, _value: &ProtoBlock, _valid: bool) -> CpResult<I> {
            unimplemented!()
        }

        fn propose(&mut self, _value: ProtoBlock, _block_context: BlockContext) -> CpResult<I> {
            unimplemented!()
        }
    }
}
