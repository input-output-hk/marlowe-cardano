module Language.Marlowe.Runtime.ChainSync.Database where

import Cardano.Api (BlockHeader, BlockInMode, CardanoMode, ChainPoint (..))
import Ouroboros.Network.Point (WithOrigin)

type CardanoBlock = BlockInMode CardanoMode

-- Commands

newtype CommitRollback m = CommitRollback { runCommitRollback :: ChainPoint -> m () }
newtype CommitBlocks m = CommitBlocks { runCommitBlocks :: [CardanoBlock] -> m () }

-- Queries

newtype GetHeaderAtPoint m = GetHeaderAtPoint
  { runGetHeaderAtPoint :: ChainPoint -> m (WithOrigin BlockHeader) }
newtype GetIntersectionPoints m = GetIntersectionPoints
  { runGetIntersectionPoints :: ChainPoint -> Maybe ChainPoint -> m [ChainPoint] }
