{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync.Database where

import Cardano.Api (BlockHeader, BlockInMode, CardanoMode, ChainPoint (..), TxInMode)
import Ouroboros.Network.Point (WithOrigin)

type CardanoBlock = BlockInMode CardanoMode
type CardanoTx = TxInMode CardanoMode

-- Commands

newtype CommitRollback m = CommitRollback { runCommitRollback :: ChainPoint -> m () }
newtype CommitBlocks m = CommitBlocks { runCommitBlocks :: [CardanoBlock] -> m () }

hoistCommitRollback :: (forall a. m a -> n a) -> CommitRollback m -> CommitRollback n
hoistCommitRollback transformation = CommitRollback . fmap transformation . runCommitRollback

hoistCommitBlocks :: (forall a. m a -> n a) -> CommitBlocks m -> CommitBlocks n
hoistCommitBlocks transformation = CommitBlocks . fmap transformation . runCommitBlocks

-- Queries

newtype GetHeaderAtPoint m = GetHeaderAtPoint
  { runGetHeaderAtPoint :: ChainPoint -> m (WithOrigin BlockHeader) }
newtype GetIntersectionPoints m = GetIntersectionPoints
  { runGetIntersectionPoints :: ChainPoint -> Maybe ChainPoint -> m [ChainPoint] }

hoistGetHeaderAtPoint :: (forall a. m a -> n a) -> GetHeaderAtPoint m -> GetHeaderAtPoint n
hoistGetHeaderAtPoint transformation = GetHeaderAtPoint . fmap transformation . runGetHeaderAtPoint

hoistGetIntersectionPoints :: (forall a. m a -> n a) -> GetIntersectionPoints m -> GetIntersectionPoints n
hoistGetIntersectionPoints transformation = GetIntersectionPoints . (fmap . fmap) transformation . runGetIntersectionPoints

-- Bundles

data DatabaseQueries m = DatabaseQueries
  { commitRollback        :: !(CommitRollback m)
  , commitBlocks          :: !(CommitBlocks m)
  , getHeaderAtPoint      :: !(GetHeaderAtPoint m)
  , getIntersectionPoints :: !(GetIntersectionPoints m)
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} = DatabaseQueries
  { commitBlocks = hoistCommitBlocks transformation commitBlocks
  , commitRollback = hoistCommitRollback transformation commitRollback
  , getHeaderAtPoint = hoistGetHeaderAtPoint transformation getHeaderAtPoint
  , getIntersectionPoints = hoistGetIntersectionPoints transformation getIntersectionPoints
  }
