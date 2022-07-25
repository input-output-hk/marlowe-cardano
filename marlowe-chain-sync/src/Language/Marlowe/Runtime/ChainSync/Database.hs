{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync.Database where

import Cardano.Api (BlockHeader, BlockInMode, CardanoMode, ChainPoint (..), ChainTip, TxInMode)
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock (..))
import Ouroboros.Network.Point (WithOrigin)

type CardanoBlock = BlockInMode CardanoMode
type CardanoTx = TxInMode CardanoMode

-- Commands

newtype CommitRollback m = CommitRollback { runCommitRollback :: ChainPoint -> m () }

instance Applicative m => Semigroup (CommitRollback m) where
  CommitRollback a <> CommitRollback b = CommitRollback \point -> a point *> b point

instance Applicative m => Monoid (CommitRollback m) where
  mempty = CommitRollback \_ -> pure mempty
  mappend = (<>)

newtype CommitBlocks m = CommitBlocks { runCommitBlocks :: [CardanoBlock] -> ChainPoint -> ChainTip -> m () }

instance Applicative m => Semigroup (CommitBlocks m) where
  CommitBlocks a <> CommitBlocks b = CommitBlocks \blocks -> a blocks *> b blocks

instance Applicative m => Monoid (CommitBlocks m) where
  mempty = CommitBlocks \_ _ _ -> pure mempty
  mappend = (<>)

newtype CommitGenesisBlock m = CommitGenesisBlock { runCommitGenesisBlock :: GenesisBlock -> m () }

instance Applicative m => Semigroup (CommitGenesisBlock m) where
  CommitGenesisBlock a <> CommitGenesisBlock b = CommitGenesisBlock \block -> a block *> b block

instance Applicative m => Monoid (CommitGenesisBlock m) where
  mempty = CommitGenesisBlock \_ -> pure mempty
  mappend = (<>)

hoistCommitRollback :: (forall a. m a -> n a) -> CommitRollback m -> CommitRollback n
hoistCommitRollback transformation = CommitRollback . fmap transformation . runCommitRollback

hoistCommitBlocks :: (forall a. m a -> n a) -> CommitBlocks m -> CommitBlocks n
hoistCommitBlocks transformation = CommitBlocks . (fmap . fmap . fmap) transformation . runCommitBlocks

hoistCommitGenesisBlock :: (forall a. m a -> n a) -> CommitGenesisBlock m -> CommitGenesisBlock n
hoistCommitGenesisBlock transformation = CommitGenesisBlock . fmap transformation . runCommitGenesisBlock

-- Queries

newtype GetHeaderAtPoint m = GetHeaderAtPoint
  { runGetHeaderAtPoint :: ChainPoint -> m (WithOrigin BlockHeader) }

newtype GetIntersectionPoints m = GetIntersectionPoints
  { runGetIntersectionPoints :: ChainPoint -> Maybe ChainPoint -> m [ChainPoint] }

newtype GetGenesisBlock m = GetGenesisBlock
  { runGetGenesisBlock :: m (Maybe GenesisBlock) }

hoistGetHeaderAtPoint :: (forall a. m a -> n a) -> GetHeaderAtPoint m -> GetHeaderAtPoint n
hoistGetHeaderAtPoint transformation = GetHeaderAtPoint . fmap transformation . runGetHeaderAtPoint

hoistGetIntersectionPoints :: (forall a. m a -> n a) -> GetIntersectionPoints m -> GetIntersectionPoints n
hoistGetIntersectionPoints transformation = GetIntersectionPoints . (fmap . fmap) transformation . runGetIntersectionPoints

hoistGetGenesisBlock :: (forall a. m a -> n a) -> GetGenesisBlock m -> GetGenesisBlock n
hoistGetGenesisBlock transformation = GetGenesisBlock . transformation . runGetGenesisBlock

-- Bundles

data DatabaseQueries m = DatabaseQueries
  { commitRollback        :: !(CommitRollback m)
  , commitBlocks          :: !(CommitBlocks m)
  , commitGenesisBlock    :: !(CommitGenesisBlock m)
  , getHeaderAtPoint      :: !(GetHeaderAtPoint m)
  , getIntersectionPoints :: !(GetIntersectionPoints m)
  , getGenesisBlock       :: !(GetGenesisBlock m)
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} = DatabaseQueries
  { commitBlocks = hoistCommitBlocks transformation commitBlocks
  , commitGenesisBlock = hoistCommitGenesisBlock transformation commitGenesisBlock
  , commitRollback = hoistCommitRollback transformation commitRollback
  , getHeaderAtPoint = hoistGetHeaderAtPoint transformation getHeaderAtPoint
  , getIntersectionPoints = hoistGetIntersectionPoints transformation getIntersectionPoints
  , getGenesisBlock = hoistGetGenesisBlock transformation getGenesisBlock
  }
