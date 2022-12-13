{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainIndexer.Database
  where

import Cardano.Api (BlockHeader, BlockInMode, CardanoMode, ChainPoint(..), TxInMode)
import Language.Marlowe.Runtime.ChainIndexer.Genesis (GenesisBlock(..))
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

newtype CommitBlocks m = CommitBlocks { runCommitBlocks :: [CardanoBlock] -> m () }

newtype CommitGenesisBlock m = CommitGenesisBlock { runCommitGenesisBlock :: GenesisBlock -> m () }

hoistCommitRollback :: (forall a. m a -> n a) -> CommitRollback m -> CommitRollback n
hoistCommitRollback transformation = CommitRollback . fmap transformation . runCommitRollback

hoistCommitBlocks :: (forall a. m a -> n a) -> CommitBlocks m -> CommitBlocks n
hoistCommitBlocks transformation = CommitBlocks . fmap transformation . runCommitBlocks

hoistCommitGenesisBlock :: (forall a. m a -> n a) -> CommitGenesisBlock m -> CommitGenesisBlock n
hoistCommitGenesisBlock transformation = CommitGenesisBlock . fmap transformation . runCommitGenesisBlock

-- Queries

newtype GetIntersectionPoints m = GetIntersectionPoints
  { runGetIntersectionPoints :: m [WithOrigin BlockHeader] }

newtype GetGenesisBlock m = GetGenesisBlock
  { runGetGenesisBlock :: m (Maybe GenesisBlock) }

hoistGetIntersectionPoints :: (forall a. m a -> n a) -> GetIntersectionPoints m -> GetIntersectionPoints n
hoistGetIntersectionPoints transformation = GetIntersectionPoints . transformation . runGetIntersectionPoints

hoistGetGenesisBlock :: (forall a. m a -> n a) -> GetGenesisBlock m -> GetGenesisBlock n
hoistGetGenesisBlock transformation = GetGenesisBlock . transformation . runGetGenesisBlock

-- Bundles

data DatabaseQueries m = DatabaseQueries
  { commitRollback :: CommitRollback m
  , commitBlocks :: CommitBlocks m
  , commitGenesisBlock :: CommitGenesisBlock m
  , getIntersectionPoints :: GetIntersectionPoints m
  , getGenesisBlock :: GetGenesisBlock m
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} = DatabaseQueries
  { commitBlocks = hoistCommitBlocks transformation commitBlocks
  , commitGenesisBlock = hoistCommitGenesisBlock transformation commitGenesisBlock
  , commitRollback = hoistCommitRollback transformation commitRollback
  , getIntersectionPoints = hoistGetIntersectionPoints transformation getIntersectionPoints
  , getGenesisBlock = hoistGetGenesisBlock transformation getGenesisBlock
  }
