{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.ChainSync.Database
  where

import Cardano.Api (BlockHeader, BlockInMode, CardanoMode, ChainPoint(..), TxInMode)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Api
import Language.Marlowe.Runtime.ChainSync.Genesis (GenesisBlock(..))
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

newtype GetHeaderAtPoint m = GetHeaderAtPoint
  { runGetHeaderAtPoint :: ChainPoint -> m (WithOrigin BlockHeader) }

newtype GetIntersectionPoints m = GetIntersectionPoints
  { runGetIntersectionPoints :: m [ChainPoint] }

newtype GetGenesisBlock m = GetGenesisBlock
  { runGetGenesisBlock :: m (Maybe GenesisBlock) }

newtype GetUTxOs m = GetUTxOs
  { runGetUTxOs :: Set Api.Address -> m (Map Api.TxOutRef Api.TransactionOutput) }

data MoveResult err result
  = RollForward result Api.BlockHeader Api.ChainPoint
  | RollBack Api.ChainPoint Api.ChainPoint
  | Reject err Api.ChainPoint
  | Wait Api.ChainPoint

newtype MoveClient m = MoveClient
  { runMoveClient :: forall err result. Api.ChainPoint -> Api.Move err result -> m (MoveResult err result) }

hoistGetHeaderAtPoint :: (forall a. m a -> n a) -> GetHeaderAtPoint m -> GetHeaderAtPoint n
hoistGetHeaderAtPoint transformation = GetHeaderAtPoint . fmap transformation . runGetHeaderAtPoint

hoistGetIntersectionPoints :: (forall a. m a -> n a) -> GetIntersectionPoints m -> GetIntersectionPoints n
hoistGetIntersectionPoints transformation = GetIntersectionPoints . transformation . runGetIntersectionPoints

hoistGetGenesisBlock :: (forall a. m a -> n a) -> GetGenesisBlock m -> GetGenesisBlock n
hoistGetGenesisBlock transformation = GetGenesisBlock . transformation . runGetGenesisBlock

hoistGetUTxOs :: (forall a. m a -> n a) -> GetUTxOs m -> GetUTxOs n
hoistGetUTxOs transformation = GetUTxOs . fmap transformation . runGetUTxOs

hoistMoveClient :: (forall a. m a -> n a) -> MoveClient m -> MoveClient n
hoistMoveClient transformation (MoveClient runMoveClient) =
  MoveClient $ fmap transformation . runMoveClient


-- Bundles

data DatabaseQueries m = DatabaseQueries
  { commitRollback        :: !(CommitRollback m)
  , commitBlocks          :: !(CommitBlocks m)
  , commitGenesisBlock    :: !(CommitGenesisBlock m)
  , getHeaderAtPoint      :: !(GetHeaderAtPoint m)
  , getIntersectionPoints :: !(GetIntersectionPoints m)
  , getGenesisBlock       :: !(GetGenesisBlock m)
  , getUTxOs              :: !(GetUTxOs m)
  , moveClient            :: !(MoveClient m)
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} = DatabaseQueries
  { commitBlocks = hoistCommitBlocks transformation commitBlocks
  , commitGenesisBlock = hoistCommitGenesisBlock transformation commitGenesisBlock
  , commitRollback = hoistCommitRollback transformation commitRollback
  , getHeaderAtPoint = hoistGetHeaderAtPoint transformation getHeaderAtPoint
  , getIntersectionPoints = hoistGetIntersectionPoints transformation getIntersectionPoints
  , getGenesisBlock = hoistGetGenesisBlock transformation getGenesisBlock
  , getUTxOs = hoistGetUTxOs transformation getUTxOs
  , moveClient = hoistMoveClient transformation moveClient
  }
