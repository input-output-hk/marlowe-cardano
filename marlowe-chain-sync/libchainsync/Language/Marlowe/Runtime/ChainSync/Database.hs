{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Database where

import Data.List.NonEmpty (NonEmpty)
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader,
  ChainPoint,
  ChainSyncMove,
  GetUTxOsQuery,
  Move,
  SeekError,
  SeekResult,
  UTxOs,
 )
import Numeric.Natural (Natural)

-- Queries

newtype GetTip m = GetTip
  {runGetTip :: m ChainPoint}

newtype GetUTxOs m = GetUTxOs
  {runGetUTxOs :: GetUTxOsQuery -> m UTxOs}

data MoveResult (t :: ChainSyncMove)
  = RollForward (SeekResult t) BlockHeader ChainPoint
  | RollBack ChainPoint ChainPoint
  | Reject (SeekError t) ChainPoint
  | Wait ChainPoint

newtype MoveClient m = MoveClient
  {runMoveClient :: forall t. ChainPoint -> Move t -> m (MoveResult t)}

newtype Scan m = Scan
  {runScan :: forall t. ChainPoint -> Move t -> m (Collect t m)}

data CollectResult t m
  = NextBlocks (NonEmpty (BlockHeader, SeekResult t)) ChainPoint (Collect t m)
  | CollectRollBack ChainPoint ChainPoint
  | CollectReject (SeekError t) ChainPoint
  | CollectWait ChainPoint

newtype Collect (t :: ChainSyncMove) m = Collect
  {runCollect :: Natural -> m (CollectResult t m)}

hoistGetUTxOs :: (forall a. m a -> n a) -> GetUTxOs m -> GetUTxOs n
hoistGetUTxOs transformation = GetUTxOs . fmap transformation . runGetUTxOs

hoistGetTip :: (forall a. m a -> n a) -> GetTip m -> GetTip n
hoistGetTip transformation = GetTip . transformation . runGetTip

hoistMoveClient :: (forall a. m a -> n a) -> MoveClient m -> MoveClient n
hoistMoveClient transformation (MoveClient runMoveClient) =
  MoveClient $ fmap transformation . runMoveClient

hoistScan :: (Functor m) => (forall a. m a -> n a) -> Scan m -> Scan n
hoistScan transformation (Scan runScan) =
  Scan $ fmap (transformation . fmap (hoistCollect transformation)) . runScan

hoistCollect :: (Functor m) => (forall a. m a -> n a) -> Collect t m -> Collect t n
hoistCollect transformation (Collect runCollect) =
  Collect $ transformation . fmap (hoistCollectResult transformation) . runCollect

hoistCollectResult :: (Functor m) => (forall a. m a -> n a) -> CollectResult t m -> CollectResult t n
hoistCollectResult transformation = \case
  NextBlocks results tip collect -> NextBlocks results tip $ hoistCollect transformation collect
  CollectRollBack point tip -> CollectRollBack point tip
  CollectReject err tip -> CollectReject err tip
  CollectWait tip -> CollectWait tip

-- Bundles

data DatabaseQueries m = DatabaseQueries
  { getUTxOs :: GetUTxOs m
  , getTip :: GetTip m
  , moveClient :: MoveClient m
  , scan :: Scan m
  }

hoistDatabaseQueries :: (Functor m) => (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} =
  DatabaseQueries
    { getUTxOs = hoistGetUTxOs transformation getUTxOs
    , getTip = hoistGetTip transformation getTip
    , moveClient = hoistMoveClient transformation moveClient
    , scan = hoistScan transformation scan
    }
