{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Database where

import Data.List.NonEmpty (NonEmpty)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, GetUTxOsQuery, Move, UTxOs)
import Numeric.Natural (Natural)

-- Queries

newtype GetTip m = GetTip
  {runGetTip :: m ChainPoint}

newtype GetUTxOs m = GetUTxOs
  {runGetUTxOs :: GetUTxOsQuery -> m UTxOs}

data MoveResult err result
  = RollForward result BlockHeader ChainPoint
  | RollBack ChainPoint ChainPoint
  | Reject err ChainPoint
  | Wait ChainPoint

newtype MoveClient m = MoveClient
  {runMoveClient :: forall err result. ChainPoint -> Move err result -> m (MoveResult err result)}

newtype Scan m = Scan
  {runScan :: forall err result. ChainPoint -> Move err result -> m (Collect err result m)}

data CollectResult err result m
  = NextBlocks (NonEmpty (BlockHeader, result)) ChainPoint (Collect err result m)
  | CollectRollBack ChainPoint ChainPoint
  | CollectReject err ChainPoint
  | CollectWait ChainPoint

newtype Collect err result m = Collect
  {runCollect :: Natural -> m (CollectResult err result m)}

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

hoistCollect :: (Functor m) => (forall a. m a -> n a) -> Collect err result m -> Collect err result n
hoistCollect transformation (Collect runCollect) =
  Collect $ transformation . fmap (hoistCollectResult transformation) . runCollect

hoistCollectResult :: (Functor m) => (forall a. m a -> n a) -> CollectResult err result m -> CollectResult err result n
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
