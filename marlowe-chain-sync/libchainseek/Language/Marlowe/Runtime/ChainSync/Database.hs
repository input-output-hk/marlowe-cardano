{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Database
  where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, GetUTxOsQuery, Move, UTxOs)

-- Queries

newtype GetTip m = GetTip
  { runGetTip :: m ChainPoint }

newtype GetUTxOs m = GetUTxOs
  { runGetUTxOs :: GetUTxOsQuery -> m UTxOs }

data MoveResult err result
  = RollForward result BlockHeader ChainPoint
  | RollBack ChainPoint ChainPoint
  | Reject err ChainPoint
  | Wait ChainPoint

newtype MoveClient m = MoveClient
  { runMoveClient :: forall err result. ChainPoint -> Move err result -> m (MoveResult err result) }

hoistGetUTxOs :: (forall a. m a -> n a) -> GetUTxOs m -> GetUTxOs n
hoistGetUTxOs transformation = GetUTxOs . fmap transformation . runGetUTxOs

hoistGetTip :: (forall a. m a -> n a) -> GetTip m -> GetTip n
hoistGetTip transformation = GetTip . transformation . runGetTip

hoistMoveClient :: (forall a. m a -> n a) -> MoveClient m -> MoveClient n
hoistMoveClient transformation (MoveClient runMoveClient) =
  MoveClient $ fmap transformation . runMoveClient

-- Bundles

data DatabaseQueries m = DatabaseQueries
  { getUTxOs :: GetUTxOs m
  , getTip :: GetTip m
  , moveClient :: MoveClient m
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} = DatabaseQueries
  { getUTxOs = hoistGetUTxOs transformation getUTxOs
  , getTip = hoistGetTip transformation getTip
  , moveClient = hoistMoveClient transformation moveClient
  }
