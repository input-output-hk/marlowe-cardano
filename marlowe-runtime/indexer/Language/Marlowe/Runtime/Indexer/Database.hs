{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Indexer.Database
  where

import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint)
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock)

data DatabaseQueries m = DatabaseQueries
  { commitRollback :: ChainPoint -> m ()
  , commitBlocks :: [MarloweBlock] -> m ()
  , getIntersectionPoints :: m [BlockHeader]
  }

hoistDatabaseQueries :: (forall a. m a -> n a) -> DatabaseQueries m -> DatabaseQueries n
hoistDatabaseQueries transformation DatabaseQueries{..} = DatabaseQueries
  { commitBlocks = transformation <$> commitBlocks
  , commitRollback = transformation <$> commitRollback
  , getIntersectionPoints = transformation getIntersectionPoints
  }
