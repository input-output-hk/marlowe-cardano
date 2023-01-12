{-# LANGUAGE Arrows #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Indexer.Store
  where

import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, modifyTVar, newTVar, readTVar, writeTVar)
import Control.Monad (forever, guard, unless)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (partition)
import Data.Semigroup (Last(..))
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, WithGenesis(..))
import Language.Marlowe.Runtime.Indexer.ChainSeekClient (ChainEvent(..))
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock(..), MarloweTransaction(..))
import Observe.Event (EventBackend, addField, withEvent)

data StoreSelector f where
  Save :: StoreSelector SaveField

data SaveField
  = RollbackPoint ChainPoint
  | Stats ChangesStatistics
  | LocalTip ChainPoint
  | RemoteTip ChainPoint

data StoreDependencies r = StoreDependencies
  { databaseQueries :: DatabaseQueries IO
  , eventBackend :: EventBackend IO r StoreSelector
  , pullEvent :: STM ChainEvent
  }

store :: Component IO (StoreDependencies r) ()
store = proc StoreDependencies{..} -> do
  readChanges <- aggregator -< pullEvent
  persister -< PersisterDependencies{..}

aggregator :: Component IO (STM ChainEvent) (STM Changes)
aggregator = component \pullEvent -> do
  changesVar <- newTVar mempty
  let
    readChanges = do
      changes <- readTVar changesVar
      guard case changes of
        Changes Nothing [] _ _ _-> False
        _ -> True
      writeTVar changesVar mempty
      pure changes

    runAggregator = forever $ atomically do
      event <- pullEvent
      let
        changes = case event of
          RollForward block point tip ->
            mempty { blocks = [block], statistics = computeStats block, localTip = Just point, remoteTip = Just tip }
          RollBackward point tip ->
            mempty { rollbackTo = Just point, localTip = Just point, remoteTip = Just tip }
      modifyTVar changesVar (<> changes)
  pure (runAggregator, readChanges)

data PersisterDependencies r = PersisterDependencies
  { databaseQueries :: DatabaseQueries IO
  , eventBackend :: EventBackend IO r StoreSelector
  , readChanges :: STM Changes
  }

persister :: Component IO (PersisterDependencies r) ()
persister = component_ \PersisterDependencies{..} -> forever do
  Changes{..} <- atomically readChanges
  withEvent eventBackend Save \ev -> do
    traverse_ (addField ev . LocalTip) localTip
    traverse_ (addField ev . RemoteTip) remoteTip
    addField ev $ Stats statistics
    for_ rollbackTo \point -> do
      addField ev $ RollbackPoint point
      commitRollback databaseQueries point
    unless (null blocks) $ commitBlocks databaseQueries blocks

data Changes = Changes
  { rollbackTo :: Maybe ChainPoint
  , blocks :: [MarloweBlock]
  , statistics :: ChangesStatistics
  , localTip :: Maybe ChainPoint
  , remoteTip :: Maybe ChainPoint
  } deriving (Show, Eq, Generic)

instance Semigroup Changes where
  a <> b =
    let
      a' = maybe id applyRollback (rollbackTo b) a
    in
      Changes
        { rollbackTo = rollbackTo a'
        , blocks = on (<>) blocks a' b
        , statistics = on (<>) statistics a' b
        , localTip = getLast $ on (<>) (Last . localTip) a b
        , remoteTip = getLast $ on (<>) (Last . localTip) a b
        }

applyRollback :: ChainPoint -> Changes -> Changes
applyRollback Genesis _ = mempty { rollbackTo = Just Genesis }
applyRollback (At block) Changes{..} = if null blocksNotRolledBack
  then mempty { rollbackTo = Just (At block) }
  else
    mempty
      { blocks = blocksNotRolledBack
      , statistics = statistics `subStats` foldMap computeStats blocksRolledBack
      }
  where
    (blocksRolledBack, blocksNotRolledBack) = partition isRolledBack blocks
    isRolledBack MarloweBlock{..} = blockHeader > block

subStats :: ChangesStatistics -> ChangesStatistics -> ChangesStatistics
subStats a b = ChangesStatistics
  { blockCount = on (-) blockCount a b
  , createTxCount = on (-) createTxCount a b
  , applyInputsTxCount = on (-) applyInputsTxCount a b
  , withdrawTxCount = on (-) withdrawTxCount a b
  }

computeStats :: MarloweBlock -> ChangesStatistics
computeStats MarloweBlock{..} = (foldMap computeTxStats transactions) { blockCount = 1 }
  where
    computeTxStats CreateTransaction{} = mempty { createTxCount = 1 }
    computeTxStats ApplyInputsTransaction{} = mempty { applyInputsTxCount = 1 }
    computeTxStats WithdrawTransaction{} = mempty { withdrawTxCount = 1 }

instance Monoid Changes where
  mempty = Changes Nothing mempty mempty Nothing Nothing

data ChangesStatistics = ChangesStatistics
  { blockCount :: Int
  , createTxCount :: Int
  , applyInputsTxCount :: Int
  , withdrawTxCount :: Int
  } deriving (Show, Eq, Generic)

instance Semigroup ChangesStatistics where
  a <> b = ChangesStatistics
    { blockCount = on (+) blockCount a b
    , createTxCount = on (+) createTxCount a b
    , applyInputsTxCount = on (+) applyInputsTxCount a b
    , withdrawTxCount = on (+) withdrawTxCount a b
    }

instance Monoid ChangesStatistics where
  mempty = ChangesStatistics 0 0 0 0
