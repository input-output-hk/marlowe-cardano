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
import Data.Aeson (ToJSON)
import Data.Foldable (for_, traverse_)
import Data.Function (on)
import Data.List (partition)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Semigroup (Last(..))
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, TxId, WithGenesis(..))
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.History.Api (ExtractCreationError, ExtractMarloweTransactionError)
import Language.Marlowe.Runtime.Indexer.ChainSeekClient (ChainEvent(..))
import Language.Marlowe.Runtime.Indexer.Database (DatabaseQueries(..))
import Language.Marlowe.Runtime.Indexer.Types (MarloweBlock(..), MarloweTransaction(..))
import Observe.Event.Explicit (EventBackend, addField, withEvent)

data StoreSelector f where
  Save :: StoreSelector SaveField

data SaveField
  = RollbackPoint ChainPoint
  | Stats ChangesStatistics
  | LocalTip ChainPoint
  | RemoteTip ChainPoint
  | InvalidCreateTxs (Map ContractId ExtractCreationError)
  | InvalidApplyInputsTxs (Map TxId ExtractMarloweTransactionError)

data StoreDependencies r = StoreDependencies
  { databaseQueries :: DatabaseQueries IO
  , eventBackend :: EventBackend IO r StoreSelector
  , pullEvent :: STM ChainEvent
  }

-- | The store component aggregates changes into batches in one thread, and
-- pulls batches to save in another.
store :: Component IO (StoreDependencies r) ()
store = proc StoreDependencies{..} -> do
  -- Spawn the aggregator thread
  readChanges <- aggregator -< pullEvent

  -- Spawn the persister thread to read the changes accumulated by the aggregator.
  persister -< PersisterDependencies{..}

-- | The aggregator component pulls chain events and accumulates a batch of
-- changes to persist.
aggregator :: Component IO (STM ChainEvent) (STM Changes)
aggregator = component \pullEvent -> do
  -- A variable which will hold the accumulated changes.
  changesVar <- newTVar mempty

  let
    -- An action to read and empty the changes.
    readChanges = do
      -- Read the current changes
      changes <- readTVar changesVar

      -- Retry the STM transaction if the changes are empty
      guard case changes of
        Changes Nothing [] _ _ _ invalidCreateTxs invalidApplyInputsTxs ->
          not $ Map.null invalidCreateTxs && Map.null invalidApplyInputsTxs
        _ -> True

      -- Empty the changes variable
      writeTVar changesVar mempty

      -- Return the changes
      pure changes

    -- The IO action loop to run in this component's thread.
    runAggregator = forever do
      -- Pull an event from the queue.
      event <- atomically pullEvent

      let
        -- Compute the changes for the pulled event.
        changes = case event of
          RollForward block point tip ->
            mempty
              { blocks = [block]
              , statistics = computeStats block
              , localTip = Just point
              , remoteTip = Just tip
              , invalidCreateTxs = flip foldMap (transactions block) \case
                  InvalidCreateTransaction contractId err -> Map.singleton contractId err
                  _ -> mempty
              , invalidApplyInputsTxs = flip foldMap (transactions block) \case
                  InvalidApplyInputsTransaction txId _ err -> Map.singleton txId err
                  _ -> mempty
              }
          RollBackward point tip ->
            mempty { rollbackTo = Just point, localTip = Just point, remoteTip = Just tip }

      -- Append the new changes to the existing changes.
      atomically $ modifyTVar changesVar (<> changes)
  pure (runAggregator, readChanges)

data PersisterDependencies r = PersisterDependencies
  { databaseQueries :: DatabaseQueries IO
  , eventBackend :: EventBackend IO r StoreSelector
  , readChanges :: STM Changes
  }

-- | A component to save batches of changes to the database.
persister :: Component IO (PersisterDependencies r) ()
persister = component_ \PersisterDependencies{..} -> forever do
  -- Read the next batch of changes.
  Changes{..} <- atomically readChanges

  -- Log a save event.
  withEvent eventBackend Save \ev -> do
    traverse_ (addField ev . LocalTip) localTip
    traverse_ (addField ev . RemoteTip) remoteTip
    addField ev $ Stats statistics

    -- If there is a rollback, save it first.
    for_ rollbackTo \point -> do
      addField ev $ RollbackPoint point
      commitRollback databaseQueries point

    unless (Map.null invalidCreateTxs) $ addField ev $ InvalidCreateTxs invalidCreateTxs
    unless (Map.null invalidApplyInputsTxs) $ addField ev $ InvalidApplyInputsTxs invalidApplyInputsTxs

    -- If there are blocks to save, save them.
    unless (null blocks) $ commitBlocks databaseQueries blocks

data Changes = Changes
  { rollbackTo :: Maybe ChainPoint
  , blocks :: [MarloweBlock]
  , statistics :: ChangesStatistics
  , localTip :: Maybe ChainPoint
  , remoteTip :: Maybe ChainPoint
  , invalidCreateTxs :: Map ContractId ExtractCreationError
  , invalidApplyInputsTxs :: Map TxId ExtractMarloweTransactionError
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
        , remoteTip = getLast $ on (<>) (Last . remoteTip) a b
        , invalidCreateTxs = on (<>) invalidCreateTxs a b
        , invalidApplyInputsTxs = on (<>) invalidApplyInputsTxs a b
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
    computeTxStats _ = mempty

instance Monoid Changes where
  mempty = Changes Nothing mempty mempty Nothing Nothing mempty mempty

data ChangesStatistics = ChangesStatistics
  { blockCount :: Int
  , createTxCount :: Int
  , applyInputsTxCount :: Int
  , withdrawTxCount :: Int
  } deriving (Show, Eq, Generic)

instance ToJSON ChangesStatistics

instance Semigroup ChangesStatistics where
  a <> b = ChangesStatistics
    { blockCount = on (+) blockCount a b
    , createTxCount = on (+) createTxCount a b
    , applyInputsTxCount = on (+) applyInputsTxCount a b
    , withdrawTxCount = on (+) withdrawTxCount a b
    }

instance Monoid ChangesStatistics where
  mempty = ChangesStatistics 0 0 0 0
