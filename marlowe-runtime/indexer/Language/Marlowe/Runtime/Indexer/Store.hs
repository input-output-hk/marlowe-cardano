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
import Observe.Event (EventBackend, addField, withEvent)

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
        Changes Nothing [] _ _ _ invalidCreateTxs invalidApplyInputsTxs ->
          not $ Map.null invalidCreateTxs && Map.null invalidApplyInputsTxs
        _ -> True
      writeTVar changesVar mempty
      pure changes

    runAggregator = forever $ atomically do
      event <- pullEvent
      let
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
                  InvalidApplyInputsTransaction txId err -> Map.singleton txId err
                  _ -> mempty
              }
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
    unless (Map.null invalidCreateTxs) $ addField ev $ InvalidCreateTxs invalidCreateTxs
    unless (Map.null invalidApplyInputsTxs) $ addField ev $ InvalidApplyInputsTxs invalidApplyInputsTxs
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
