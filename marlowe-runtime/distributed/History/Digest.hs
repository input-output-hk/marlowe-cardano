{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# LANGUAGE NamedFieldPuns             #-}
module History.Digest where

import Cardano.Api (NetworkId (Testnet))
import Cardano.Api.Byron (NetworkMagic (NetworkMagic))
import ChainSync.Database (Block (..))
import ChainSync.Store (ChainStoreQuery, getBlocks)
import Control.Applicative (empty)
import Control.Distributed.Process (Closure, Process, RemoteTable, SendPort, receiveChan, say, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras.Time (TimeInterval)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..))
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS (RWST, ask, execRWST, get, modify)
import Data.Bifunctor (first, second)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (Foldable (..), traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainPoint (..), MarloweTx (..),
                                             MarloweTxIn (..), MarloweTxOut (..), TxOutRef (..), slotToIntegral)
import Language.Marlowe.Runtime.History (creationToEvent, extractCreations, extractEvents)
import Language.Marlowe.Runtime.History.Types (AppTxOutRef (..), ContractCreationTxOut (..), Datum (..), Event)

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data HistoryDigestConfig = HistoryDigestConfig
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDigestConfig

data HistoryDigestDependencies = HistoryDigestDependencies
  { config         :: HistoryDigestConfig
  , sendEvent      :: SendPort Event
  , chainStoreChan :: SendPort ChainStoreQuery
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDigestDependencies

data State = State
  { utxoIndex :: Map TxOutRef (Datum, ContractCreationTxOut)
  , slotIndex :: IntMap (Set TxOutRef)
  }

data CandidateTx = CandidateTx
  { isCreationCandidate :: Bool
  , isEventCandidate    :: Bool
  , tx                  :: MarloweTx
  }

historyDigest :: HistoryDigestDependencies -> Process ()
historyDigest HistoryDigestDependencies{..} = do
  say "starting"
  getNextBlock <- getBlocks chainStoreChan MarloweChainPointAtGenesis
  let
    go state = go =<< ($ state) . either handleRollback handleRollForward =<< receiveChan getNextBlock

    initialState = State { utxoIndex = mempty, slotIndex = mempty }

    handleRollForward Block{..} state = snd . fst <$> execRWST go' header (mempty, state)
      where
        go' = do
          let
            candidateTxs = extractCandidate =<< txs
            extractCandidateWith f candidate@CandidateTx{..} = tx <$ guard (f candidate)
            creationCandidates = mapMaybe (extractCandidateWith isCreationCandidate) candidateTxs
            eventCandidates = mapMaybe (extractCandidateWith isEventCandidate) candidateTxs
            creations = extractCreations (Testnet $ NetworkMagic 1566) header =<< creationCandidates
          processCreations sendEvent creations
          events <- expand eventCandidates extractEvents'
          lift $ traverse_ (sendChan sendEvent) events

    handleRollback MarloweChainPointAtGenesis _ = pure initialState
    handleRollback (MarloweChainPoint rollbackSlot _) State{..} = do
      let afterRollback (slot, _) = slot > slotToIntegral rollbackSlot
      let (beforeSlotIndex, afterSlotIndex) = break afterRollback $ IntMap.toAscList slotIndex
      let afterSlotTxOuts = foldMap snd afterSlotIndex
      pure State
        { utxoIndex = Map.withoutKeys utxoIndex afterSlotTxOuts
        , slotIndex = IntMap.fromDistinctAscList beforeSlotIndex
        }

    extractCandidate tx@MarloweTx{..} = CandidateTx{..} <$ guard (isEventCandidate || isCreationCandidate)
      where
        hasRedeemer (MarloweTxIn _ _ mRedeemer) = isJust mRedeemer
        isEventCandidate = any hasRedeemer marloweTx_inputs
        isCreationCandidate = not $ null marloweTx_policies

  go initialState

expand :: Monad m => [a] -> (a -> m [b]) -> m [b]
expand a extract = do
  bs <- concat <$> traverse extract a
  case bs of
    [] -> pure bs
    _  -> (bs <>) <$> expand a extract

extractEvents' :: MarloweTx -> RWST MarloweBlockHeader () (Set TxOutRef, State) Process [Event]
extractEvents' tx@MarloweTx{..} = concat <$> traverse go marloweTx_inputs
  where
    go (MarloweTxIn txid txix _) = fmap (concat . maybeToList) $ runMaybeT do
      (consumed, State{..}) <- lift get
      header <- lift ask
      let txOutRef = TxOutRef txid txix
      guard $ Set.notMember txOutRef consumed
      (datum, creationTx@ContractCreationTxOut{contractId, txOut}) <- hoistMaybe $ Map.lookup txOutRef utxoIndex
      lift $ modify $ first $ Set.insert txOutRef
      let appTxOutRef = AppTxOutRef{..}
      case extractEvents (marloweTxOut_address txOut) contractId appTxOutRef header tx of
        Left err -> do
          lift $ lift $ say $ "error: " <> err
          empty
        Right (mNewAppOut, events) -> do
          lift $ traverse_ (addUtxo creationTx) mNewAppOut
          pure events

hoistMaybe :: Applicative f => Maybe a -> MaybeT f a
hoistMaybe = MaybeT . pure

processCreations
  :: SendPort Event
  -> [Either ([TxOutRef], String) ContractCreationTxOut]
  -> RWST MarloweBlockHeader () (Set TxOutRef, State) Process ()
processCreations sendEvent = fmap fold . traverse \case
  Left err -> do
    lift $ say $ "found invalid creation transaction " <> show err
    pure ()
  Right creation@ContractCreationTxOut{..} -> do
    lift $ sendChan sendEvent $ creationToEvent creation
    addUtxo creation (AppTxOutRef (marloweTxOut_txOutRef txOut) datum)

addUtxo
  :: ContractCreationTxOut
  -> AppTxOutRef
  -> RWST MarloweBlockHeader () (Set TxOutRef, State) Process ()
addUtxo creation@ContractCreationTxOut{contractId} AppTxOutRef{..} = do
  MarloweBlockHeader slot _ _ <- ask
  modify $ second \State{..} -> State
    { utxoIndex = Map.insert txOutRef (datum, creation) utxoIndex
    , slotIndex = IntMap.insertWith (<>) (slotToIntegral slot) (Set.singleton txOutRef) slotIndex
    }

remotable ['historyDigest]

remoteTable :: RemoteTable -> RemoteTable
remoteTable = __remoteTable

process :: HistoryDigestDependencies -> Closure (Process ())
process = $(mkClosure 'historyDigest)
