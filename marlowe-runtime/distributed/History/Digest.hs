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
import Control.Applicative (empty, (<|>))
import Control.Distributed.Process (Closure, Process, SendPort, receiveChan, say, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Distributed.Process.Extras.Time (TimeInterval)
import Control.Distributed.Process.Supervisor (ChildSpec (..), ChildStart (..), ChildStopPolicy (..), ChildType (..),
                                               RegisteredName (..), RestartPolicy (..))
import Control.Monad (guard, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS (RWST, ask, execRWST, get, modify)
import Data.Bifunctor (first, second)
import Data.Binary (Binary)
import Data.Data (Typeable)
import Data.Foldable (Foldable (..), for_, traverse_)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as Set
import GHC.Generics (Generic)
import History.Database (History (..), HistoryQueryChan, getContractIds, getHistory, getIntersectionPoint)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainPoint (..), MarloweSlotNo (..),
                                             MarloweTx (..), MarloweTxIn (..), MarloweTxOut (..), TxOutRef (..),
                                             slotToIntegral)
import Language.Marlowe.Runtime.History (creationToEvent, extractCreations, extractEvents)
import Language.Marlowe.Runtime.History.Types (AppTxOutRef (..), ContractCreationTxOut (..), Datum (..), Event (..),
                                               HistoryEvent (..))

worker :: String -> RestartPolicy -> Maybe TimeInterval -> ChildStopPolicy -> ChildStart -> ChildSpec
worker name restartPolicy restartDelay stopPolicy start =
  ChildSpec name Worker restartPolicy restartDelay stopPolicy start $ Just $ LocalName name

data HistoryDigestConfig = HistoryDigestConfig
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDigestConfig

data HistoryDigestDependencies = HistoryDigestDependencies
  { config         :: HistoryDigestConfig
  , sendEvents     :: SendPort [Event]
  , chainStoreChan :: SendPort ChainStoreQuery
  , dbChan         :: HistoryQueryChan
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryDigestDependencies

data State = State
  { utxoIndex        :: Map TxOutRef (Datum, ContractCreationTxOut)
  , paymentUtxoIndex :: Map TxOutRef ContractCreationTxOut
  , slotIndex        :: IntMap (Set TxOutRef)
  }

data CandidateTx = CandidateTx
  { isCreationCandidate :: Bool
  , isEventCandidate    :: Bool
  , tx                  :: MarloweTx
  }

historyDigest :: HistoryDigestDependencies -> Process ()
historyDigest HistoryDigestDependencies{..} = do
  intersectionPoint <- getIntersectionPoint dbChan
  contractIds <- getContractIds dbChan
  getNextBlock <- getBlocks chainStoreChan intersectionPoint

  let
    go state = go =<< ($ state) . either handleRollback handleRollForward =<< receiveChan getNextBlock

    emptyState = State { utxoIndex = mempty, paymentUtxoIndex = mempty, slotIndex = mempty }

    loadInitialState = do
      traverse_ initializeContractState contractIds

    initializeContractState contractId =
      lift (getHistory dbChan contractId) >>= traverse_ \History{..} ->
        for_ events \Event{historyEvent, blockHeader} -> case historyEvent of
          ContractWasCreated ContractCreationTxOut{datum, txOut} -> do
            let MarloweBlockHeader slot _ _ = blockHeader
            addUtxo slot creationTxOut $ AppTxOutRef (marloweTxOut_txOutRef txOut) datum
          InputsWereApplied (Just appTxOutRef) _ -> do
            let MarloweBlockHeader slot _ _ = blockHeader
            addUtxo slot creationTxOut appTxOutRef
          RoleWasPaidOut{..} -> do
            let MarloweBlockHeader slot _ _ = blockHeader
            addPaymentUtxo slot creationTxOut payoutTxOut
          _ -> pure ()


    handleRollForward Block{..} state = snd . fst <$> execRWST go' header (mempty, state)
      where
        go' = do
          let
            candidateTxs = extractCandidate =<< txs
            extractCandidateWith f candidate@CandidateTx{..} = tx <$ guard (f candidate)
            creationCandidates = mapMaybe (extractCandidateWith isCreationCandidate) candidateTxs
            eventCandidates = mapMaybe (extractCandidateWith isEventCandidate) candidateTxs
            creations = extractCreations (Testnet $ NetworkMagic 1566) header =<< creationCandidates
          creationEvents <- processCreations creations
          nonCreationEvents <- expand eventCandidates extractEvents'
          let events = creationEvents <> nonCreationEvents
          lift $ unless (null events) $ sendChan sendEvents events

    handleRollback MarloweChainPointAtGenesis _ = pure emptyState
    handleRollback (MarloweChainPoint rollbackSlot _) State{..} = do
      let afterRollback (slot, _) = slot > slotToIntegral rollbackSlot
      let (beforeSlotIndex, afterSlotIndex) = break afterRollback $ IntMap.toAscList slotIndex
      let afterSlotTxOuts = foldMap snd afterSlotIndex
      pure State
        { utxoIndex = Map.withoutKeys utxoIndex afterSlotTxOuts
        , paymentUtxoIndex = Map.withoutKeys paymentUtxoIndex afterSlotTxOuts
        , slotIndex = IntMap.fromDistinctAscList beforeSlotIndex
        }

    extractCandidate tx@MarloweTx{..} = CandidateTx{..} <$ guard (isEventCandidate || isCreationCandidate)
      where
        hasRedeemer (MarloweTxIn _ _ mRedeemer) = isJust mRedeemer
        isEventCandidate = any hasRedeemer marloweTx_inputs
        isCreationCandidate = not $ null marloweTx_policies

  initialState <- snd . fst <$> execRWST loadInitialState () ((), emptyState)

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
      header@(MarloweBlockHeader slot _ _) <- lift ask
      let txOutRef = TxOutRef txid txix
      guard $ Set.notMember txOutRef consumed
      utxo <- hoistMaybe $ (Left <$> Map.lookup txOutRef paymentUtxoIndex) <|> (Right <$> Map.lookup txOutRef utxoIndex)
      lift $ modify $ first $ Set.insert txOutRef
      case utxo of
        Left creationTx@ContractCreationTxOut{contractId, txOut, roleValidatorAddress} ->
          pure [Event contractId header marloweTx_id $ PayoutWasRedeemed txOutRef]
        Right (datum, creationTx@ContractCreationTxOut{contractId, txOut, roleValidatorAddress}) -> do
          let appTxOutRef = AppTxOutRef{..}
          case extractEvents roleValidatorAddress (marloweTxOut_address txOut) contractId appTxOutRef header tx of
            Left err -> do
              lift $ lift $ say $ "error: " <> err
              empty
            Right (mNewAppOut, events) -> do
              lift $ traverse_ (addUtxo slot creationTx) mNewAppOut
              for_ events \case
                Event{historyEvent = RoleWasPaidOut{..}} -> lift $ addPaymentUtxo slot creationTx payoutTxOut
                _                                        -> pure ()
              pure events

hoistMaybe :: Applicative f => Maybe a -> MaybeT f a
hoistMaybe = MaybeT . pure

processCreations
  :: [Either ([TxOutRef], String) ContractCreationTxOut]
  -> RWST MarloweBlockHeader () (Set TxOutRef, State) Process [Event]
processCreations = fmap fold . traverse \case
  Left err -> do
    lift $ say $ "found invalid creation transaction " <> show err
    pure []
  Right creation@ContractCreationTxOut{..} -> do
    MarloweBlockHeader slot _ _ <- ask
    addUtxo slot creation (AppTxOutRef (marloweTxOut_txOutRef txOut) datum)
    pure [creationToEvent creation]

addUtxo
  :: MarloweSlotNo
  -> ContractCreationTxOut
  -> AppTxOutRef
  -> RWST r () (s, State) Process ()
addUtxo slot creation@ContractCreationTxOut{contractId} AppTxOutRef{..} =
  modify $ second \State{..} -> State
    { utxoIndex = Map.insert txOutRef (datum, creation) utxoIndex
    , paymentUtxoIndex = paymentUtxoIndex
    , slotIndex = IntMap.insertWith (<>) (slotToIntegral slot) (Set.singleton txOutRef) slotIndex
    }

addPaymentUtxo
  :: MarloweSlotNo
  -> ContractCreationTxOut
  -> TxOutRef
  -> RWST r () (s, State) Process ()
addPaymentUtxo slot creation@ContractCreationTxOut{contractId} txOutRef =
  modify $ second \State{..} -> State
    { utxoIndex = utxoIndex
    , paymentUtxoIndex = Map.insert txOutRef creation paymentUtxoIndex
    , slotIndex = IntMap.insertWith (<>) (slotToIntegral slot) (Set.singleton txOutRef) slotIndex
    }

remotable ['historyDigest]

process :: HistoryDigestDependencies -> Closure (Process ())
process = $(mkClosure 'historyDigest)
