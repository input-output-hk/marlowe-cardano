{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Marlowe.Runtime.Web.Server.TxClient
  where

import Cardano.Api (BabbageEra, Tx, getTxId)
import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.Component
import Control.Concurrent.STM
  ( STM
  , TVar
  , atomically
  , modifyTVar
  , newEmptyTMVar
  , newTQueue
  , newTVar
  , putTMVar
  , readTQueue
  , readTVar
  , takeTMVar
  , writeTQueue
  )
import Control.Concurrent.STM.Delay (newDelay, waitDelay)
import Control.Exception (SomeException, try)
import Control.Monad (when, (<=<))
import Data.Foldable (for_)
import qualified Data.Map as Map
import Data.Time (UTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (Lovelace, StakeCredential, TransactionMetadata, TxId)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, Inputs, MarloweVersion, MarloweVersionTag)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , ContractCreated(..)
  , CreateError
  , InputsApplied(..)
  , MarloweTxCommand(..)
  , RoleTokensConfig
  , SubmitError
  , SubmitStatus(..)
  , WalletAddresses
  )
import Network.Protocol.Driver (RunClient)
import Network.Protocol.Job.Client
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (EventBackendModifier, modifyEventBackend, setAncestor)
import Observe.Event.DSL (SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))

compile $ SelectorSpec ["tx", "client"]
  [ ["submit", "tx"] ≔ ''Void
  , ["submit", "success"] ≔ ''Void
  , ["submit", "fail"] ≔ ''String
  , ["submit", "await"] ≔ ''String
  ]

data TxClientDependencies r = TxClientDependencies
  { runTxJobClient :: RunClient IO (JobClient MarloweTxCommand)
  , eventBackend :: EventBackend IO r TxClientSelector
  }

type CreateContract m
   = forall v
   . Maybe StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> TransactionMetadata
  -> Lovelace
  -> Contract v
  -> m (Either (CreateError v) (ContractCreated BabbageEra v))

type ApplyInputs m
   = forall v
   . MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> TransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs v
  -> m (Either (ApplyInputsError v) (InputsApplied BabbageEra v))

data TempTxStatus = Unsigned | Submitted

type Submit r m
   = [EventBackendModifier r]
  -> Tx BabbageEra
  -> m (Maybe SubmitError)

data TempTx (tx :: * -> MarloweVersionTag -> *) where
  TempTx :: MarloweVersion v -> TempTxStatus -> tx BabbageEra v -> TempTx tx

-- | Public API of the TxClient
data TxClient r = TxClient
  { createContract :: CreateContract IO
  , applyInputs :: ApplyInputs IO
  , submitContract :: ContractId -> Submit r IO
  , submitTransaction :: ContractId -> TxId -> Submit r IO
  , lookupTempContract :: ContractId -> STM (Maybe (TempTx ContractCreated))
  , getTempContracts :: STM [TempTx ContractCreated]
  , lookupTempTransaction :: ContractId -> TxId -> STM (Maybe (TempTx InputsApplied))
  , getTempTransactions :: ContractId -> STM [TempTx InputsApplied]
  }

-- Basically a lens to the actual map of temp txs to modify within a structure.
-- For example, tempTransactions is a (Map ContractId (Map TxId (TempTx InputsApplied))),
-- so to apply the given map modification to the inner map, we provide
-- \update -> Map.update (Just . update txId) contractId
type WithMapUpdate k tx a
   = (k -> Map.Map k (TempTx tx) -> Map.Map k (TempTx tx)) -- ^ Takes a function that, given a key in a map, modifies a map of temp txs
  -> (a -> a)                                              -- ^ And turns it into a function that modifies values of some type a

-- Pack a TVar of a's with a lens for modifying some inner map in the TVar and
-- existentialize the type parameters.
data SomeTVarWithMapUpdate = forall k tx a. Ord k => SomeTVarWithMapUpdate (TVar a) (WithMapUpdate k tx a)

txClient :: forall r. Component IO (TxClientDependencies r) (TxClient r)
txClient = component \TxClientDependencies{..} -> do
  tempContracts <- newTVar mempty
  tempTransactions <- newTVar mempty
  submitQueue <- newTQueue

  let
    runSubmitGeneric
      (SomeTVarWithMapUpdate tempVar updateTemp)
      tx
      sender
      mods = do
      let
        eb = modifyEventBackend mods eventBackend
        cmd = Submit tx
        client = JobClient $ pure $ SendMsgExec cmd $ clientCmd True
        clientCmd report = ClientStCmd
          { recvMsgFail = \err -> withEvent eb SubmitFail \ev -> do
              addField ev $ show err
              atomically do
                when report $ putTMVar sender $ Just err
                modifyTVar tempVar $ updateTemp $ Map.update (Just . toUnsigned)
          , recvMsgSucceed = \_ -> withEvent eb SubmitSuccess \_ -> do
              atomically do
                modifyTVar tempVar $ updateTemp Map.delete
                when report $ putTMVar sender Nothing
          , recvMsgAwait = \status _ -> withEvent eb SubmitAwait \ev -> do
              addField ev $ show status
              report' <- case status of
                Accepted -> do
                  when report $ atomically do
                    putTMVar sender Nothing
                    modifyTVar tempVar $ updateTemp $ Map.update (Just . toSubmitted)
                  pure False
                _ -> pure report
              delay <- newDelay 1_000_000
              atomically $ waitDelay delay
              pure $ SendMsgPoll $ clientCmd report'
          }
      runTxJobClient client

    genericSubmit
      :: SomeTVarWithMapUpdate
      -> [EventBackendModifier r]
      -> Tx BabbageEra
      -> IO (Maybe SubmitError)
    genericSubmit tVarWithUpdate mods tx =
      withEvent (modifyEventBackend mods eventBackend) SubmitTx \ev -> do
        sender <- atomically do
          sender <- newEmptyTMVar
          writeTQueue submitQueue (tVarWithUpdate, tx, sender, setAncestor (reference ev) <> mods)
          pure sender
        atomically $ takeTMVar sender

    runTxClient = do
      (tVarWithUpdate, tx, sender, mods) <- atomically $ readTQueue submitQueue
      concurrently_
        (try @SomeException $ runSubmitGeneric tVarWithUpdate tx sender mods)
        runTxClient

  pure (runTxClient, TxClient
    { createContract = \stakeCredential version addresses roles metadata minUTxODeposit contract -> do
        response <- runTxJobClient
          $ liftCommand
          $ Create stakeCredential version addresses roles metadata minUTxODeposit contract
        for_ response \creation@ContractCreated{contractId} -> atomically
          $ modifyTVar tempContracts
          $ Map.insert contractId
          $ TempTx version Unsigned creation
        pure response
    , applyInputs = \version addresses contractId metadata invalidBefore invalidHereafter inputs -> do
        response <- runTxJobClient
          $ liftCommand
          $ ApplyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs
        for_ response \application@InputsApplied{txBody} -> do
          let txId = fromCardanoTxId $ getTxId txBody
          let tempTx = TempTx version Unsigned application
          atomically
            $ modifyTVar tempTransactions
            $ Map.alter (Just . maybe (Map.singleton txId tempTx) (Map.insert txId tempTx)) contractId
        pure response
    , submitContract = \contractId -> genericSubmit $ SomeTVarWithMapUpdate tempContracts ($ contractId)
    , submitTransaction = \contractId txId -> genericSubmit $ SomeTVarWithMapUpdate tempTransactions \update -> Map.update (Just . update txId) contractId
    , lookupTempContract = \contractId -> Map.lookup contractId <$> readTVar tempContracts
    , getTempContracts = fmap snd . Map.toAscList <$> readTVar tempContracts
    , lookupTempTransaction = \contractId txId -> (Map.lookup txId <=< Map.lookup contractId) <$> readTVar tempTransactions
    , getTempTransactions = \contractId -> fmap snd . foldMap Map.toList . Map.lookup contractId <$> readTVar tempTransactions
    })

toSubmitted :: TempTx tx -> TempTx tx
toSubmitted (TempTx v _ tx) = TempTx v Submitted tx

toUnsigned :: TempTx tx -> TempTx tx
toUnsigned (TempTx v _ tx) = TempTx v Unsigned tx
