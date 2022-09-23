{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}


module Language.Marlowe.Runtime.Transaction.Server
  where

import Cardano.Api
  ( AddressInEra(..)
  , AddressTypeInEra(..)
  , BabbageEra
  , CardanoEra(BabbageEra)
  , NetworkId
  , PaymentCredential(..)
  , ShelleyBasedEra(..)
  , StakeAddressReference(..)
  , StakeCredential
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(..)
  , getTxBody
  , getTxId
  , makeShelleyAddress
  )
import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT, withExceptT)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoTxId, toCardanoPaymentCredential, toCardanoScriptHash)
import Language.Marlowe.Runtime.ChainSync.Api (Address, BlockHeader, Credential(..), SlotConfig, TokenName, TxId(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
    ( Contract,
      ContractId(..),
      MarloweVersion,
      PayoutDatum,
      Redeemer,
      Contract,
      ContractId(..),
      IsMarloweVersion(marloweVersion),
      MarloweVersion,
      PayoutDatum,
      Redeemer,
      withMarloweVersion )
import Language.Marlowe.Runtime.Core.ScriptRegistry (getCurrentScripts, marloweScript)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as Registry
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError(..)
  , CreateError(..)
  , JobId(..)
  , MarloweTxCommand(..)
  , SubmitError(..)
  , SubmitStatus(..)
  , WalletAddresses(..)
  , WithdrawError(..)
  )
import Language.Marlowe.Runtime.Transaction.BuildConstraints
  (buildApplyInputsConstraints, buildCreateConstraints, buildWithdrawConstraints)
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(..), SolveConstraints)
import Language.Marlowe.Runtime.Transaction.Query
  (LoadMarloweContext, LoadWalletContext, lookupMarloweScriptUtxo, lookupPayoutScriptUtxo)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob(..), SubmitJobStatus(..))
import Network.Protocol.Job.Server
  (JobServer(..), ServerStAttach(..), ServerStAwait(..), ServerStCmd(..), ServerStInit(..), hoistAttach, hoistCmd)
import System.IO (hPutStrLn, stderr)

newtype RunTransactionServer m = RunTransactionServer (forall a. JobServer MarloweTxCommand m a -> m a)

data TransactionServerDependencies = TransactionServerDependencies
  { acceptRunTransactionServer :: IO (RunTransactionServer IO)
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , solveConstraints :: SolveConstraints
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , slotConfig :: SlotConfig
  , networkId :: NetworkId
  }

newtype TransactionServer = TransactionServer
  { runTransactionServer :: IO ()
  }

mkTransactionServer :: TransactionServerDependencies -> STM TransactionServer
mkTransactionServer TransactionServerDependencies{..} = do
  submitJobsVar <- newTVar mempty
  let
    getSubmitJob txId = Map.lookup txId <$> readTVar submitJobsVar
    trackSubmitJob txId = modifyTVar submitJobsVar . Map.insert txId
    runTransactionServer = do
      runServer <- acceptRunTransactionServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runTransactionServer
  pure $ TransactionServer { runTransactionServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Job worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runServer :: RunTransactionServer IO
  , getSubmitJob :: TxId -> STM (Maybe SubmitJob)
  , trackSubmitJob :: TxId -> SubmitJob -> STM ()
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , solveConstraints :: SolveConstraints
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , slotConfig :: SlotConfig
  , networkId :: NetworkId
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunTransactionServer run = runServer
  in
    pure Worker { runWorker = run server }

  where
    server :: JobServer MarloweTxCommand IO ()
    server = JobServer $ pure serverInit

    serverInit :: ServerStInit MarloweTxCommand IO ()
    serverInit = ServerStInit
      { recvMsgExec = \case
          Create mStakeCredential version addresses roles metadata contract ->
            execCreate
              solveConstraints
              loadWalletContext
              networkId
              mStakeCredential
              version
              addresses
              roles
              metadata
              contract
          ApplyInputs era version addresses contractId invalidBefore invalidHereafter redeemer ->
            withMarloweVersion version $ execApplyInputs
              slotConfig
              solveConstraints
              loadWalletContext
              loadMarloweContext
              era
              addresses
              contractId
              invalidBefore
              invalidHereafter
              redeemer
          Withdraw version addresses contractId payoutDatum ->
            execWithdraw
              solveConstraints
              loadWalletContext
              loadMarloweContext
              version
              addresses
              contractId
              payoutDatum
          Submit tx ->
            execSubmit mkSubmitJob trackSubmitJob tx
      , recvMsgAttach = \case
          jobId@(JobIdSubmit txId) ->
            attachSubmit jobId $ getSubmitJob txId
      }

attachSubmit
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe SubmitJob)
  -> IO (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
attachSubmit jobId getSubmitJob =
  atomically $ fmap (hoistAttach atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: SolveConstraints
  -> LoadWalletContext
  -> NetworkId
  -> Maybe StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> Map TokenName Address
  -> Map Int Aeson.Value
  -> Contract v
  -> IO (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractId, TxBody BabbageEra) IO ())
execCreate solveConstraints loadWalletContext networkId mStakeCredential version addresses roleTokens metadata contract = execExceptT do
  constraints <- except $ buildCreateConstraints version roleTokens metadata contract
  walletContext <- lift $ loadWalletContext addresses
  let
    scripts@Registry.MarloweScripts{..} = Registry.getCurrentScripts version
    stakeReference = maybe NoStakeAddress StakeAddressByValue mStakeCredential
    marloweAddress = fromCardanoAddressInEra BabbageEra
      $ AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage)
      $ makeShelleyAddress
          networkId
          (fromJust $ toCardanoPaymentCredential $ ScriptCredential marloweScript)
          stakeReference
    payoutAddress = fromCardanoAddressInEra BabbageEra
      $ AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage)
      $ makeShelleyAddress
          networkId
          (fromJust $ toCardanoPaymentCredential $ ScriptCredential payoutScript)
          NoStakeAddress
  marloweContext <- except $ first CreateLoadMarloweContextFailed do
    marloweScriptUTxO <- lookupMarloweScriptUtxo networkId scripts
    payoutScriptUTxO <- lookupPayoutScriptUtxo networkId scripts
    pure MarloweContext
      { scriptOutput = Nothing
      , payoutOutputs = mempty
      , marloweAddress
      , payoutAddress
      , marloweScriptUTxO
      , payoutScriptUTxO
      , marloweScriptHash = marloweScript
      , payoutScriptHash = payoutScript
      }
  txBody <- except
    $ first CreateConstraintError
    $ solveConstraints version marloweContext walletContext constraints
  pure (ContractId $ findMarloweOutput txBody, txBody)
  where
  findMarloweOutput = \case
    body@(TxBody TxBodyContent{..}) -> Chain.TxOutRef (fromCardanoTxId $ getTxId body)
      $ fst
      $ fromJust
      $ find (isToCurrentScriptAddress . snd)
      $ zip [0..] txOuts
    where
      scriptHash = fromJust
        $ toCardanoScriptHash
        $ marloweScript
        $ getCurrentScripts version
      scriptAddress = makeShelleyAddress networkId (PaymentCredentialByScript scriptHash)
        $ maybe NoStakeAddress StakeAddressByValue mStakeCredential
      isToCurrentScriptAddress (TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) address) _ _ _) = address == scriptAddress
      isToCurrentScriptAddress _ = False

execApplyInputs
  :: IsMarloweVersion v
  => SlotConfig
  -> SolveConstraints
  -> LoadWalletContext
  -> LoadMarloweContext
  -> ScriptDataSupportedInEra era
  -> WalletAddresses
  -> ContractId
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Redeemer v
  -> IO (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (TxBody BabbageEra) IO ())
execApplyInputs
  slotConfig
  solveConstraints
  loadWalletContext
  loadMarloweContext
  era
  addresses
  contractId
  invalidBefore
  invalidHereafter
  inputs = execExceptT do
    marloweContext@MarloweContext{..} <- withExceptT ApplyInputsLoadMarloweContextFailed
      $ ExceptT
      $ loadMarloweContext marloweVersion contractId
    scriptOutput' <- except $ maybe (Left ScriptOutputNotFound) Right scriptOutput
    constraints <- except $ buildApplyInputsConstraints
        slotConfig
        marloweVersion
        scriptOutput'
        invalidBefore
        invalidHereafter
        inputs
    walletContext <- lift $ loadWalletContext addresses
    except
      $ first ApplyInputsConstraintError
      $ solveConstraints version marloweContext walletContext constraints

execWithdraw
  :: SolveConstraints
  -> LoadWalletContext
  -> LoadMarloweContext
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> PayoutDatum v
  -> IO (ServerStCmd MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra) IO ())
execWithdraw solveConstraints loadWalletContext loadMarloweContext version addresses contractId datum = execExceptT do
  constraints <- except $ buildWithdrawConstraints version datum
  walletContext <- lift $ loadWalletContext addresses
  marloweContext <- withExceptT WithdrawLoadMarloweContextFailed
    $ ExceptT
    $ loadMarloweContext version contractId
  except
    $ first WithdrawConstraintError
    $ solveConstraints version marloweContext walletContext constraints

execSubmit
  :: (Tx BabbageEra -> STM SubmitJob)
  -> (TxId -> SubmitJob -> STM ())
  -> Tx BabbageEra
  -> IO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
execSubmit mkSubmitJob trackSubmitJob tx = do
  let txId = fromCardanoTxId $ getTxId $ getTxBody tx
  (submitJob, exVar) <- atomically do
    exVar <- newEmptyTMVar
    submitJob <- mkSubmitJob tx
    let getExceptionStatus = Failed SubmitException <$ readTMVar exVar
    let submitJob' = submitJob { submitJobStatus = getExceptionStatus <|> submitJobStatus submitJob }
    trackSubmitJob txId submitJob'
    pure (submitJob', exVar)
  -- Run the job in a new thread
  _ <- forkFinally (runSubmitJob submitJob) \case
    Left ex -> atomically $ putTMVar exVar ex
    _ -> pure ()
  -- Make a new server and run it in IO.
  hoistCmd atomically <$> atomically (submitJobServerCmd (JobIdSubmit txId) submitJob)

submitJobServerAttach
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> Maybe SubmitJob
  -> STM (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
submitJobServerAttach jobId = maybe
  (pure $ SendMsgAttachFailed ())
  (fmap SendMsgAttached . submitJobServerCmd jobId)

submitJobServerCmd
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> SubmitJob
  -> STM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
submitJobServerCmd jobId submitJob = do
  jobStatus <- submitJobStatus submitJob
  pure case jobStatus of
    Running status -> SendMsgAwait status jobId ServerStAwait
      { recvMsgDetach = pure ()
      , recvMsgPoll = submitJobServerCmd jobId submitJob
      }
    Succeeded block -> SendMsgSucceed block ()
    Failed err -> SendMsgFail err ()

execExceptT
  :: Functor m
  => ExceptT e m a
  -> m (ServerStCmd cmd status e a m ())
execExceptT = fmap (either (flip SendMsgFail ()) (flip SendMsgSucceed ())) . runExceptT
