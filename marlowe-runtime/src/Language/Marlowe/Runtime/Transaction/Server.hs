{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}


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
  , ToJSON
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(..)
  , getTxBody
  , getTxId
  , makeShelleyAddress
  )
import Colog (logDebug, logError)
import Control.Applicative ((<|>))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar)
import Control.Error.Util (failWithM, hoistMaybe, noteT)
import Control.Exception (SomeException)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT, withExceptT)
import qualified Data.Aeson as A
import qualified Data.Aeson.OneLine as O
import Data.Aeson.Types (toJSON)
import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoTxId, toCardanoPaymentCredential, toCardanoScriptHash)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader, Credential(..), PolicyId, SlotConfig, TokenName, TransactionMetadata, TxId(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (Contract, ContractId(..), MarloweVersion(MarloweV1), Payout(Payout, datum), Redeemer)
import Language.Marlowe.Runtime.Core.ScriptRegistry (getCurrentScripts, marloweScript)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as Registry
import Language.Marlowe.Runtime.Logging.Colog.LogIO
  (ConcurrentlyLogIO(ConcurrentlyLogIO, runConcurrentlyLogIO), LogIO, LogIOAction, catchLogIO, forkFinallyLogIO)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError(..)
  , CreateError(..)
  , JobId(..)
  , MarloweTxCommand(..)
  , Mint
  , SubmitError(..)
  , SubmitStatus(..)
  , WalletAddresses(..)
  , WithdrawError(..)
  , marloweTxCommandSchema
  )
import Language.Marlowe.Runtime.Transaction.BuildConstraints
  (buildApplyInputsConstraints, buildCreateConstraints, buildWithdrawConstraints)
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(..), SolveConstraints)
import Language.Marlowe.Runtime.Transaction.Query
  (LoadMarloweContext, LoadWalletContext, lookupMarloweScriptUtxo, lookupPayoutScriptUtxo)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob(..), SubmitJobStatus(..))
import Network.Protocol.Job.Server
  ( JobServer(..)
  , ServerStAttach(..)
  , ServerStAwait(..)
  , ServerStCmd(..)
  , ServerStIdle(..)
  , hoistAttach
  , hoistCmd
  , jobServer
  )

newtype RunTransactionServer m = RunTransactionServer (forall a. JobServer MarloweTxCommand m a -> m a)

data TransactionServerDependencies = TransactionServerDependencies
  { acceptRunTransactionServer :: LogIO (RunTransactionServer LogIO)
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , solveConstraints :: SolveConstraints
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , logAction :: LogIOAction
  , slotConfig :: SlotConfig
  , networkId :: NetworkId
  }

newtype TransactionServer = TransactionServer
  { runTransactionServer :: LogIO ()
  }

mkTransactionServer :: TransactionServerDependencies -> STM TransactionServer
mkTransactionServer TransactionServerDependencies{..} = do
  submitJobsVar <- newTVar mempty
  let
    getSubmitJob txId = Map.lookup txId <$> readTVar submitJobsVar
    trackSubmitJob txId = modifyTVar submitJobsVar . Map.insert txId
    runTransactionServer = do
      runServer <- acceptRunTransactionServer
      Worker{..} <- liftIO $ atomically $ mkWorker WorkerDependencies {..}
      runConcurrentlyLogIO $
        ConcurrentlyLogIO (runWorker `catchLogIO` catchWorker) *> ConcurrentlyLogIO runTransactionServer
  pure $ TransactionServer { runTransactionServer }

catchWorker :: SomeException -> LogIO ()
catchWorker ex = logError . T.pack $ "Job worker crash:" <> show ex

data WorkerDependencies = WorkerDependencies
  { runServer :: RunTransactionServer LogIO
  , getSubmitJob :: TxId -> STM (Maybe SubmitJob)
  , trackSubmitJob :: TxId -> SubmitJob -> STM ()
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , solveConstraints :: SolveConstraints
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , logAction :: LogIOAction
  , slotConfig :: SlotConfig
  , networkId :: NetworkId
  }

newtype Worker = Worker
  { runWorker :: LogIO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunTransactionServer run = runServer
  in
    pure Worker { runWorker = run server }

  where
    server :: JobServer MarloweTxCommand LogIO ()
    server = jobServer marloweTxCommandSchema $ do
      Colog.logDebug "Handshake succeeded. Starting Marlowe Tx Job server"
      pure serverIdle

    serverIdle :: ServerStIdle MarloweTxCommand LogIO ()
    serverIdle = ServerStIdle
      { recvMsgExec = \case
          cmd@(Create mStakeCredential version addresses roles metadata minAda contract) -> do
            Colog.logDebug $ "Performing create:" <> T.pack (show cmd)
            case version of
              MarloweV1 -> execCreate
                solveConstraints
                loadWalletContext
                networkId
                mStakeCredential
                version
                addresses
                roles
                metadata
                minAda
                contract
          cmd@(ApplyInputs version addresses contractId invalidBefore invalidHereafter redeemer) -> do
            Colog.logDebug $ "Applying inputs:" <> T.pack (show cmd)
            case version of
              MarloweV1-> execApplyInputs
                slotConfig
                solveConstraints
                loadWalletContext
                loadMarloweContext
                version
                addresses
                contractId
                invalidBefore
                invalidHereafter
                redeemer
          Withdraw version addresses contractId roleToken ->
            execWithdraw
              solveConstraints
              loadWalletContext
              loadMarloweContext
              version
              addresses
              contractId
              roleToken
          Submit tx ->
            execSubmit mkSubmitJob trackSubmitJob tx
      , recvMsgAttach = \case
          jobId@(JobIdSubmit txId) ->
            attachSubmit jobId $ getSubmitJob txId
      , recvMsgDone = pure ()
      }

attachSubmit
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe SubmitJob)
  -> LogIO (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader LogIO ())
attachSubmit jobId getSubmitJob =
  liftIO $ atomically $ fmap (hoistAttach $ liftIO . atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: HasCallStack
  => ToJSON (MarloweContext v)
  => SolveConstraints
  -> LoadWalletContext
  -> NetworkId
  -> Maybe StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> Maybe (Either PolicyId Mint)
  -> TransactionMetadata
  -> Chain.Lovelace
  -> Contract v
  -> LogIO (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractId, TxBody BabbageEra) LogIO ())
execCreate solveConstraints loadWalletContext networkId mStakeCredential version addresses roleTokens metadata minAda contract = execExceptT do
  walletContext <-
    failWithM (CreateExecutionError "Failed to load wallet context") $ loadWalletContext addresses
  constraints <- except $ buildCreateConstraints version walletContext roleTokens metadata minAda contract
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
  lift $ logDebug . mappend "Initial MarloweContext:" . O.renderValue . toJSON $ marloweContext
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
  :: ToJSON (MarloweContext v)
  => SlotConfig
  -> SolveConstraints
  -> LoadWalletContext
  -> LoadMarloweContext
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Redeemer v
  -> LogIO (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (TxBody BabbageEra) LogIO ())
execApplyInputs
  slotConfig
  solveConstraints
  loadWalletContext
  loadMarloweContext
  version
  addresses
  contractId
  invalidBefore
  invalidHereafter
  inputs = execExceptT do
    lift . Colog.logDebug $ "Loading marlowe context: " <> T.pack (show contractId)
    marloweContext@MarloweContext{..} <- withExceptT ApplyInputsLoadMarloweContextFailed
      $ ExceptT
      $ loadMarloweContext version contractId
    lift $ Colog.logDebug . O.renderValue . A.toJSON $ marloweContext

    invalidBefore' <- liftIO $ maybe getCurrentTime pure invalidBefore
    scriptOutput' <- except $ maybe (Left ScriptOutputNotFound) Right scriptOutput
    constraints <- except $ buildApplyInputsConstraints
        slotConfig
        version
        scriptOutput'
        invalidBefore'
        invalidHereafter
        inputs
    walletContext <- failWithM (ApplyExecutionError "Failed to load wallet context") $ loadWalletContext addresses
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
  -> TokenName
  -> LogIO (ServerStCmd MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra) LogIO ())
execWithdraw solveConstraints loadWalletContext loadMarloweContext version addresses contractId roleToken = execExceptT $ case version of
  MarloweV1 -> do
    marloweContext@MarloweContext{payoutOutputs=Map.elems -> payouts} <- withExceptT WithdrawLoadMarloweContextFailed
      $ ExceptT
      $ loadMarloweContext version contractId
    let
      payoutAssetId Payout {datum = assetId } = assetId
      isRolePayout (Chain.AssetId _ roleName) = roleName == roleToken
      possibleDatum = find isRolePayout . map payoutAssetId $ payouts
    datum <- noteT (UnableToFindPayoutForAGivenRole roleToken) $ hoistMaybe possibleDatum
    constraints <- except $ buildWithdrawConstraints version datum
    walletContext <- failWithM  (WithdrawExecutionError "Failed to load wallet context") $ loadWalletContext addresses
    except
      $ first WithdrawConstraintError
      $ solveConstraints version marloweContext walletContext constraints

execSubmit
  :: HasCallStack
  => (Tx BabbageEra -> STM SubmitJob)
  -> (TxId -> SubmitJob -> STM ())
  -> Tx BabbageEra
  -> LogIO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader LogIO ())
execSubmit mkSubmitJob trackSubmitJob tx = do
  let txId = fromCardanoTxId $ getTxId $ getTxBody tx
  Colog.logDebug $ "Received transaction: " <> T.pack (show txId)
  (submitJob, exVar) <- liftIO $ atomically do
    exVar <- newEmptyTMVar
    submitJob <- mkSubmitJob tx
    let getExceptionStatus = Failed . SubmitException . show <$> readTMVar exVar
    let submitJob' = submitJob { submitJobStatus = getExceptionStatus <|> submitJobStatus submitJob }
    trackSubmitJob txId submitJob'
    pure (submitJob', exVar)
  -- Run the job in a new thread
  _ <- forkFinallyLogIO (runSubmitJob submitJob) \case
    Left ex -> do
      liftIO $ atomically $ putTMVar exVar ex
    _ -> pure ()
  -- Make a new server and run it in IO.
  hoistCmd (liftIO . atomically) <$> liftIO (atomically (submitJobServerCmd (JobIdSubmit txId) submitJob))

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
  -> m (ServerStCmd cmd status e a n ())
execExceptT = fmap (either (flip SendMsgFail ()) (flip SendMsgSucceed ())) . runExceptT

