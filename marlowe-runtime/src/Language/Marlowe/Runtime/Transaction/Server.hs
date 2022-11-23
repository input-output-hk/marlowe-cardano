{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.Transaction.Server
  where

import Cardano.Api
  ( AddressInEra(..)
  , AddressTypeInEra(..)
  , BabbageEra
  , CardanoEra(BabbageEra)
  , CardanoMode
  , EraHistory
  , NetworkId(..)
  , PaymentCredential(..)
  , ShelleyBasedEra(..)
  , StakeAddressReference(..)
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(..)
  , getTxBody
  , getTxId
  , makeShelleyAddress
  )
import qualified Colog
import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Async (Concurrently(..))
import Control.Concurrent.STM (STM, atomically, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar)
import Control.Error.Util (hoistMaybe, note, noteT)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Trans.Reader (ReaderT(..))
import qualified Data.Aeson as A
import qualified Data.Aeson.OneLine as O
import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time (UTCTime, getCurrentTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoTxId, toCardanoPaymentCredential, toCardanoScriptHash, toCardanoStakeCredential)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader, ChainSyncQuery(..), Credential(..), TokenName, TransactionMetadata, TxId(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  (Contract, ContractId(..), MarloweVersion(MarloweV1), Payout(Payout, datum), Redeemer, withMarloweVersion)
import Language.Marlowe.Runtime.Core.ScriptRegistry (getCurrentScripts, marloweScript)
import qualified Language.Marlowe.Runtime.Core.ScriptRegistry as Registry
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError(..)
  , ContractCreated(..)
  , CreateError(..)
  , JobId(..)
  , MarloweTxCommand(..)
  , RoleTokensConfig
  , SubmitError(..)
  , SubmitStatus(..)
  , WalletAddresses(..)
  , WithdrawError(..)
  )
import Language.Marlowe.Runtime.Transaction.BuildConstraints
  (buildApplyInputsConstraints, buildCreateConstraints, buildWithdrawConstraints)
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(..), SolveConstraints)
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Language.Marlowe.Runtime.Transaction.Query
  (LoadMarloweContext, LoadWalletContext, lookupMarloweScriptUtxo, lookupPayoutScriptUtxo)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob(..), SubmitJobStatus(..))
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Job.Server
  (JobServer(..), ServerStAttach(..), ServerStAwait(..), ServerStCmd(..), ServerStInit(..), hoistAttach, hoistCmd)
import Ouroboros.Consensus.BlockchainTime (SystemStart)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import Unsafe.Coerce (unsafeCoerce)

type RunTransactionServer m = RunServer m (JobServer MarloweTxCommand)

data TransactionServerDependencies = TransactionServerDependencies
  { acceptRunTransactionServer :: WorkerM (RunTransactionServer WorkerM)
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , logAction :: AppLogAction
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
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
      runServer <- Colog.usingLoggerT logAction acceptRunTransactionServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (Colog.usingLoggerT logAction runWorker `catch` catchWorker) *> Concurrently runTransactionServer
  pure $ TransactionServer { runTransactionServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Job worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runServer :: RunTransactionServer WorkerM
  , getSubmitJob :: TxId -> STM (Maybe SubmitJob)
  , trackSubmitJob :: TxId -> SubmitJob -> STM ()
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext
  , loadMarloweContext :: LoadMarloweContext
  , logAction :: AppLogAction
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
  }

type AppLogAction = Colog.LogAction IO Colog.Message

type WorkerM = Colog.LoggerT Colog.Message IO

instance MonadBase IO WorkerM where
  liftBase = liftIO

instance MonadBaseControl IO WorkerM where
  type StM WorkerM a = a
  liftBaseWith withRunInBase = Colog.LoggerT $ ReaderT \logAction -> do
    liftIO $ withRunInBase \(Colog.LoggerT r) -> runReaderT r logAction
  restoreM = pure

newtype Worker = Worker
  { runWorker :: WorkerM ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunServer run = runServer
  in
    pure Worker { runWorker = run server }

  where
    server :: JobServer MarloweTxCommand WorkerM ()
    server = JobServer $ pure serverInit

    serverInit :: ServerStInit MarloweTxCommand WorkerM ()
    serverInit = ServerStInit
      { recvMsgExec = \command -> do
          systemStart <- liftIO $ queryChainSync GetSystemStart
          eraHistory <- liftIO $ queryChainSync GetEraHistory
          protocolParameters <- liftIO $ queryChainSync GetProtocolParameters
          networkId <- liftIO $ queryChainSync GetNetworkId
          liftIO $ when (networkId == Mainnet) do
            die "Mainnet support is currently disabled."
          let
            solveConstraints :: Constraints.SolveConstraints
            solveConstraints =
              Constraints.solveConstraints
                systemStart
                eraHistory
                protocolParameters
          case command of
            Create mStakeCredential version addresses roles metadata minAda contract ->
              execCreate
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
            ApplyInputs version addresses contractId invalidBefore invalidHereafter redeemer ->
              withMarloweVersion version $ execApplyInputs
                (unsafeCoerce systemStart) -- TODO fix this when the imports are not messed up anymore
                eraHistory
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
      }

attachSubmit
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe SubmitJob)
  -> WorkerM (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader WorkerM ())
attachSubmit jobId getSubmitJob =
  liftIO $ atomically $ fmap (hoistAttach $ liftIO . atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: SolveConstraints
  -> LoadWalletContext
  -> NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> TransactionMetadata
  -> Chain.Lovelace
  -> Contract v
  -> WorkerM (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) WorkerM ())
execCreate solveConstraints loadWalletContext networkId mStakeCredential version addresses roleTokens metadata minAda contract = execExceptT do
  walletContext <- liftIO $ loadWalletContext addresses
  lift . Colog.logDebug . O.renderValue . A.toJSON $ walletContext
  mCardanoStakeCredential <- except $ traverse (note CreateToCardanoError . toCardanoStakeCredential) mStakeCredential
  ((datum, assets, rolesCurrency), constraints) <- except
    $ buildCreateConstraints version walletContext roleTokens metadata minAda contract
  let
    scripts@Registry.MarloweScripts{..} = Registry.getCurrentScripts version
    stakeReference = maybe NoStakeAddress StakeAddressByValue mCardanoStakeCredential
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
  pure ContractCreated
    { contractId = ContractId $ findMarloweOutput mCardanoStakeCredential txBody
    , rolesCurrency
    , metadata = Chain.unTransactionMetadata metadata
    , txBody
    , marloweScriptHash = Constraints.marloweScriptHash marloweContext
    , marloweScriptAddress = Constraints.marloweAddress marloweContext
    , payoutScriptHash = Constraints.payoutScriptHash marloweContext
    , payoutScriptAddress = Constraints.payoutAddress marloweContext
    , version
    , datum
    , assets
    }
  where
  findMarloweOutput mCardanoStakeCredential = \case
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
        $ maybe NoStakeAddress StakeAddressByValue mCardanoStakeCredential
      isToCurrentScriptAddress (TxOut (AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) address) _ _ _) = address == scriptAddress
      isToCurrentScriptAddress _ = False

execApplyInputs
  :: SystemStart
  -> EraHistory CardanoMode
  -> SolveConstraints
  -> LoadWalletContext
  -> LoadMarloweContext
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Redeemer v
  -> WorkerM (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (TxBody BabbageEra) WorkerM ())
execApplyInputs
  systemStart
  eraHistory
  solveConstraints
  loadWalletContext
  loadMarloweContext
  version
  addresses
  contractId
  invalidBefore
  invalidHereafter
  inputs = execExceptT do
    marloweContext@MarloweContext{..} <- withExceptT ApplyInputsLoadMarloweContextFailed
      $ ExceptT
      $ liftIO $ loadMarloweContext version contractId
    invalidBefore' <- liftIO $ maybe getCurrentTime pure invalidBefore
    scriptOutput' <- except $ maybe (Left ScriptOutputNotFound) Right scriptOutput
    constraints <- except $ buildApplyInputsConstraints
        systemStart
        eraHistory
        version
        scriptOutput'
        invalidBefore'
        invalidHereafter
        inputs
    walletContext <- liftIO $ loadWalletContext addresses
    lift . Colog.logDebug . O.renderValue . A.toJSON $ walletContext
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
  -> WorkerM (ServerStCmd MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra) WorkerM ())
execWithdraw solveConstraints loadWalletContext loadMarloweContext version addresses contractId roleToken = liftIO $ execExceptT $ case version of
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
    walletContext <- lift $ loadWalletContext addresses
    except
      $ first WithdrawConstraintError
      $ solveConstraints version marloweContext walletContext constraints

execSubmit
  :: (Tx BabbageEra -> STM SubmitJob)
  -> (TxId -> SubmitJob -> STM ())
  -> Tx BabbageEra
  -> WorkerM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader WorkerM ())
execSubmit mkSubmitJob trackSubmitJob tx = liftIO do
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
  hoistCmd (liftIO . atomically) <$> atomically (submitJobServerCmd (JobIdSubmit txId) submitJob)

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
