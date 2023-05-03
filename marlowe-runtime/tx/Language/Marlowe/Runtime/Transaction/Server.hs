{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.Server
  where

import Cardano.Api
  ( AddressInEra(..)
  , AddressTypeInEra(..)
  , BabbageEra
  , CardanoEra(BabbageEra)
  , CardanoMode
  , EraHistory
  , IsCardanoEra
  , NetworkId(..)
  , ShelleyBasedEra(..)
  , StakeAddressReference(..)
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxMetadataInEra(..)
  , TxOut(..)
  , cardanoEra
  , getTxBody
  , getTxId
  , makeShelleyAddress
  )
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Applicative ((<|>))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar, retry)
import Control.Error.Util (hoistMaybe, note, noteT)
import Control.Exception (Exception(..))
import Control.Monad (unless)
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT, throwE, withExceptT)
import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoTxId, toCardanoPaymentCredential, toCardanoStakeCredential)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader, ChainSyncQuery(..), Credential(..), TokenName, TxId(..), fromCardanoTxMetadata)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , ContractId(..)
  , Inputs
  , MarloweTransactionMetadata
  , MarloweVersion(MarloweV1)
  , Payout(Payout, datum)
  , TransactionScriptOutput(..)
  , decodeMarloweTransactionMetadataLenient
  , withMarloweVersion
  )
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts(..))
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError(..)
  , ContractCreated(..)
  , CreateError(..)
  , InputsApplied(..)
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
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext(..), SolveConstraints, TxConstraints)
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Language.Marlowe.Runtime.Transaction.Query
  (LoadMarloweContext, LoadWalletContext, lookupMarloweScriptUtxo, lookupPayoutScriptUtxo)
import Language.Marlowe.Runtime.Transaction.Safety (checkContract, checkTransactions, noContinuations)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob(..), SubmitJobStatus(..))
import Network.Protocol.Connection (SomeConnectionSource(..), SomeServerConnector, acceptSomeConnector)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Job.Server
  (JobServer(..), ServerStAttach(..), ServerStAwait(..), ServerStCmd(..), ServerStInit(..), hoistAttach, hoistCmd)
import Observe.Event.Explicit (addField)
import Ouroboros.Consensus.BlockchainTime (SystemStart)
import UnliftIO (MonadUnliftIO, atomically)
import UnliftIO.Concurrent (forkFinally)

data TransactionServerSelector f where
  Exec :: TransactionServerSelector ExecField
  ExecCreate :: TransactionServerSelector BuildTxField
  ExecApplyInputs :: TransactionServerSelector BuildTxField
  ExecWithdraw :: TransactionServerSelector BuildTxField

data ExecField
  = SystemStart SystemStart
  | EraHistory (EraHistory CardanoMode)
  | ProtocolParameters ProtocolParameters
  | NetworkId NetworkId

data BuildTxField where
  Constraints :: MarloweVersion v -> TxConstraints v -> BuildTxField
  ResultingTxBody :: TxBody BabbageEra -> BuildTxField

data TransactionServerDependencies m = TransactionServerDependencies
  { connectionSource :: SomeConnectionSource (JobServer MarloweTxCommand) m
  , mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> m a
  , getTip :: STM Chain.ChainPoint
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  }

transactionServer
  :: (MonadInjectEvent r TransactionServerSelector s m, MonadUnliftIO m)
  => Component m (TransactionServerDependencies m) ()
transactionServer = serverComponentWithSetup worker \TransactionServerDependencies{..} -> do
  submitJobsVar <- newTVar mempty
  let
    getSubmitJob txId = Map.lookup txId <$> readTVar submitJobsVar
    trackSubmitJob txId = modifyTVar submitJobsVar . Map.insert txId
  pure do
    connector <- acceptSomeConnector connectionSource
    pure WorkerDependencies {..}

data WorkerDependencies m = WorkerDependencies
  { connector :: SomeServerConnector (JobServer MarloweTxCommand) m
  , getSubmitJob :: TxId -> STM (Maybe (SubmitJob m))
  , trackSubmitJob :: TxId -> SubmitJob m -> STM ()
  , mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> m a
  , getTip :: STM Chain.ChainPoint
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  }

worker
  :: forall r s m
   . (MonadInjectEvent r TransactionServerSelector s m, MonadUnliftIO m)
  => Component m (WorkerDependencies m) ()
worker = component_ \WorkerDependencies{..} -> do
  let
    server :: JobServer MarloweTxCommand m ()
    server = JobServer $ pure serverInit

    serverInit :: ServerStInit MarloweTxCommand m ()
    serverInit = ServerStInit
      { recvMsgExec = \command -> withEvent Exec \ev -> do
          systemStart <- queryChainSync GetSystemStart
          addField ev $ SystemStart systemStart
          eraHistory <- queryChainSync GetEraHistory
          addField ev $ EraHistory eraHistory
          protocolParameters <- queryChainSync GetProtocolParameters
          addField ev $ ProtocolParameters protocolParameters
          networkId <- queryChainSync GetNetworkId
          addField ev $ NetworkId networkId
          let
            solveConstraints :: Constraints.SolveConstraints
            solveConstraints =
              Constraints.solveConstraints
                systemStart
                eraHistory
                protocolParameters
          case command of
            Create mStakeCredential version addresses roles metadata minAda contract ->
              withEvent ExecCreate \_ -> execCreate
                getCurrentScripts
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
            ApplyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs ->
              withEvent ExecApplyInputs \_ -> withMarloweVersion version $ execApplyInputs
                getTip
                systemStart
                eraHistory
                solveConstraints
                loadWalletContext
                loadMarloweContext
                version
                addresses
                contractId
                metadata
                invalidBefore
                invalidHereafter
                inputs
            Withdraw version addresses contractId roleToken ->
              withEvent ExecWithdraw \_ -> execWithdraw
                solveConstraints
                loadWalletContext
                loadMarloweContext
                version
                addresses
                contractId
                roleToken
            Submit tx -> execSubmit mkSubmitJob trackSubmitJob tx
      , recvMsgAttach = \case
          jobId@(JobIdSubmit txId) ->
            attachSubmit jobId $ getSubmitJob txId
      }
  runSomeConnector connector server

attachSubmit
  :: MonadIO m
  => JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe (SubmitJob m))
  -> m (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
attachSubmit jobId getSubmitJob =
  atomically $ fmap (hoistAttach $ liftIO . atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: MonadIO m
  => (MarloweVersion v -> MarloweScripts)
  -> SolveConstraints
  -> LoadWalletContext m
  -> NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Chain.Lovelace
  -> Contract v
  -> m (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) m ())
execCreate getCurrentScripts solveConstraints loadWalletContext networkId mStakeCredential version addresses roleTokens metadata minAda contract = execExceptT do
  walletContext <- lift $ loadWalletContext addresses
  mCardanoStakeCredential <- except $ traverse (note CreateToCardanoError . toCardanoStakeCredential) mStakeCredential
  ((datum, assets, rolesCurrency), constraints) <- except
    $ buildCreateConstraints version walletContext roleTokens metadata minAda contract
  let
    scripts@MarloweScripts{..} = getCurrentScripts version
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
  let
    continuations = noContinuations version  -- FIXME: Revise this when continuations are available at creation.
    contractSafetyErrors =
      if False  -- FIXME: Disabled because of incompatibility with integration tests.
        then checkContract roleTokens version contract continuations
        else mempty
  -- FIXME: The is a placeholder until we design safety-analysis reporting.
  unless (null contractSafetyErrors)
    . throwE . CreateSafetyAnalysisError
    $ show contractSafetyErrors
  transactionSafetyErrors <-
    ExceptT
      $ first CreateSafetyAnalysisError
      <$> if False  -- FIXME: Disabled because of incompatibility with integration tests.
            then checkTransactions solveConstraints version marloweContext rolesCurrency (changeAddress addresses) (toInteger minAda) contract continuations
            else pure $ pure mempty
  -- FIXME: The is a placeholder until we design safety-analysis reporting.
  unless (null transactionSafetyErrors)
    . throwE . CreateSafetyAnalysisError
    $ show transactionSafetyErrors
  txBody <- except
    $ first CreateConstraintError
    $ solveConstraints version marloweContext walletContext constraints
  let marloweScriptAddress = Constraints.marloweAddress marloweContext
  pure ContractCreated
    { contractId = ContractId $ fromJust $ findMarloweOutput marloweAddress txBody
    , rolesCurrency
    , metadata = decodeMarloweTransactionMetadataLenient case txBody of
        TxBody TxBodyContent{..} -> case txMetadata of
          TxMetadataNone -> mempty
          TxMetadataInEra _ m -> fromCardanoTxMetadata m
    , txBody
    , marloweScriptHash = Constraints.marloweScriptHash marloweContext
    , marloweScriptAddress
    , payoutScriptHash = Constraints.payoutScriptHash marloweContext
    , payoutScriptAddress = Constraints.payoutAddress marloweContext
    , version
    , datum
    , assets
    }

findMarloweOutput :: forall era. IsCardanoEra era => Chain.Address -> TxBody era -> Maybe Chain.TxOutRef
findMarloweOutput address = \case
  body@(TxBody TxBodyContent{..}) -> fmap (Chain.TxOutRef (fromCardanoTxId $ getTxId body) . fst)
    $ find (isToCurrentScriptAddress . snd)
    $ zip [0..] txOuts
  where
    isToCurrentScriptAddress (TxOut address' _ _ _) =
      address == fromCardanoAddressInEra (cardanoEra @era) address'

execApplyInputs
  :: MonadIO m
  => STM Chain.ChainPoint
  -> SystemStart
  -> EraHistory CardanoMode
  -> SolveConstraints
  -> LoadWalletContext m
  -> LoadMarloweContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> MarloweTransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs v
  -> m (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (InputsApplied BabbageEra v) m ())
execApplyInputs
  getTip
  systemStart
  eraHistory
  solveConstraints
  loadWalletContext
  loadMarloweContext
  version
  addresses
  contractId
  metadata
  invalidBefore'
  invalidHereafter'
  inputs = execExceptT do
    marloweContext@MarloweContext{..} <- withExceptT ApplyInputsLoadMarloweContextFailed
      $ ExceptT
      $ loadMarloweContext version contractId
    let
      getTipSlot = atomically $ getTip >>= \case
        Chain.Genesis -> retry
        Chain.At Chain.BlockHeader{..} -> pure slotNo
    tipSlot <- liftIO getTipSlot
    scriptOutput' <- except $ maybe (Left ScriptOutputNotFound) Right scriptOutput
    ((invalidBefore, invalidHereafter, mAssetsAndDatum), constraints) <-
      except $ buildApplyInputsConstraints
        systemStart
        eraHistory
        version
        scriptOutput'
        tipSlot
        metadata
        invalidBefore'
        invalidHereafter'
        inputs
    walletContext <- lift $ loadWalletContext addresses
    txBody <- except
      $ first ApplyInputsConstraintError
      $ solveConstraints version marloweContext walletContext constraints
    let input = scriptOutput'
    let buildOutput (assets, datum) utxo = TransactionScriptOutput marloweAddress assets utxo datum
    let output = buildOutput <$> mAssetsAndDatum <*> findMarloweOutput marloweAddress txBody
    pure InputsApplied
      { metadata = decodeMarloweTransactionMetadataLenient case txBody of
          TxBody TxBodyContent{..} -> case txMetadata of
            TxMetadataNone -> mempty
            TxMetadataInEra _ m -> fromCardanoTxMetadata m
      , ..
      }

execWithdraw
  :: Monad m
  => SolveConstraints
  -> LoadWalletContext m
  -> LoadMarloweContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> TokenName
  -> m (ServerStCmd MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra) m ())
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
    walletContext <- lift $ loadWalletContext addresses
    except
      $ first WithdrawConstraintError
      $ solveConstraints version marloweContext walletContext constraints

execSubmit
  :: MonadUnliftIO m
  => (Tx BabbageEra -> STM (SubmitJob m))
  -> (TxId -> SubmitJob m -> STM ())
  -> Tx BabbageEra
  -> m (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
execSubmit mkSubmitJob trackSubmitJob tx = do
  let txId = fromCardanoTxId $ getTxId $ getTxBody tx
  (submitJob, exVar) <- atomically do
    exVar <- newEmptyTMVar
    submitJob <- mkSubmitJob tx
    let getExceptionStatus = Failed . SubmitException <$> readTMVar exVar
    let submitJob' = submitJob { submitJobStatus = getExceptionStatus <|> submitJobStatus submitJob }
    trackSubmitJob txId submitJob'
    pure (submitJob', exVar)
  -- Run the job in a new thread
  _ <- forkFinally (runSubmitJob submitJob) \case
    Left ex -> atomically $ putTMVar exVar $ displayException ex
    _ -> pure ()
  -- Make a new server and run it in IO.
  hoistCmd (liftIO . atomically) <$> atomically (submitJobServerCmd (JobIdSubmit txId) submitJob)

submitJobServerAttach
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> Maybe (SubmitJob m)
  -> STM (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
submitJobServerAttach jobId = maybe
  (pure $ SendMsgAttachFailed ())
  (fmap SendMsgAttached . submitJobServerCmd jobId)

submitJobServerCmd
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> SubmitJob m
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
