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
  , IsCardanoEra
  , NetworkId(..)
  , ShelleyBasedEra(..)
  , StakeAddressReference(..)
  , Tx
  , TxBody(..)
  , TxBodyContent(..)
  , TxOut(..)
  , cardanoEra
  , getTxBody
  , getTxId
  , makeShelleyAddress
  )
import Cardano.Api.Shelley (ProtocolParameters)
import Control.Applicative ((<|>))
import Control.Concurrent (forkFinally)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, atomically, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar)
import Control.Error.Util (hoistMaybe, note, noteT)
import Control.Exception (Exception(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT(..), except, runExceptT, withExceptT)
import Data.Bifunctor (first)
import Data.List (find)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Time (UTCTime)
import Data.Void (Void)
import Language.Marlowe.Runtime.Cardano.Api
  (fromCardanoAddressInEra, fromCardanoTxId, toCardanoPaymentCredential, toCardanoStakeCredential)
import Language.Marlowe.Runtime.ChainSync.Api
  (BlockHeader, ChainSyncQuery(..), Credential(..), TokenName, TransactionMetadata, TxId(..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api
  ( Contract
  , ContractId(..)
  , Inputs
  , MarloweVersion(MarloweV1)
  , Payout(Payout, datum)
  , TransactionScriptOutput(..)
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
  ( LoadMarloweContext
  , LoadMarloweContextSelector
  , LoadWalletContext
  , LoadWalletContextSelector
  , lookupMarloweScriptUtxo
  , lookupPayoutScriptUtxo
  )
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob(..), SubmitJobStatus(..))
import Network.Protocol.Driver (RunServer(..))
import Network.Protocol.Job.Server
  (JobServer(..), ServerStAttach(..), ServerStAwait(..), ServerStCmd(..), ServerStInit(..), hoistAttach, hoistCmd)
import Observe.Event (Event, EventBackend, addField, subEventBackend, withEvent, withSubEvent)
import Ouroboros.Consensus.BlockchainTime (SystemStart)
import System.Exit (die)

data TransactionServerSelector f where
  Exec :: TransactionServerSelector ExecField
  ExecCreate :: TransactionServerSelector BuildTxField
  ExecApplyInputs :: TransactionServerSelector BuildTxField
  ExecWithdraw :: TransactionServerSelector BuildTxField
  LoadWalletContext :: LoadWalletContextSelector f -> TransactionServerSelector f
  LoadMarloweContext :: LoadMarloweContextSelector f -> TransactionServerSelector f

data ExecField
  = SystemStart SystemStart
  | EraHistory (EraHistory CardanoMode)
  | ProtocolParameters ProtocolParameters
  | NetworkId NetworkId

data BuildTxField where
  Constraints :: MarloweVersion v -> TxConstraints v -> BuildTxField
  ResultingTxBody :: TxBody BabbageEra -> BuildTxField

type RunTransactionServer m = RunServer m (JobServer MarloweTxCommand)

data TransactionServerDependencies r = TransactionServerDependencies
  { acceptRunTransactionServer :: IO (RunTransactionServer IO)
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext r
  , loadMarloweContext :: LoadMarloweContext r
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
  , getTip :: STM Chain.ChainPoint
  , eventBackend :: EventBackend IO r TransactionServerSelector
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  }

transactionServer :: Component IO (TransactionServerDependencies r) ()
transactionServer = serverComponentWithSetup worker \TransactionServerDependencies{..} -> do
  submitJobsVar <- newTVar mempty
  let
    getSubmitJob txId = Map.lookup txId <$> readTVar submitJobsVar
    trackSubmitJob txId = modifyTVar submitJobsVar . Map.insert txId
  pure do
    runServer <- acceptRunTransactionServer
    pure WorkerDependencies {..}

data WorkerDependencies r = WorkerDependencies
  { runServer :: RunTransactionServer IO
  , getSubmitJob :: TxId -> STM (Maybe SubmitJob)
  , trackSubmitJob :: TxId -> SubmitJob -> STM ()
  , mkSubmitJob :: Tx BabbageEra -> STM SubmitJob
  , loadWalletContext :: LoadWalletContext r
  , loadMarloweContext :: LoadMarloweContext r
  , queryChainSync :: forall e a. ChainSyncQuery Void e a -> IO a
  , getTip :: STM Chain.ChainPoint
  , eventBackend :: EventBackend IO r TransactionServerSelector
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  }

worker :: Component IO (WorkerDependencies r) ()
worker = component_ \WorkerDependencies{..} -> do
  let
    RunServer run = runServer

    server :: JobServer MarloweTxCommand IO ()
    server = JobServer $ pure serverInit

    serverInit :: ServerStInit MarloweTxCommand IO ()
    serverInit = ServerStInit
      { recvMsgExec = \command -> withEvent eventBackend Exec \ev -> do
          systemStart <- liftIO $ queryChainSync GetSystemStart
          addField ev $ SystemStart systemStart
          eraHistory <- liftIO $ queryChainSync GetEraHistory
          addField ev $ EraHistory eraHistory
          protocolParameters <- liftIO $ queryChainSync GetProtocolParameters
          addField ev $ ProtocolParameters protocolParameters
          networkId <- liftIO $ queryChainSync GetNetworkId
          addField ev $ NetworkId networkId
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
              withSubEvent ev ExecCreate \ev' -> execCreate
                getCurrentScripts
                ev'
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
              withSubEvent ev ExecApplyInputs \ev' -> withMarloweVersion version $ execApplyInputs
                ev'
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
              withSubEvent ev ExecWithdraw \ev' -> execWithdraw
                ev'
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
  run server

attachSubmit
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe SubmitJob)
  -> IO (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
attachSubmit jobId getSubmitJob =
  liftIO $ atomically $ fmap (hoistAttach $ liftIO . atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: (MarloweVersion v -> MarloweScripts)
  -> Event IO r TransactionServerSelector BuildTxField
  -> SolveConstraints
  -> LoadWalletContext r
  -> NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> TransactionMetadata
  -> Chain.Lovelace
  -> Contract v
  -> IO (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) IO ())
execCreate getCurrentScripts ev solveConstraints loadWalletContext networkId mStakeCredential version addresses roleTokens metadata minAda contract = execExceptT do
  walletContext <- liftIO $ loadWalletContext (subEventBackend LoadWalletContext ev) addresses
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
  txBody <- except
    $ first CreateConstraintError
    $ solveConstraints version marloweContext walletContext constraints
  let marloweScriptAddress = Constraints.marloweAddress marloweContext
  pure ContractCreated
    { contractId = ContractId $ fromJust $ findMarloweOutput marloweAddress txBody
    , rolesCurrency
    , metadata = Chain.unTransactionMetadata metadata
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
  :: Event IO r TransactionServerSelector BuildTxField
  -> STM Chain.ChainPoint
  -> SystemStart
  -> EraHistory CardanoMode
  -> SolveConstraints
  -> LoadWalletContext r
  -> LoadMarloweContext r
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> TransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs v
  -> IO (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (InputsApplied BabbageEra v) IO ())
execApplyInputs
  ev
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
      $ liftIO $ loadMarloweContext (subEventBackend LoadMarloweContext ev) version contractId
    tip <- liftIO $ atomically getTip
    tipSlot <- except case tip of
      Chain.Genesis -> Left TipAtGenesis
      Chain.At Chain.BlockHeader{..} -> Right slotNo
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
    walletContext <- liftIO $ loadWalletContext (subEventBackend LoadWalletContext ev)addresses
    txBody <- except
      $ first ApplyInputsConstraintError
      $ solveConstraints version marloweContext walletContext constraints
    let input = scriptOutput'
    let buildOutput (assets, datum) utxo = TransactionScriptOutput marloweAddress assets utxo datum
    let output = buildOutput <$> mAssetsAndDatum <*> findMarloweOutput marloweAddress txBody
    pure InputsApplied{..}

execWithdraw
  :: Event IO r TransactionServerSelector BuildTxField
  -> SolveConstraints
  -> LoadWalletContext r
  -> LoadMarloweContext r
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> TokenName
  -> IO (ServerStCmd MarloweTxCommand Void (WithdrawError v) (TxBody BabbageEra) IO ())
execWithdraw ev solveConstraints loadWalletContext loadMarloweContext version addresses contractId roleToken = liftIO $ execExceptT $ case version of
  MarloweV1 -> do
    marloweContext@MarloweContext{payoutOutputs=Map.elems -> payouts} <- withExceptT WithdrawLoadMarloweContextFailed
      $ ExceptT
      $ loadMarloweContext (subEventBackend LoadMarloweContext ev) version contractId
    let
      payoutAssetId Payout {datum = assetId } = assetId
      isRolePayout (Chain.AssetId _ roleName) = roleName == roleToken
      possibleDatum = find isRolePayout . map payoutAssetId $ payouts
    datum <- noteT (UnableToFindPayoutForAGivenRole roleToken) $ hoistMaybe possibleDatum
    constraints <- except $ buildWithdrawConstraints version datum
    walletContext <- lift $ loadWalletContext (subEventBackend LoadWalletContext ev) addresses
    except
      $ first WithdrawConstraintError
      $ solveConstraints version marloweContext walletContext constraints

execSubmit
  :: (Tx BabbageEra -> STM SubmitJob)
  -> (TxId -> SubmitJob -> STM ())
  -> Tx BabbageEra
  -> IO (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader IO ())
execSubmit mkSubmitJob trackSubmitJob tx = liftIO do
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
