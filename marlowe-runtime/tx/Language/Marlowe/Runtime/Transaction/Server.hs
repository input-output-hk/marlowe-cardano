{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.Transaction.Server where

import Cardano.Api (
  AddressInEra (..),
  AddressTypeInEra (..),
  AnyCardanoEra (..),
  CardanoEra (..),
  CardanoMode,
  CtxTx,
  EraHistory,
  IsCardanoEra,
  NetworkId (..),
  ScriptDataSupportedInEra (ScriptDataInBabbageEra),
  ShelleyBasedEra (..),
  StakeAddressReference (..),
  Tx,
  TxBody (..),
  TxBodyContent (..),
  TxMetadataInEra (..),
  TxOut (..),
  cardanoEra,
  getTxBody,
  getTxId,
  makeShelleyAddress,
 )
import Cardano.Api.Shelley (
  ProtocolParameters,
  ReferenceTxInsScriptsInlineDatumsSupportedInEra (..),
 )
import Colog (Message, WithLog)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar, retry)
import Control.Error (MaybeT (..))
import Control.Error.Util (hoistMaybe, hush, note, noteT)
import Control.Exception (Exception (..), SomeException)
import Control.Monad (guard, (<=<))
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, nominalDiffTimeToSeconds)
import Data.Void (Void)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (SafetyAnalysisTimeout))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressInEra,
  fromCardanoTxId,
  fromCardanoTxIn,
  fromCardanoTxOutDatum,
  fromCardanoTxOutValue,
  toCardanoPaymentCredential,
  toCardanoStakeCredential,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader,
  ChainSyncQuery (..),
  Credential (..),
  DatumHash,
  TokenName,
  TxId (..),
  fromCardanoTxMetadata,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Contract.Api (ContractRequest, getContract, merkleizeInputs)
import qualified Language.Marlowe.Runtime.Contract.Api as Contract
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  ContractId (..),
  Inputs,
  MarloweTransactionMetadata,
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (Payout, datum),
  TransactionOutput (..),
  TransactionScriptOutput (..),
  decodeMarloweTransactionMetadataLenient,
  fromChainPayoutDatum,
  withMarloweVersion,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (..))
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsError (..),
  ContractCreated (..),
  ContractCreatedInEra (..),
  CreateError (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  JobId (..),
  MarloweTxCommand (..),
  RoleTokensConfig,
  SubmitError (..),
  SubmitStatus (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
 )
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  buildApplyInputsConstraints,
  buildCreateConstraints,
  buildWithdrawConstraints,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext (..), SolveConstraints, TxConstraints)
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Language.Marlowe.Runtime.Transaction.Query (
  LoadMarloweContext,
  LoadWalletContext,
  lookupMarloweScriptUtxo,
  lookupPayoutScriptUtxo,
 )
import Language.Marlowe.Runtime.Transaction.Safety (Continuations, checkContract, checkTransactions, noContinuations)
import Language.Marlowe.Runtime.Transaction.Submit (SubmitJob (..), SubmitJobStatus (..))
import Network.Protocol.Connection (Connector, ServerSource (..), runConnector)
import Network.Protocol.Job.Server (
  JobServer (..),
  ServerStAttach (..),
  ServerStAwait (..),
  ServerStCmd (..),
  ServerStInit (..),
 )
import Network.Protocol.Query.Client (QueryClient, request)
import Observe.Event.Explicit (addField)
import Ouroboros.Consensus.BlockchainTime (SystemStart)
import UnliftIO (MonadUnliftIO, atomically, throwIO)
import UnliftIO.Concurrent (forkFinally)
import Witherable (mapMaybe)

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
  | Era AnyCardanoEra

data BuildTxField where
  Constraints :: (IsCardanoEra era) => MarloweVersion v -> TxConstraints era v -> BuildTxField
  ResultingTxBody :: (IsCardanoEra era) => TxBody era -> BuildTxField

data TransactionServerDependencies m = TransactionServerDependencies
  { mkSubmitJob :: forall era. ScriptDataSupportedInEra era -> Tx era -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , contractQueryConnector :: Connector (QueryClient ContractRequest) m
  , getTip :: STM Chain.ChainPoint
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  , analysisTimeout :: NominalDiffTime
  }

transactionServer
  :: forall r s env m
   . (MonadInjectEvent r TransactionServerSelector s m, MonadUnliftIO m, WithLog env Message m)
  => Component m (TransactionServerDependencies m) (ServerSource (JobServer MarloweTxCommand) m ())
transactionServer = component "tx-job-server" \TransactionServerDependencies{..} -> do
  submitJobsVar <- newTVar @(Map TxId (SubmitJob m)) mempty
  let getSubmitJob txId = Map.lookup txId <$> readTVar submitJobsVar

      trackSubmitJob txId = modifyTVar submitJobsVar . Map.insert txId

      server = JobServer $ pure serverInit

      serverInit :: ServerStInit MarloweTxCommand m ()
      serverInit =
        ServerStInit
          { recvMsgExec = \command -> withEvent Exec \ev -> do
              (systemStart, eraHistory, protocolParameters, networkId, AnyCardanoEra era) <-
                runConnector chainSyncQueryConnector $
                  (,,,,)
                    <$> request GetSystemStart
                    <*> request GetEraHistory
                    <*> request GetProtocolParameters
                    <*> request GetNetworkId
                    <*> request GetEra
              addField ev $ SystemStart systemStart
              addField ev $ EraHistory eraHistory
              addField ev $ ProtocolParameters protocolParameters
              addField ev $ NetworkId networkId
              addField ev $ Era $ AnyCardanoEra era
              let solveConstraints :: Constraints.SolveConstraints
                  solveConstraints =
                    Constraints.solveConstraints
                      systemStart
                      eraHistory
                      protocolParameters
              case command of
                Create mStakeCredential version addresses roles metadata minAda contract ->
                  withEvent ExecCreate \_ ->
                    execCreate
                      era
                      contractQueryConnector
                      getCurrentScripts
                      solveConstraints
                      protocolParameters
                      loadWalletContext
                      networkId
                      mStakeCredential
                      version
                      addresses
                      roles
                      metadata
                      minAda
                      contract
                      analysisTimeout
                ApplyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs ->
                  withEvent ExecApplyInputs \_ ->
                    withMarloweVersion version $
                      execApplyInputs
                        era
                        contractQueryConnector
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
                  withEvent ExecWithdraw \_ ->
                    execWithdraw
                      era
                      solveConstraints
                      loadWalletContext
                      loadMarloweContext
                      version
                      addresses
                      contractId
                      roleToken
                Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra tx ->
                  execSubmit (mkSubmitJob ScriptDataInBabbageEra) trackSubmitJob tx
          , recvMsgAttach = \case
              jobId@(JobIdSubmit txId) ->
                attachSubmit jobId $ getSubmitJob txId
          }
  pure (pure (), ServerSource $ pure server)

attachSubmit
  :: (MonadIO m)
  => JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe (SubmitJob m))
  -> m (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
attachSubmit jobId = submitJobServerAttach jobId <=< atomically

execCreate
  :: forall era m v
   . (MonadUnliftIO m, IsCardanoEra era)
  => CardanoEra era
  -> Connector (QueryClient ContractRequest) m
  -> (MarloweVersion v -> MarloweScripts)
  -> SolveConstraints
  -> ProtocolParameters
  -> LoadWalletContext m
  -> NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Chain.Lovelace
  -> Either (Contract v) DatumHash
  -> NominalDiffTime
  -> m (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractCreated v) m ())
execCreate era contractQueryConnector getCurrentScripts solveConstraints protocolParameters loadWalletContext networkId mStakeCredential version addresses roleTokens metadata minAda contract analysisTimeout = execExceptT do
  referenceInputsSupported <- referenceInputsSupportedInEra (CreateEraUnsupported $ AnyCardanoEra era) era
  walletContext <- lift $ loadWalletContext addresses
  (contract', continuations) <- case contract of
    Right hash -> case version of
      MarloweV1 -> noteT CreateContractNotFound do
        let getContract' = MaybeT . runConnector contractQueryConnector . getContract
        Contract.ContractWithAdjacency{contract = c, ..} <- getContract' hash
        (c :: Contract v,) <$> foldMapM (fmap singletonContinuations . getContract') (Set.delete hash closure)
    Left c -> pure (c, noContinuations version)
  mCardanoStakeCredential <- except $ traverse (note CreateToCardanoError . toCardanoStakeCredential) mStakeCredential
  ((datum, assets, rolesCurrency), constraints) <-
    except $
      buildCreateConstraints referenceInputsSupported version walletContext roleTokens metadata minAda contract'
  let scripts@MarloweScripts{..} = getCurrentScripts version
      stakeReference = maybe NoStakeAddress StakeAddressByValue mCardanoStakeCredential
      marloweAddress =
        fromCardanoAddressInEra BabbageEra $
          AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) $
            makeShelleyAddress
              networkId
              (fromJust $ toCardanoPaymentCredential $ ScriptCredential marloweScript)
              stakeReference
      payoutAddress =
        fromCardanoAddressInEra BabbageEra $
          AddressInEra (ShelleyAddressInEra ShelleyBasedEraBabbage) $
            makeShelleyAddress
              networkId
              (fromJust $ toCardanoPaymentCredential $ ScriptCredential payoutScript)
              NoStakeAddress
  marloweContext <- except $ first CreateLoadMarloweContextFailed do
    marloweScriptUTxO <- lookupMarloweScriptUtxo networkId scripts
    payoutScriptUTxO <- lookupPayoutScriptUtxo networkId scripts
    pure
      MarloweContext
        { scriptOutput = Nothing
        , payoutOutputs = mempty
        , marloweAddress
        , payoutAddress
        , marloweScriptUTxO
        , payoutScriptUTxO
        , marloweScriptHash = marloweScript
        , payoutScriptHash = payoutScript
        }
  let -- Fast analysis of safety: examines bounds for transactions.
      contractSafetyErrors = checkContract networkId roleTokens version contract' continuations
      limitAnalysisTime =
        liftIO
          . fmap (either Right id)
          . race ([SafetyAnalysisTimeout] <$ threadDelay (floor $ nominalDiffTimeToSeconds analysisTimeout * 1_000_000))
  -- Slow analysis of safety: examines all possible transactions.
  transactionSafetyErrors <-
    ExceptT $
      first CreateSafetyAnalysisError
        <$> limitAnalysisTime
          ( checkTransactions
              protocolParameters
              referenceInputsSupported
              version
              marloweContext
              rolesCurrency
              (changeAddress addresses)
              (toInteger minAda)
              contract'
              continuations
          )
  txBody <-
    except $
      first CreateConstraintError $
        solveConstraints referenceInputsSupported version marloweContext walletContext constraints
  let marloweScriptAddress = Constraints.marloweAddress marloweContext
  pure $
    ContractCreated referenceInputsSupported $
      ContractCreatedInEra
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
        , safetyErrors = contractSafetyErrors <> transactionSafetyErrors
        }

referenceInputsSupportedInEra
  :: (Monad m) => e -> CardanoEra era -> ExceptT e m (ReferenceTxInsScriptsInlineDatumsSupportedInEra era)
referenceInputsSupportedInEra e = \case
  ByronEra -> throwE e
  ShelleyEra -> throwE e
  AllegraEra -> throwE e
  MaryEra -> throwE e
  AlonzoEra -> throwE e
  BabbageEra -> pure ReferenceTxInsScriptsInlineDatumsInBabbageEra

singletonContinuations :: Contract.ContractWithAdjacency -> Continuations 'V1
singletonContinuations Contract.ContractWithAdjacency{..} = Map.singleton contractHash contract

foldMapM :: (Applicative m, Monoid b, Foldable t) => (a -> m b) -> t a -> m b
foldMapM f = foldl' (\b a -> mappend <$> b <*> f a) (pure mempty)

findMarloweOutput :: forall era. (IsCardanoEra era) => Chain.Address -> TxBody era -> Maybe Chain.TxOutRef
findMarloweOutput address = \case
  body@(TxBody TxBodyContent{..}) ->
    fmap (Chain.TxOutRef (fromCardanoTxId $ getTxId body) . fst) $
      find (isToCurrentScriptAddress . snd) $
        zip [0 ..] txOuts
  where
    isToCurrentScriptAddress (TxOut address' _ _ _) =
      address == fromCardanoAddressInEra (cardanoEra @era) address'

findPayouts
  :: forall era v. (IsCardanoEra era) => MarloweVersion v -> Chain.Address -> TxBody era -> Map Chain.TxOutRef (Payout v)
findPayouts version address body@(TxBody TxBodyContent{..}) =
  Map.fromDistinctAscList $
    zipWith (\txIx payout -> (Chain.TxOutRef (fromCardanoTxId $ getTxId body) txIx, payout)) [0 ..] $
      mapMaybe parsePayout txOuts
  where
    parsePayout :: TxOut CtxTx era -> Maybe (Payout v)
    parsePayout (TxOut addr value datum _) = do
      guard $ fromCardanoAddressInEra (cardanoEra @era) addr == address
      datum' <- fromChainPayoutDatum version =<< snd (fromCardanoTxOutDatum datum)
      pure $ Payout address (fromCardanoTxOutValue value) datum'

execApplyInputs
  :: (MonadUnliftIO m, IsCardanoEra era)
  => CardanoEra era
  -> Connector (QueryClient ContractRequest) m
  -> STM Chain.ChainPoint
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
  -> m (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (InputsApplied v) m ())
execApplyInputs
  era
  contractQueryConnector
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
    referenceInputsSupported <- referenceInputsSupportedInEra (ApplyInputsEraUnsupported $ AnyCardanoEra era) era
    marloweContext@MarloweContext{..} <-
      withExceptT ApplyInputsLoadMarloweContextFailed $
        ExceptT $
          loadMarloweContext version contractId
    let getTipSlot =
          atomically $
            getTip >>= \case
              Chain.Genesis -> retry
              Chain.At Chain.BlockHeader{..} -> pure slotNo
    tipSlot <- liftIO getTipSlot
    scriptOutput'@TransactionScriptOutput{datum = inputDatum} <-
      except $ maybe (Left ScriptOutputNotFound) Right scriptOutput
    let (contract, state) = case version of
          MarloweV1 -> case inputDatum of
            V1.MarloweData{..} -> (marloweContract, marloweState)
        merkleizeInputs' = fmap hush . runConnector contractQueryConnector . merkleizeInputs contract state
    ((invalidBefore, invalidHereafter, mAssetsAndDatum, inputs'), constraints) <-
      buildApplyInputsConstraints
        merkleizeInputs'
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
    txBody <-
      except $
        first ApplyInputsConstraintError $
          solveConstraints referenceInputsSupported version marloweContext walletContext constraints
    let input = scriptOutput'
    let buildOutput (assets, datum) utxo = TransactionScriptOutput marloweAddress assets utxo datum
    let output =
          TransactionOutput
            { payouts = findPayouts version payoutAddress txBody
            , scriptOutput = buildOutput <$> mAssetsAndDatum <*> findMarloweOutput marloweAddress txBody
            }
    pure $
      InputsApplied referenceInputsSupported $
        InputsAppliedInEra
          { metadata = decodeMarloweTransactionMetadataLenient case txBody of
              TxBody TxBodyContent{..} -> case txMetadata of
                TxMetadataNone -> mempty
                TxMetadataInEra _ m -> fromCardanoTxMetadata m
          , inputs = inputs'
          , ..
          }

execWithdraw
  :: forall era v m
   . (Monad m, IsCardanoEra era)
  => CardanoEra era
  -> SolveConstraints
  -> LoadWalletContext m
  -> LoadMarloweContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> TokenName
  -> m (ServerStCmd MarloweTxCommand Void (WithdrawError v) (WithdrawTx v) m ())
execWithdraw era solveConstraints loadWalletContext loadMarloweContext version addresses contractId roleToken = execExceptT $ case version of
  MarloweV1 -> do
    referenceInputsSupported <- referenceInputsSupportedInEra (WithdrawEraUnsupported $ AnyCardanoEra era) era
    marloweContext@MarloweContext{payoutOutputs = Map.elems -> payouts} <-
      withExceptT WithdrawLoadMarloweContextFailed $
        ExceptT $
          loadMarloweContext version contractId
    let payoutAssetId Payout{datum = assetId} = assetId
        isRolePayout (Chain.AssetId _ roleName) = roleName == roleToken
        possibleDatum = find isRolePayout . map payoutAssetId $ payouts
    datum <- noteT (UnableToFindPayoutForAGivenRole roleToken) $ hoistMaybe possibleDatum
    constraints <- except $ buildWithdrawConstraints version datum
    walletContext <- lift $ loadWalletContext addresses
    txBody <-
      except $
        first WithdrawConstraintError $
          solveConstraints referenceInputsSupported version marloweContext walletContext constraints
    let inputs = getPayoutInputs marloweContext txBody
    pure $ WithdrawTx referenceInputsSupported $ WithdrawTxInEra{roleToken = datum, ..}
  where
    getPayoutInputs :: MarloweContext v -> TxBody era -> Map Chain.TxOutRef (Payout v)
    getPayoutInputs MarloweContext{..} (TxBody TxBodyContent{..}) = Map.restrictKeys payoutOutputs txIns'
      where
        txIns' = Set.fromList $ fromCardanoTxIn . fst <$> txIns

execSubmit
  :: (MonadUnliftIO m)
  => (Tx era -> STM (SubmitJob m))
  -> (TxId -> SubmitJob m -> STM ())
  -> Tx era
  -> m (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
execSubmit mkSubmitJob trackSubmitJob tx = do
  let txId = fromCardanoTxId $ getTxId $ getTxBody tx
  (submitJob, exVar) <- atomically do
    exVar <- newEmptyTMVar
    submitJob <- mkSubmitJob tx
    let getExceptionStatus = Failed . SubmitException <$> readTMVar exVar
    let submitJob' = submitJob{submitJobStatus = getExceptionStatus <|> submitJobStatus submitJob}
    trackSubmitJob txId submitJob'
    pure (submitJob', exVar)
  -- Run the job in a new thread
  _ <- forkFinally (runSubmitJob submitJob) \case
    Left ex -> atomically $ putTMVar exVar $ displayException ex
    _ -> pure ()
  -- Make a new server and run it in IO.
  submitJobServerCmd (JobIdSubmit txId) submitJob

submitJobServerAttach
  :: (MonadIO m)
  => JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> Maybe (SubmitJob m)
  -> m (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
submitJobServerAttach jobId =
  maybe
    (pure $ SendMsgAttachFailed ())
    (fmap SendMsgAttached . submitJobServerCmd jobId)

submitJobServerCmd
  :: (MonadIO m)
  => JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> SubmitJob m
  -> m (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
submitJobServerCmd jobId@(JobIdSubmit txId) submitJob = do
  jobStatus <- atomically $ submitJobStatus submitJob
  case jobStatus of
    Running status ->
      pure $
        SendMsgAwait
          status
          jobId
          ServerStAwait
            { recvMsgDetach = pure ()
            , recvMsgPoll = submitJobServerCmd jobId submitJob
            }
    Succeeded block -> pure $ SendMsgSucceed block ()
    Failed err -> pure $ SendMsgFail err ()
    Crashed e -> throwIO $ SubmitJobCrashedException txId e

data SubmitJobCrashedException = SubmitJobCrashedException TxId SomeException
  deriving (Show)
  deriving anyclass (Exception)

execExceptT
  :: (Functor m)
  => ExceptT e m a
  -> m (ServerStCmd cmd status e a n ())
execExceptT = fmap (either (flip SendMsgFail ()) (flip SendMsgSucceed ())) . runExceptT
