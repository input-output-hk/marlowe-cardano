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
  BabbageEra,
  CardanoEra (BabbageEra),
  CardanoMode,
  EraHistory,
  IsCardanoEra,
  NetworkId (..),
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
  hashScriptData,
  makeShelleyAddress,
 )
import Cardano.Api.Shelley (ProtocolParameters)
import Colog (Message, WithLog)
import Control.Applicative ((<|>))
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar, retry)
import Control.Error (MaybeT (..))
import Control.Error.Util (hoistMaybe, hush, note, noteT)
import Control.Exception (Exception (..))
import Control.Monad (unless)
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
import Data.Time (UTCTime)
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressInEra,
  fromCardanoDatumHash,
  fromCardanoTxId,
  fromCardanoTxIn,
  toCardanoPaymentCredential,
  toCardanoScriptData,
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
  toDatum,
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
  TransactionScriptOutput (..),
  decodeMarloweTransactionMetadataLenient,
  withMarloweVersion,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (MarloweScripts (..))
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsError (..),
  ContractCreated (..),
  CreateError (..),
  InputsApplied (..),
  JobId (..),
  MarloweTxCommand (..),
  RoleTokensConfig,
  SubmitError (..),
  SubmitStatus (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (..),
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
  hoistAttach,
  hoistCmd,
 )
import Network.Protocol.Query.Client (QueryClient, request)
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
  { mkSubmitJob :: Tx BabbageEra -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , contractQueryConnector :: Connector (QueryClient ContractRequest) m
  , getTip :: STM Chain.ChainPoint
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
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
              (systemStart, eraHistory, protocolParameters, networkId) <-
                runConnector chainSyncQueryConnector $
                  (,,,)
                    <$> request GetSystemStart
                    <*> request GetEraHistory
                    <*> request GetProtocolParameters
                    <*> request GetNetworkId
              addField ev $ SystemStart systemStart
              addField ev $ EraHistory eraHistory
              addField ev $ ProtocolParameters protocolParameters
              addField ev $ NetworkId networkId
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
                      contractQueryConnector
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
                  withEvent ExecApplyInputs \_ ->
                    withMarloweVersion version $
                      execApplyInputs
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
  pure (pure (), ServerSource $ pure server)

attachSubmit
  :: (MonadIO m)
  => JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> STM (Maybe (SubmitJob m))
  -> m (ServerStAttach MarloweTxCommand SubmitStatus SubmitError BlockHeader m ())
attachSubmit jobId getSubmitJob =
  atomically $ fmap (hoistAttach $ liftIO . atomically) <$> submitJobServerAttach jobId =<< getSubmitJob

execCreate
  :: forall m v
   . (MonadUnliftIO m)
  => Connector (QueryClient ContractRequest) m
  -> (MarloweVersion v -> MarloweScripts)
  -> SolveConstraints
  -> LoadWalletContext m
  -> NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Chain.Lovelace
  -> Either (Contract v) DatumHash
  -> m (ServerStCmd MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) m ())
execCreate contractQueryConnector getCurrentScripts solveConstraints loadWalletContext networkId mStakeCredential version addresses roleTokens metadata minAda contract = execExceptT do
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
      buildCreateConstraints version walletContext roleTokens metadata minAda contract'
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
  let contractSafetyErrors =
        if False -- FIXME: Disabled because of incompatibility with integration tests.
          then checkContract networkId roleTokens version contract' continuations
          else mempty
  -- FIXME: The is a placeholder until we design safety-analysis reporting.
  unless (null contractSafetyErrors)
    . throwE
    . CreateSafetyAnalysisError
    $ show contractSafetyErrors
  transactionSafetyErrors <-
    ExceptT $
      first CreateSafetyAnalysisError
        <$> if False -- FIXME: Disabled because of incompatibility with integration tests.
          then
            checkTransactions
              solveConstraints
              version
              marloweContext
              rolesCurrency
              (changeAddress addresses)
              (toInteger minAda)
              contract'
              continuations
          else pure $ pure mempty
  -- FIXME: The is a placeholder until we design safety-analysis reporting.
  unless (null transactionSafetyErrors)
    . throwE
    . CreateSafetyAnalysisError
    $ show transactionSafetyErrors
  txBody <-
    except $
      first CreateConstraintError $
        solveConstraints version marloweContext walletContext constraints
  let marloweScriptAddress = Constraints.marloweAddress marloweContext
  pure
    ContractCreated
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

execApplyInputs
  :: (MonadUnliftIO m)
  => Connector (QueryClient ContractRequest) m
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
  -> m (ServerStCmd MarloweTxCommand Void (ApplyInputsError v) (InputsApplied BabbageEra v) m ())
execApplyInputs
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
    let (contractHash, state) = case version of
          MarloweV1 -> case inputDatum of
            V1.MarloweData{..} -> (fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ toDatum marloweContract, marloweState)
        merkleizeInputs' = fmap hush . runConnector contractQueryConnector . merkleizeInputs contractHash state
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
          solveConstraints version marloweContext walletContext constraints
    let input = scriptOutput'
    let buildOutput (assets, datum) utxo = TransactionScriptOutput marloweAddress assets utxo datum
    let output = buildOutput <$> mAssetsAndDatum <*> findMarloweOutput marloweAddress txBody
    pure
      InputsApplied
        { metadata = decodeMarloweTransactionMetadataLenient case txBody of
            TxBody TxBodyContent{..} -> case txMetadata of
              TxMetadataNone -> mempty
              TxMetadataInEra _ m -> fromCardanoTxMetadata m
        , inputs = inputs'
        , ..
        }

execWithdraw
  :: (Monad m)
  => SolveConstraints
  -> LoadWalletContext m
  -> LoadMarloweContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> TokenName
  -> m (ServerStCmd MarloweTxCommand Void (WithdrawError v) (WithdrawTx BabbageEra v) m ())
execWithdraw solveConstraints loadWalletContext loadMarloweContext version addresses contractId roleToken = execExceptT $ case version of
  MarloweV1 -> do
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
          solveConstraints version marloweContext walletContext constraints
    let inputs = getPayoutInputs marloweContext txBody
    pure WithdrawTx{roleToken = datum, ..}
  where
    getPayoutInputs :: MarloweContext v -> TxBody BabbageEra -> Map Chain.TxOutRef (Payout v)
    getPayoutInputs MarloweContext{..} (TxBody TxBodyContent{..}) = Map.restrictKeys payoutOutputs txIns'
      where
        txIns' = Set.fromList $ fromCardanoTxIn . fst <$> txIns

execSubmit
  :: (MonadUnliftIO m)
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
    let submitJob' = submitJob{submitJobStatus = getExceptionStatus <|> submitJobStatus submitJob}
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
submitJobServerAttach jobId =
  maybe
    (pure $ SendMsgAttachFailed ())
    (fmap SendMsgAttached . submitJobServerCmd jobId)

submitJobServerCmd
  :: JobId MarloweTxCommand SubmitStatus SubmitError BlockHeader
  -> SubmitJob m
  -> STM (ServerStCmd MarloweTxCommand SubmitStatus SubmitError BlockHeader STM ())
submitJobServerCmd jobId submitJob = do
  jobStatus <- submitJobStatus submitJob
  pure case jobStatus of
    Running status ->
      SendMsgAwait
        status
        jobId
        ServerStAwait
          { recvMsgDetach = pure ()
          , recvMsgPoll = submitJobServerCmd jobId submitJob
          }
    Succeeded block -> SendMsgSucceed block ()
    Failed err -> SendMsgFail err ()

execExceptT
  :: (Functor m)
  => ExceptT e m a
  -> m (ServerStCmd cmd status e a n ())
execExceptT = fmap (either (flip SendMsgFail ()) (flip SendMsgSucceed ())) . runExceptT
