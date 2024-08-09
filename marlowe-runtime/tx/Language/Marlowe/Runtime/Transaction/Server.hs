{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Language.Marlowe.Runtime.Transaction.Server where

import Cardano.Api (
  AddressInEra (..),
  AddressTypeInEra (..),
  AlonzoEraOnwards (..),
  AnyCardanoEra (..),
  BabbageEraOnwards (..),
  CardanoEra (..),
  CtxTx,
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
  cardanoEraConstraints,
  getTxBody,
  getTxId,
  inEonForEra,
  inEonForEraMaybe,
  makeShelleyAddress,
  toLedgerEpochInfo,
 )
import qualified Cardano.Api as C
import Cardano.Api.Shelley (
  LedgerProtocolParameters,
  ProtocolParameters,
  convertToLedgerProtocolParameters,
 )
import qualified Cardano.Api.Shelley as C
import Colog (Message, WithLog, logDebug)
import Control.Applicative ((<|>))
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race)
import Control.Concurrent.Component
import Control.Concurrent.STM (STM, modifyTVar, newEmptyTMVar, newTVar, putTMVar, readTMVar, readTVar, retry)
import qualified Control.DeepSeq as DeepSeq
import Control.Error (MaybeT (..))
import Control.Error.Util (hush, note, noteT)
import Control.Exception (Exception (..), SomeException, catch)
import qualified Control.Exception as Exception
import Control.Monad (guard, unless, when, (<=<))
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import qualified Data.Aeson as A
import qualified Data.Aeson.Text as A
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Time (NominalDiffTime, UTCTime, nominalDiffTimeToSeconds)
import Data.Traversable (for)
import Data.Void (Void)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (SafetyAnalysisTimeout))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient, getRoleCurrencies)
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressInEra,
  fromCardanoTxId,
  fromCardanoTxOutDatum,
  fromCardanoTxOutValue,
  toCardanoPaymentCredential,
  toCardanoStakeCredential,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  BlockHeader,
  ChainSyncQuery (..),
  Credential (..),
  DatumHash (..),
  TxId (..),
  fromCardanoTxMetadata,
  mkTxOutAssets,
 )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Contract.Api (ContractRequest, getContract, merkleizeInputs)
import qualified Language.Marlowe.Runtime.Contract.Api as Contract
import Language.Marlowe.Runtime.Core.Api (
  Contract,
  ContractId (..),
  Inputs,
  IsMarloweVersion (..),
  MarloweTransactionMetadata,
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (Payout),
  TransactionOutput (..),
  TransactionScriptOutput (..),
  decodeMarloweTransactionMetadataLenient,
  fromChainPayoutDatum,
  withMarloweVersion,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..), MarloweScripts (..))
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsError (..),
  BurnRoleTokensError (..),
  BurnRoleTokensTx (BurnRoleTokensTx),
  ContractCreated (..),
  ContractCreatedInEra (..),
  CreateError (..),
  Destination (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  JobId (..),
  MarloweTxCommand (..),
  Mint (unMint),
  MintRole (roleTokenRecipients),
  RoleTokenFilter,
  RoleTokenFilter' (..),
  RoleTokensConfig (..),
  SubmitError (..),
  SubmitStatus (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
  optimizeRoleTokenFilter,
  roleTokenFilterToRoleCurrencyFilter,
  unAccounts,
 )
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  Accounts,
  MinAdaProvider (MinAdaProvider),
  MkRoleTokenMintingPolicy,
  RolesPolicyId (RolesPolicyId),
  buildApplyInputsConstraints,
  buildCreateConstraints,
  buildWithdrawConstraints,
  initialMarloweState,
  invalidAddressesError,
 )
import Language.Marlowe.Runtime.Transaction.Burn (burnRoleTokens)
import Language.Marlowe.Runtime.Transaction.Constraints (
  MarloweContext (..),
  SolveConstraints,
  TxConstraints,
  WalletContext (WalletContext),
 )

import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints
import Language.Marlowe.Runtime.Transaction.Query (
  LoadMarloweContext,
  LoadPayoutContext,
  LoadWalletContext,
  lookupMarloweScriptUtxo,
  lookupPayoutScriptUtxo,
 )
import Language.Marlowe.Runtime.Transaction.Query.Helper (LoadHelpersContext)
import Language.Marlowe.Runtime.Transaction.Safety (
  Continuations,
  ThreadTokenAssetId (..),
  checkContract,
  checkTransactions,
  minAdaUpperBound,
  mkAdjustMinUTxO,
  mkLockedRolesContext,
  mockLockedRolesContext,
  noContinuations,
 )
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
import qualified PlutusLedgerApi.V2 as PV2
import UnliftIO (MonadUnliftIO, atomically, throwIO)
import UnliftIO.Concurrent (forkFinally)
import Witherable (mapMaybe)

data TransactionServerSelector f where
  Exec :: TransactionServerSelector ExecField
  ExecCreate :: TransactionServerSelector BuildTxField
  ExecApplyInputs :: TransactionServerSelector BuildTxField
  ExecWithdraw :: TransactionServerSelector BuildTxField
  ExecBurnRoleTokens :: TransactionServerSelector BuildTxField

data ExecField
  = SystemStart SystemStart
  | EraHistory EraHistory
  | ProtocolParameters ProtocolParameters
  | NetworkId NetworkId
  | Era AnyCardanoEra

data BuildTxField where
  Constraints :: (IsCardanoEra era) => MarloweVersion v -> TxConstraints era v -> BuildTxField
  ResultingTxBody :: (IsCardanoEra era) => TxBody era -> BuildTxField

data TransactionServerDependencies m = TransactionServerDependencies
  { mkSubmitJob :: forall era. AlonzoEraOnwards era -> Tx era -> STM (SubmitJob m)
  , loadWalletContext :: LoadWalletContext m
  , loadMarloweContext :: LoadMarloweContext m
  , loadPayoutContext :: LoadPayoutContext m
  , loadHelpersContext :: LoadHelpersContext m
  , marloweQueryConnector :: Connector MarloweQueryClient m
  , chainSyncQueryConnector :: Connector (QueryClient ChainSyncQuery) m
  , contractQueryConnector :: Connector (QueryClient ContractRequest) m
  , getTip :: STM Chain.ChainPoint
  , getCurrentScripts :: forall v. MarloweVersion v -> MarloweScripts
  , analysisTimeout :: NominalDiffTime
  , mkRoleTokenMintingPolicy :: MkRoleTokenMintingPolicy m
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
              shelleyEra <- case inEonForEraMaybe id era of
                Nothing -> error "Current era not supported"
                Just a -> pure a

              addField ev $ SystemStart systemStart
              addField ev $ EraHistory eraHistory
              addField ev $ ProtocolParameters protocolParameters
              addField ev $ NetworkId networkId
              addField ev $ Era $ AnyCardanoEra era

              logDebug $
                "Server received a command: " <> case command of
                  Create{} -> "Create"
                  ApplyInputs{} -> "ApplyInputs"
                  Withdraw{} -> "Withdraw"
                  BurnRoleTokens{} -> "BurnRoleTokens"
                  Submit{} -> "Submit"

              let solveConstraints :: Constraints.SolveConstraints era v
                  solveConstraints =
                    Constraints.solveConstraints
                      systemStart
                      (toLedgerEpochInfo eraHistory)
              ledgerProtocolParameters <- case convertToLedgerProtocolParameters shelleyEra protocolParameters of
                Left e -> error $ "Failed to convert protocol params: " <> show e
                Right a -> pure a
              cardanoEraConstraints era case command of
                Create mStakeCredential version addresses threadRole roles metadata minAda accounts contract ->
                  withEvent ExecCreate \_ ->
                    execCreate
                      mkRoleTokenMintingPolicy
                      era
                      contractQueryConnector
                      getCurrentScripts
                      solveConstraints
                      ledgerProtocolParameters
                      loadWalletContext
                      loadHelpersContext
                      networkId
                      mStakeCredential
                      version
                      addresses
                      threadRole
                      roles
                      metadata
                      minAda
                      accounts
                      contract
                      analysisTimeout
                ApplyInputs version addresses contractId metadata invalidBefore invalidHereafter inputs ->
                  withEvent ExecApplyInputs \_ ->
                    withMarloweVersion version $
                      execApplyInputs
                        era
                        ledgerProtocolParameters
                        contractQueryConnector
                        getTip
                        systemStart
                        eraHistory
                        solveConstraints
                        loadWalletContext
                        loadMarloweContext
                        loadHelpersContext
                        networkId
                        version
                        addresses
                        contractId
                        metadata
                        invalidBefore
                        invalidHereafter
                        inputs
                        analysisTimeout
                Withdraw version addresses payouts ->
                  withEvent ExecWithdraw \_ ->
                    execWithdraw
                      era
                      ledgerProtocolParameters
                      solveConstraints
                      loadWalletContext
                      loadPayoutContext
                      loadHelpersContext
                      version
                      addresses
                      payouts
                BurnRoleTokens version addresses tokenFilter ->
                  withEvent ExecBurnRoleTokens \_ ->
                    execBurn
                      systemStart
                      eraHistory
                      chainSyncQueryConnector
                      marloweQueryConnector
                      era
                      ledgerProtocolParameters
                      loadWalletContext
                      version
                      addresses
                      tokenFilter
                Submit BabbageEraOnwardsBabbage tx ->
                  execSubmit (mkSubmitJob AlonzoEraOnwardsBabbage) trackSubmitJob tx
                Submit BabbageEraOnwardsConway tx ->
                  execSubmit (mkSubmitJob AlonzoEraOnwardsConway) trackSubmitJob tx
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

limitAnalysisTime :: NominalDiffTime -> IO (Either String [SafetyError]) -> IO (Either String [SafetyError])
limitAnalysisTime timeout analysis = do
  let analysis' = runExceptT do
        let go = do
              errs <- analysis >>= (Exception.evaluate . DeepSeq.force)
              pure $ Right errs
        res <-
          ExceptT $
            go `catch` \(e :: SomeException) -> do
              pure $ Left (show e)
        except res
  res <-
    race
      (threadDelay (floor $ nominalDiffTimeToSeconds timeout * 1_000_000))
      analysis'
  case res of
    -- Timeout reached
    Left () -> pure $ Right [SafetyAnalysisTimeout]
    -- Analysis finished with exception or internal error
    Right (Left e) -> pure $ Left e
    -- Analysis finished successfully
    Right (Right res') -> pure $ Right res'

type GetCurrentScripts v = MarloweVersion v -> MarloweScripts

mkMarloweContext
  :: (MonadUnliftIO m)
  => NetworkId
  -> MarloweVersion v
  -> GetCurrentScripts v
  -> Maybe Chain.StakeCredential
  -> ExceptT CreateError m (MarloweContext v)
mkMarloweContext networkId version getCurrentScripts mStakeCredential = do
  mCardanoStakeCredential <- except $ traverse (note CreateToCardanoError . toCardanoStakeCredential) mStakeCredential
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
  except $ first CreateLoadMarloweContextFailed do
    marloweScriptUTxO <- lookupMarloweScriptUtxo networkId scripts
    payoutScriptUTxO <- lookupPayoutScriptUtxo networkId scripts
    pure
      MarloweContext
        { scriptOutput = Nothing
        , marloweAddress
        , payoutAddress
        , marloweScriptUTxO
        , payoutScriptUTxO
        , marloweScriptHash = marloweScript
        , payoutScriptHash = payoutScript
        }

logSolveConstraintsParams
  :: (WithLog env Message m)
  => C.BabbageEraOnwards era
  -> C.LedgerProtocolParameters era
  -> MarloweVersion v
  -> Either (MarloweContext v) Constraints.PayoutContext
  -> WalletContext
  -> Constraints.HelpersContext
  -> TxConstraints era v
  -> m ()
logSolveConstraintsParams era protocol version scriptCtx walletCtx helpersCtx constraints = case version of
  MarloweV1 -> do
    let shelleyBasedEra = C.babbageEraOnwardsToShelleyBasedEra era
        protocolParams = C.fromLedgerPParams shelleyBasedEra $ C.unLedgerProtocolParameters protocol

        entry =
          A.object
            [ ("era", A.String . T.pack . show $ era)
            , ("protocol", A.toJSON protocolParams)
            , ("version", A.toJSON version)
            , case scriptCtx of
                Left marloweCtx -> ("marloweCtx", A.toJSON marloweCtx)
                Right payoutCtx -> ("payoutCtx", A.toJSON payoutCtx)
            , ("walletCtx", A.toJSON walletCtx)
            , ("helpersCtx", A.toJSON helpersCtx)
            , ("constraints", A.toJSON constraints)
            ]
    logDebug . TL.toStrict . A.encodeToLazyText $ entry
    logDebug . T.pack $ show (era, protocolParams, version, scriptCtx, walletCtx, helpersCtx, constraints)

execCreate
  :: forall env era m v
   . (MonadUnliftIO m, IsCardanoEra era, WithLog env Message m)
  => MkRoleTokenMintingPolicy m
  -> CardanoEra era
  -> Connector (QueryClient ContractRequest) m
  -> GetCurrentScripts v
  -> SolveConstraints era v
  -> LedgerProtocolParameters era
  -> LoadWalletContext m
  -> LoadHelpersContext m
  -> NetworkId
  -> Maybe Chain.StakeCredential
  -> MarloweVersion v
  -> WalletAddresses
  -> Maybe Chain.TokenName
  -> RoleTokensConfig
  -> MarloweTransactionMetadata
  -> Maybe Chain.Lovelace
  -> Accounts
  -> Either (Contract v) DatumHash
  -> NominalDiffTime
  -> m (ServerStCmd MarloweTxCommand Void CreateError (ContractCreated v) m ())
execCreate mkRoleTokenMintingPolicy era contractQueryConnector getCurrentScripts solveConstraints protocolParameters loadWalletContext loadHelpersContext networkId mStakeCredential version addresses threadRole roleTokens metadata optMinAda accounts contract analysisTimeout = execExceptT do
  eon <- toBabbageEraOnwards (CreateEraUnsupported $ AnyCardanoEra era) era
  let threadRole' = fromMaybe "" threadRole
  let adjustMinUtxo = mkAdjustMinUTxO eon protocolParameters version
  walletContext <- lift $ loadWalletContext addresses
  dummyState <- do
    let WalletContext{changeAddress} = walletContext
    except $
      first invalidAddressesError $
        initialMarloweState
          adjustMinUtxo
          version
          accounts
          ( ThreadTokenAssetId (Chain.AssetId "00000000000000000000000000000000000000000000000000000000" threadRole') <$ guard case roleTokens of
              RoleTokensNone -> False
              RoleTokensMint (unMint -> mint) -> any (NEMap.member (ToScript OpenRoleScript) . roleTokenRecipients) mint
              RoleTokensUsePolicy _ distribution -> any (Map.member (ToScript OpenRoleScript)) distribution
          )
          (fromMaybe mempty optMinAda)
          (MinAdaProvider changeAddress)
  (contract', continuations) <- case contract of
    Right hash -> case version of
      MarloweV1 -> noteT CreateContractNotFound do
        let getContract' = MaybeT . runConnector contractQueryConnector . getContract
        Contract.ContractWithAdjacency{contract = c, ..} <- getContract' hash
        (c :: Contract v,) <$> foldMapM (fmap singletonContinuations . getContract') (Set.delete hash closure)
    Left c -> pure (c, noContinuations version)
  computedMinAdaDeposit <-
    except $
      note ProtocolParamNoUTxOCostPerByte $
        minAdaUpperBound eon protocolParameters version dummyState contract' continuations
  let minAda = fromMaybe computedMinAdaDeposit optMinAda
  unless (minAda >= computedMinAdaDeposit) $ throwE $ InsufficientMinAdaDeposit computedMinAdaDeposit
  ((datum, assets, RolesPolicyId rolesCurrency), constraints) <-
    ExceptT $ do
      buildCreateConstraints
        mkRoleTokenMintingPolicy
        eon
        version
        walletContext
        threadRole'
        roleTokens
        metadata
        minAda
        accounts
        adjustMinUtxo
        contract'
  marloweContext@MarloweContext{marloweAddress} <-
    mkMarloweContext
      networkId
      version
      getCurrentScripts
      mStakeCredential
  helpersContext <-
    withExceptT CreateLoadHelpersContextFailed $
      ExceptT $
        loadHelpersContext version $
          Left (rolesCurrency, roleTokens)
  let -- Fast analysis of safety: examines bounds for transactions.
      contractSafetyErrors = checkContract networkId (Just roleTokens) version datum continuations
  -- Slow analysis of safety: examines all possible transactions.
  transactionSafetyErrors <- do
    let threadTokenAssetId = ThreadTokenAssetId (Chain.AssetId rolesCurrency threadRole')
        lockedRolesContext = mockLockedRolesContext threadTokenAssetId adjustMinUtxo helpersContext
    ExceptT $
      liftIO $
        fmap (first CreateSafetyAnalysisError) . limitAnalysisTime analysisTimeout $
          checkTransactions
            protocolParameters
            eon
            version
            marloweContext
            lockedRolesContext
            (changeAddress addresses)
            datum
            continuations

  let safetyErrors = contractSafetyErrors <> transactionSafetyErrors
  unless (Map.null (unAccounts accounts) || null safetyErrors) do
    throwE $ CreateSafetyAnalysisFailed safetyErrors

  txBody <-
    except $
      first CreateConstraintError $
        solveConstraints eon protocolParameters version (Left marloweContext) walletContext helpersContext constraints

  let marloweScriptAddress = Constraints.marloweAddress marloweContext
  pure $
    ContractCreated eon $
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
        , safetyErrors
        }

toBabbageEraOnwards
  :: (Monad m) => e -> CardanoEra era -> ExceptT e m (BabbageEraOnwards era)
toBabbageEraOnwards e = inEonForEra (throwE e) pure

singletonContinuations :: Contract.ContractWithAdjacency -> Continuations 'V1
singletonContinuations Contract.ContractWithAdjacency{..} = Map.singleton contractHash contract

foldMapM :: (Applicative m, Monoid b, Foldable t) => (a -> m b) -> t a -> m b
foldMapM f = foldl' (\b a -> mappend <$> b <*> f a) (pure mempty)

findMarloweOutput :: forall era. (IsCardanoEra era) => Chain.Address -> TxBody era -> Maybe Chain.TxOutRef
findMarloweOutput address = \case
  body@(TxBody TxBodyContent{..}) ->
    fmap (Chain.TxOutRef (fromCardanoTxId $ getTxId body) . fst) $
      find (isToCurrentScriptAddress . snd) $
        zip [minBound ..] txOuts
  where
    isToCurrentScriptAddress (TxOut address' _ _ _) =
      address == fromCardanoAddressInEra (cardanoEra @era) address'

findPayouts
  :: forall era v. (IsCardanoEra era) => MarloweVersion v -> Chain.Address -> TxBody era -> Map Chain.TxOutRef (Payout v)
findPayouts version address body@(TxBody TxBodyContent{..}) =
  Map.fromDistinctAscList $ mapMaybe (uncurry parsePayout) $ zip [minBound ..] txOuts
  where
    txId = fromCardanoTxId $ getTxId body
    parsePayout :: Chain.TxIx -> TxOut CtxTx era -> Maybe (Chain.TxOutRef, Payout v)
    parsePayout txIx (TxOut addr value datum _) = do
      guard $ fromCardanoAddressInEra (cardanoEra @era) addr == address
      datum' <- fromChainPayoutDatum version =<< snd (fromCardanoTxOutDatum datum)
      assets <- mkTxOutAssets $ fromCardanoTxOutValue value
      pure (Chain.TxOutRef txId txIx, Payout address assets datum')

execApplyInputs
  :: (MonadUnliftIO m, IsCardanoEra era, WithLog env Message m)
  => CardanoEra era
  -> LedgerProtocolParameters era
  -> Connector (QueryClient ContractRequest) m
  -> STM Chain.ChainPoint
  -> SystemStart
  -> EraHistory
  -> SolveConstraints era v
  -> LoadWalletContext m
  -> LoadMarloweContext m
  -> LoadHelpersContext m
  -> NetworkId
  -> MarloweVersion v
  -> WalletAddresses
  -> ContractId
  -> MarloweTransactionMetadata
  -> Maybe UTCTime
  -> Maybe UTCTime
  -> Inputs v
  -> NominalDiffTime
  -> m (ServerStCmd MarloweTxCommand Void ApplyInputsError (InputsApplied v) m ())
execApplyInputs
  era
  protocolParameters
  contractQueryConnector
  getTip
  systemStart
  eraHistory
  solveConstraints
  loadWalletContext
  loadMarloweContext
  loadHelpersContext
  networkId
  version@MarloweV1
  addresses
  contractId
  metadata
  invalidBefore'
  invalidHereafter'
  inputs
  analysisTimeout = execExceptT do
    eon <- toBabbageEraOnwards (ApplyInputsEraUnsupported $ AnyCardanoEra era) era
    marloweContext@MarloweContext{..} <-
      withExceptT ApplyInputsLoadMarloweContextFailed $
        ExceptT $
          loadMarloweContext version contractId
    helpersContext <-
      withExceptT ApplyInputsLoadHelpersContextFailed $ ExceptT $ loadHelpersContext version $ Right $ Just contractId
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
            V1.MarloweData{..} -> do
              (marloweContract, marloweState)
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
          solveConstraints eon protocolParameters version (Left marloweContext) walletContext helpersContext constraints

    let input = scriptOutput'
    let buildOutput (assets, datum) utxo = TransactionScriptOutput marloweAddress assets utxo datum
    let output =
          TransactionOutput
            { payouts = findPayouts version payoutAddress txBody
            , scriptOutput = buildOutput <$> mAssetsAndDatum <*> findMarloweOutput marloweAddress txBody
            }

    continuations <-
      lift (getContractContinuations contractQueryConnector contract) >>= \case
        Nothing -> throwE ApplyInputsContractContinuationNotFound
        Just c -> pure c

    let -- Fast analysis of safety: examines bounds for transactions.
    -- FIXME: We should verify minting policy here as well:
    --  * we should check if trusted minting policy was used
    --  * we should check the role where minted as NFTs or they were redundant (do we check this in creation?)
    -- Slow analysis of safety: examines all possible transactions.
    safetyErrors <- case mAssetsAndDatum of
      Nothing -> pure []
      Just (_, datum) -> do
        let lockedRolesContext = mkLockedRolesContext helpersContext
            contractSafetyErrors = checkContract networkId Nothing version datum continuations
        transactionSafetyErrors <-
          ExceptT $
            liftIO $
              fmap (first ApplyInputsSafetyAnalysisError) . limitAnalysisTime analysisTimeout $
                checkTransactions
                  protocolParameters
                  eon
                  version
                  marloweContext
                  lockedRolesContext
                  (changeAddress addresses)
                  datum
                  continuations

        pure $ contractSafetyErrors <> transactionSafetyErrors
    pure $
      InputsApplied eon $
        InputsAppliedInEra
          { metadata = decodeMarloweTransactionMetadataLenient case txBody of
              TxBody TxBodyContent{..} -> case txMetadata of
                TxMetadataNone -> mempty
                TxMetadataInEra _ m -> fromCardanoTxMetadata m
          , inputs = inputs'
          , safetyErrors
          , ..
          }

-- | Build up continuations closure map for a contract.
-- We don't want to just compute the root hash of the contract and ask the store for the closure because
-- we can have more granular merkleization in the future which sometimes does not merkleize every step
-- in the contract. In other words the root hash could be missing from the store.
getContractContinuations
  :: (Monad m)
  => Connector (QueryClient ContractRequest) m
  -> V1.Contract
  -> m (Maybe (Map DatumHash V1.Contract))
getContractContinuations contractQueryConnector contract = runMaybeT do
  let getCaseContinuationHashes (V1.MerkleizedCase _ h) = [h]
      getCaseContinuationHashes (V1.Case _ continuation) = getContractContinuationHashes continuation

      getContractContinuationHashes (V1.When cases _ continuation) =
        foldMap getCaseContinuationHashes cases <> getContractContinuationHashes continuation
      getContractContinuationHashes (V1.If _ trueContinuation falseContinuation) =
        getContractContinuationHashes trueContinuation <> getContractContinuationHashes falseContinuation
      getContractContinuationHashes (V1.Pay _ _ _ _ continuation) = getContractContinuationHashes continuation
      getContractContinuationHashes (V1.Let _ _ continuation) = getContractContinuationHashes continuation
      getContractContinuationHashes V1.Close = []
      getContractContinuationHashes (V1.Assert _ continuation) = getContractContinuationHashes continuation

      toDatumHash = DatumHash . PV2.fromBuiltin

      childrenHashes :: Set DatumHash
      childrenHashes = Set.fromList . fmap toDatumHash $ getContractContinuationHashes contract

      getContract' = MaybeT . runConnector contractQueryConnector . getContract

  childContracts :: [Contract.ContractWithAdjacency] <- for (Set.toList childrenHashes) getContract'
  let childrenClosure = flip foldMap childContracts \Contract.ContractWithAdjacency{closure} -> closure

  (closureContracts :: [Contract.ContractWithAdjacency]) <- do
    let hs = Set.toList $ Set.difference childrenClosure childrenHashes
    for hs getContract'
  let allContracts = childContracts <> closureContracts
      continuations = Map.fromList $ flip fmap allContracts \Contract.ContractWithAdjacency{contract = c, contractHash = ch} -> (ch, c)
  pure continuations

execWithdraw
  :: forall era v m
   . (Monad m, IsCardanoEra era)
  => CardanoEra era
  -> LedgerProtocolParameters era
  -> SolveConstraints era v
  -> LoadWalletContext m
  -> LoadPayoutContext m
  -> LoadHelpersContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> Set Chain.TxOutRef
  -> m (ServerStCmd MarloweTxCommand Void WithdrawError (WithdrawTx v) m ())
execWithdraw era protocolParameters solveConstraints loadWalletContext loadPayoutContext loadHelpersContext version addresses payouts = execExceptT $ case version of
  MarloweV1 -> do
    eon <- toBabbageEraOnwards (WithdrawEraUnsupported $ AnyCardanoEra era) era
    payoutContext <- lift $ loadPayoutContext version payouts
    (inputs, constraints) <- buildWithdrawConstraints payoutContext version payouts
    walletContext <- lift $ loadWalletContext addresses
    helpersContext <- withExceptT WithdrawLoadHelpersContextFailed $ ExceptT $ loadHelpersContext version $ Right Nothing
    txBody <-
      except $
        first WithdrawConstraintError $
          solveConstraints eon protocolParameters version (Right payoutContext) walletContext helpersContext constraints
    pure $ WithdrawTx eon $ WithdrawTxInEra{..}

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

execBurn
  :: forall era v m
   . (MonadUnliftIO m, IsCardanoEra era)
  => SystemStart
  -> EraHistory
  -> Connector (QueryClient ChainSyncQuery) m
  -> Connector MarloweQueryClient m
  -> CardanoEra era
  -> LedgerProtocolParameters era
  -> LoadWalletContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> RoleTokenFilter
  -> m (ServerStCmd MarloweTxCommand Void BurnRoleTokensError (BurnRoleTokensTx v) m ())
execBurn start history chainQueryConnector marloweQueryConnector era protocol loadWalletContext version addresses tokenFilter = execExceptT do
  eon <- toBabbageEraOnwards (BurnEraUnsupported $ AnyCardanoEra era) era
  let tokenFilter' = optimizeRoleTokenFilter tokenFilter
  when (tokenFilter' == RoleTokenFilterNone) $ throwE BurnNoTokens
  walletContext <- lift $ loadWalletContext addresses
  currencies <-
    lift $ runConnector marloweQueryConnector $ getRoleCurrencies $ roleTokenFilterToRoleCurrencyFilter tokenFilter'
  burnTx <- burnRoleTokens start history chainQueryConnector eon protocol version walletContext currencies tokenFilter'
  pure $ BurnRoleTokensTx eon burnTx
