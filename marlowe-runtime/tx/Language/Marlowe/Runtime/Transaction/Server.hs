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
  ScriptDataSupportedInEra (..),
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
  toLedgerEpochInfo,
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
import Control.Error.Util (hush, note, noteT)
import Control.Exception (Exception (..), SomeException)
import Control.Monad (guard, unless, (<=<))
import Control.Monad.Event.Class
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Data.Bifunctor (first)
import Data.Foldable (foldl')
import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Map.NonEmpty as NEMap
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, nominalDiffTimeToSeconds)
import Data.Traversable (for)
import Data.Void (Void)
import Language.Marlowe.Analysis.Safety.Types (SafetyError (SafetyAnalysisTimeout))
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (
  fromCardanoAddressInEra,
  fromCardanoLovelace,
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
  Payout (Payout),
  TransactionOutput (..),
  TransactionScriptOutput (..),
  decodeMarloweTransactionMetadataLenient,
  fromChainPayoutDatum,
  withMarloweVersion,
 )
import Language.Marlowe.Runtime.Core.ScriptRegistry (HelperScript (..), MarloweScripts (..))
import qualified Language.Marlowe.Runtime.Plutus.V2.Api (fromPlutusCurrencySymbol)
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsError (..),
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
  RoleTokensConfig (..),
  SubmitError (..),
  SubmitStatus (..),
  WalletAddresses (..),
  WithdrawError (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
 )
import Language.Marlowe.Runtime.Transaction.BuildConstraints (
  MkRoleTokenMintingPolicy,
  buildApplyInputsConstraints,
  buildCreateConstraints,
  buildWithdrawConstraints,
  initialMarloweState,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (MarloweContext (..), SolveConstraints, TxConstraints)
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
  checkContract,
  checkTransactions,
  minAdaUpperBound,
  mkAdjustMinimumUtxo,
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
  , loadPayoutContext :: LoadPayoutContext m
  , loadHelpersContext :: LoadHelpersContext m
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
              addField ev $ SystemStart systemStart
              addField ev $ EraHistory eraHistory
              addField ev $ ProtocolParameters protocolParameters
              addField ev $ NetworkId networkId
              addField ev $ Era $ AnyCardanoEra era
              let solveConstraints :: Constraints.SolveConstraints
                  solveConstraints =
                    Constraints.solveConstraints
                      systemStart
                      (toLedgerEpochInfo eraHistory)
                      protocolParameters
              case command of
                Create mStakeCredential version addresses threadRole roles metadata minAda contract ->
                  withEvent ExecCreate \_ ->
                    execCreate
                      mkRoleTokenMintingPolicy
                      era
                      contractQueryConnector
                      getCurrentScripts
                      solveConstraints
                      protocolParameters
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
                        protocolParameters
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
                      solveConstraints
                      loadWalletContext
                      loadPayoutContext
                      loadHelpersContext
                      version
                      addresses
                      payouts
                Submit ReferenceTxInsScriptsInlineDatumsInBabbageEra tx ->
                  execSubmit (mkSubmitJob ScriptDataInBabbageEra) trackSubmitJob tx
                Submit ReferenceTxInsScriptsInlineDatumsInConwayEra tx ->
                  execSubmit (mkSubmitJob ScriptDataInConwayEra) trackSubmitJob tx
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
  => MkRoleTokenMintingPolicy m
  -> CardanoEra era
  -> Connector (QueryClient ContractRequest) m
  -> (MarloweVersion v -> MarloweScripts)
  -> SolveConstraints
  -> ProtocolParameters
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
  -> Either (Contract v) DatumHash
  -> NominalDiffTime
  -> m (ServerStCmd MarloweTxCommand Void CreateError (ContractCreated v) m ())
execCreate mkRoleTokenMintingPolicy era contractQueryConnector getCurrentScripts solveConstraints protocolParameters loadWalletContext loadHelpersContext networkId mStakeCredential version addresses threadRole roleTokens metadata optMinAda contract analysisTimeout = execExceptT do
  referenceInputsSupported <- referenceInputsSupportedInEra (CreateEraUnsupported $ AnyCardanoEra era) era
  let adjustMinUtxo = mkAdjustMinimumUtxo referenceInputsSupported protocolParameters version
  let threadRole' = fromMaybe "" threadRole
  walletContext <- lift $ loadWalletContext addresses
  (_, dummyState) <-
    except $
      initialMarloweState
        adjustMinUtxo
        version
        ( Chain.AssetId "00000000000000000000000000000000000000000000000000000000" threadRole' <$ guard case roleTokens of
            RoleTokensNone -> False
            RoleTokensMint (unMint -> mint) -> any (NEMap.member (ToScript OpenRoleScript) . roleTokenRecipients) mint
            RoleTokensUsePolicy _ distribution -> any (Map.member (ToScript OpenRoleScript)) distribution
        )
        (fromMaybe 0 optMinAda)
        walletContext
  (contract', continuations) <- case contract of
    Right hash -> case version of
      MarloweV1 -> noteT CreateContractNotFound do
        let getContract' = MaybeT . runConnector contractQueryConnector . getContract
        Contract.ContractWithAdjacency{contract = c, ..} <- getContract' hash
        (c :: Contract v,) <$> foldMapM (fmap singletonContinuations . getContract') (Set.delete hash closure)
    Left c -> pure (c, noContinuations version)
  mCardanoStakeCredential <- except $ traverse (note CreateToCardanoError . toCardanoStakeCredential) mStakeCredential
  computedMinAdaDeposit <-
    except $
      note ProtocolParamNoUTxOCostPerByte $
        fromCardanoLovelace
          <$> minAdaUpperBound referenceInputsSupported protocolParameters version dummyState contract' continuations
  let minAda = fromMaybe computedMinAdaDeposit optMinAda
  unless (minAda >= computedMinAdaDeposit) $ throwE $ InsufficientMinAdaDeposit computedMinAdaDeposit
  ((datum, assets, rolesCurrency), constraints) <-
    ExceptT $
      buildCreateConstraints
        mkRoleTokenMintingPolicy
        referenceInputsSupported
        version
        walletContext
        threadRole'
        roleTokens
        metadata
        minAda
        adjustMinUtxo
        contract'
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
        , marloweAddress
        , payoutAddress
        , marloweScriptUTxO
        , payoutScriptUTxO
        , marloweScriptHash = marloweScript
        , payoutScriptHash = payoutScript
        }
  helpersContext <-
    withExceptT CreateLoadHelpersContextFailed $
      ExceptT $
        loadHelpersContext version $
          Left (rolesCurrency, roleTokens)
  let -- Fast analysis of safety: examines bounds for transactions.
      V1.MarloweData{marloweState} = case version of
        MarloweV1 -> datum
      contractSafetyErrors = checkContract networkId (Just roleTokens) version contract' marloweState continuations
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
              helpersContext
              threadRole'
              (changeAddress addresses)
              adjustMinUtxo
              datum
              continuations
          )
  txBody <-
    except $
      first CreateConstraintError $
        solveConstraints referenceInputsSupported version (Left marloweContext) walletContext helpersContext constraints
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
  ConwayEra -> pure ReferenceTxInsScriptsInlineDatumsInConwayEra

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
  Map.fromDistinctAscList $ mapMaybe (uncurry parsePayout) $ zip [0 ..] txOuts
  where
    txId = fromCardanoTxId $ getTxId body
    parsePayout :: Chain.TxIx -> TxOut CtxTx era -> Maybe (Chain.TxOutRef, Payout v)
    parsePayout txIx (TxOut addr value datum _) = do
      guard $ fromCardanoAddressInEra (cardanoEra @era) addr == address
      datum' <- fromChainPayoutDatum version =<< snd (fromCardanoTxOutDatum datum)
      pure (Chain.TxOutRef txId txIx, Payout address (fromCardanoTxOutValue value) datum')

execApplyInputs
  :: (MonadUnliftIO m, IsCardanoEra era)
  => CardanoEra era
  -> Connector (QueryClient ContractRequest) m
  -> STM Chain.ChainPoint
  -> SystemStart
  -> EraHistory CardanoMode
  -> ProtocolParameters
  -> SolveConstraints
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
  contractQueryConnector
  getTip
  systemStart
  eraHistory
  protocolParamaters
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
    referenceInputsSupported <- referenceInputsSupportedInEra (ApplyInputsEraUnsupported $ AnyCardanoEra era) era
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
          solveConstraints referenceInputsSupported version (Left marloweContext) walletContext helpersContext constraints
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
        -- FIXME: This check will ignore any safety issue related to the minting policy and tokens.
        contractSafetyErrors = checkContract networkId Nothing version contract state continuations
        limitAnalysisTime =
          liftIO
            . fmap (either Right id)
            . race ([SafetyAnalysisTimeout] <$ threadDelay (floor $ nominalDiffTimeToSeconds analysisTimeout * 1_000_000))
    -- Slow analysis of safety: examines all possible transactions.
    transactionSafetyErrors <- case mAssetsAndDatum of
      Nothing -> pure []
      Just (_, datum) ->
        ExceptT $
          first CreateSafetyAnalysisError
            <$> limitAnalysisTime
              ( checkTransactions
                  protocolParameters
                  referenceInputsSupported
                  version
                  marloweContext
                  helpersContext
                  threadRole'
                  (changeAddress addresses)
                  adjustMinUtxo
                  datum
                  continuations
              )
    let safetyErrors = []

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

--     let
--       contractSafetyErrors = checkContract networkId roleTokens version contract continuations
--     safetyErrors <- case mAssetsAndDatum of
--       Nothing -> pure []
--       Just (assets, datum) -> do
--         let -- Fast analysis of safety: examines bounds for transactions.
--             adjustMinUtxo = mkAdjustMinimumUtxo referenceInputsSupported protocolParameters version
--             limitAnalysisTime =
--               liftIO
--                 . fmap (either Right id)
--                 . race ([SafetyAnalysisTimeout] <$ threadDelay (floor $ nominalDiffTimeToSeconds analysisTimeout * 1_000_000))
--             rolesCurrency' = fromPlutusCurrencySymbol rolesCurrency
--         -- Slow analysis of safety: examines all possible transactions.
--         ExceptT $
--           first CreateSafetyAnalysisError
--             <$> limitAnalysisTime
--               ( checkTransactions
--                   protocolParameters
--                   referenceInputsSupported
--                   version
--                   marloweContext
--                   helpersContext
--                   rolesCurrency'
--                   threadRole'
--                   (changeAddress addresses)
--                   assets
--                   adjustMinUtxo
--                   contract'
--                   continuations
--               )

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
  -> SolveConstraints
  -> LoadWalletContext m
  -> LoadPayoutContext m
  -> LoadHelpersContext m
  -> MarloweVersion v
  -> WalletAddresses
  -> Set Chain.TxOutRef
  -> m (ServerStCmd MarloweTxCommand Void WithdrawError (WithdrawTx v) m ())
execWithdraw era solveConstraints loadWalletContext loadPayoutContext loadHelpersContext version addresses payouts = execExceptT $ case version of
  MarloweV1 -> do
    referenceInputsSupported <- referenceInputsSupportedInEra (WithdrawEraUnsupported $ AnyCardanoEra era) era
    payoutContext <- lift $ loadPayoutContext version payouts
    (inputs, constraints) <- buildWithdrawConstraints payoutContext version payouts
    walletContext <- lift $ loadWalletContext addresses
    helpersContext <- withExceptT WithdrawLoadHelpersContextFailed $ ExceptT $ loadHelpersContext version $ Right Nothing
    txBody <-
      except $
        first WithdrawConstraintError $
          solveConstraints referenceInputsSupported version (Right payoutContext) walletContext helpersContext constraints
    pure $ WithdrawTx referenceInputsSupported $ WithdrawTxInEra{..}

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
