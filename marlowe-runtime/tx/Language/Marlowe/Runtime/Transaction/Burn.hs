{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-record-wildcards #-}

module Language.Marlowe.Runtime.Transaction.Burn where

import Cardano.Api (BabbageEraOnwards, BuildTx, ScriptInEra, SystemStart, TxBodyContent (..), defaultTxBodyContent)
import qualified Cardano.Api as C
import Cardano.Api.Shelley (LedgerProtocolParameters, PlutusScriptOrReferenceInput (..), ReferenceScript (..))
import Control.Error (ExceptT, note, throwE)
import Control.Monad (unless, when)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (except)
import Data.Bifunctor (Bifunctor (..))
import Data.Coerce (coerce)
import Data.Foldable (Foldable (..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Language.Marlowe.Protocol.Query.Types (RoleCurrency (..))
import Language.Marlowe.Runtime.Cardano.Api (
  toCardanoAddressInEra,
  toCardanoLovelace,
  toCardanoPolicyId,
  toCardanoTxIn,
  toCardanoTxOutValue,
  tokensToCardanoValue,
 )
import Language.Marlowe.Runtime.ChainSync.Api (
  Address,
  AssetId (..),
  Assets (..),
  ChainSyncQuery (..),
  PolicyId (..),
  ScriptHash (..),
  Tokens (..),
  TransactionOutput (..),
  TxOutRef,
  UTxOs (..),
 )
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..))
import Language.Marlowe.Runtime.Transaction.Api (
  BurnError (..),
  BurnTxInEra (..),
  RoleTokenFilter,
  evalRoleTokenFilter,
 )
import Language.Marlowe.Runtime.Transaction.Constraints (
  HelpersContext (..),
  PayoutContext (PayoutContext),
  WalletContext (..),
  adjustTxForMinUtxo,
  balanceTx,
  selectCoins,
 )
import Network.Protocol.Connection (Connector, runConnector)
import Network.Protocol.Query.Client (QueryClient, request)
import UnliftIO (MonadUnliftIO)

burnRoleTokens
  :: (MonadUnliftIO m)
  => SystemStart
  -> C.EraHistory
  -> Connector (QueryClient ChainSyncQuery) m
  -> BabbageEraOnwards era
  -> LedgerProtocolParameters era
  -> WalletContext
  -> Set RoleCurrency
  -> RoleTokenFilter
  -> ExceptT BurnError m (BurnTxInEra era)
burnRoleTokens start history chainQueryConnector era protocol walletCtx@WalletContext{..} currencies tokenFilter = do
  -- convert role currency info into a list
  let currenciesList = Set.toList currencies
  -- collect the policy IDs which are used by active contracts
  let activeCurrencies = Set.fromList $ mapMaybe activeCurrency currenciesList
  -- define a mapping of policyId to contracts which use them for role tokens.
  let contractIdsByPolicyId = Map.fromListWith (<>) do
        RoleCurrency{..} <- currenciesList
        pure (rolePolicyId, Set.singleton roleContract)
  -- Splits assets into ones which match the burn filter, and ones that don't.
  let partitionAssets :: Map AssetId a -> (Map AssetId a, Map AssetId a)
      partitionAssets = Map.partitionWithKey \token _ ->
        any (flip (evalRoleTokenFilter tokenFilter) token) $
          fold $
            Map.lookup (policyId token) contractIdsByPolicyId
  -- Processes a single output from the wallet's UTxO.
  let processInput
        :: TxOutRef
        -> TransactionOutput
        -> ( Map TxOutRef (Tokens, (Address, Assets))
           , Set PolicyId
           )
      processInput txIn TransactionOutput{address, assets = assets@(Assets lovelace (Tokens tokens))} =
        case partitionAssets tokens of
          (toBurn, toKeep)
            | Map.null toBurn -> mempty
            | otherwise ->
                ( Map.singleton txIn (Tokens toBurn, (address, Assets lovelace $ Tokens toKeep))
                , Set.intersection activeCurrencies $ Set.map policyId $ Map.keysSet toBurn
                )
  -- Fold over the wallet's UTxO, selecting outputs to use as transaction inputs and looking for any
  -- matching currencies which are still active.
  let (inputs, activeOwnedCurrencies) = Map.foldMapWithKey processInput $ unUTxOs availableUtxos
  -- If the burn includes active role tokens, abort
  unless (Set.null activeOwnedCurrencies) $ throwE $ BurnRolesActive activeOwnedCurrencies
  -- If the burn is empty, abort
  when (Map.null inputs) $ throwE BurnNoTokens
  -- Fetch all the minting scripts needed to burn the tokens.
  let policyScriptHashes = foldMap (scriptHashesFromTokens . fst) inputs
  scripts <- lift $ runConnector chainQueryConnector $ request $ GetScripts era policyScriptHashes
  -- If there are policies for which scripts can't be found, abort.
  let missingScriptHashes = Set.difference policyScriptHashes $ Map.keysSet scripts
  unless (Set.null missingScriptHashes) $ throwE $ BurnRolesActive $ Set.mapMonotonic coerce missingScriptHashes
  -- Build the transaction body
  txBodyContent <- except $ note BurnFromCardanoError $ buildBurn era inputs scripts
  -- FIXME there is no reason we need these except that selectCoins and balanceTx require them. Refactor
  -- those two functions to remove these dummy contexts.
  let scriptCtx = Right $ PayoutContext mempty mempty
  let helpersCtx =
        HelpersContext
          { currentHelperScripts = mempty
          , helperPolicyId = ""
          , helperScriptStates = mempty
          }
  txBody <-
    except $
      first BurnConstraintError $
        adjustTxForMinUtxo era protocol Nothing txBodyContent
          >>= selectCoins era protocol MarloweV1 scriptCtx walletCtx helpersCtx
          >>= balanceTx era start (C.toLedgerEpochInfo history) protocol MarloweV1 scriptCtx walletCtx helpersCtx
  let burnedTokens = foldMap fst inputs
  pure BurnTxInEra{..}

scriptHashesFromTokens :: Tokens -> Set ScriptHash
scriptHashesFromTokens = Set.map (ScriptHash . unPolicyId . policyId) . Map.keysSet . unTokens

assetsFromUtxos :: UTxOs -> Assets
assetsFromUtxos = foldMap assets . unUTxOs

activeCurrency :: RoleCurrency -> Maybe PolicyId
activeCurrency RoleCurrency{..}
  | active = Just rolePolicyId
  | otherwise = Nothing

buildBurn
  :: forall era
   . BabbageEraOnwards era
  -> Map TxOutRef (Tokens, (Address, Assets))
  -> Map ScriptHash (ScriptInEra era)
  -> Maybe (TxBodyContent BuildTx era)
buildBurn era inputs scripts = do
  txIns <- traverse buildInput $ Map.keys inputs
  (outputsWithTokens, adaOnlyOutputs) <- fold <$> traverse (uncurry buildOutput . snd) inputs
  let txOuts = mergeAdaOnly adaOnlyOutputs <> outputsWithTokens
  txMintValue <- buildMint
  pure (defaultTxBodyContent shelleyEra){txIns, txOuts, txMintValue}
  where
    shelleyEra :: C.ShelleyBasedEra era
    shelleyEra = C.babbageEraOnwardsToShelleyBasedEra era

    maryEraOnwards :: C.MaryEraOnwards era
    maryEraOnwards = C.babbageEraOnwardsToMaryEraOnwards era

    buildInput :: TxOutRef -> Maybe (C.TxIn, C.BuildTxWith BuildTx (C.Witness C.WitCtxTxIn era))
    buildInput = fmap (,C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) . toCardanoTxIn

    buildOutput :: Address -> Assets -> Maybe ([C.TxOut C.CtxTx era], [(C.AddressInEra era, C.Lovelace)])
    buildOutput address assets@(Assets lovelace (Tokens tokens)) = do
      address' <- toCardanoAddressInEra (C.babbageEraOnwardsToCardanoEra era) address
      let lovelace' = toCardanoLovelace lovelace
      if Map.null tokens
        then pure ([], [(address', lovelace')])
        else do
          value <- toCardanoTxOutValue maryEraOnwards assets
          pure ([C.TxOut address' value C.TxOutDatumNone ReferenceScriptNone], [])

    mergeAdaOnly :: [(C.AddressInEra era, C.Lovelace)] -> [C.TxOut C.CtxTx era]
    mergeAdaOnly = fmap (uncurry buildAdaOnlyOutput) . Map.toList . Map.fromListWith (<>)

    buildAdaOnlyOutput :: C.AddressInEra era -> C.Lovelace -> C.TxOut C.CtxTx era
    buildAdaOnlyOutput address lovelace =
      C.TxOut address (C.lovelaceToTxOutValue shelleyEra lovelace) C.TxOutDatumNone ReferenceScriptNone

    buildMint :: Maybe (C.TxMintValue BuildTx era)
    buildMint = do
      (value, witnesses) <- fold <$> traverse (buildAssetMint . fst) inputs
      pure $ C.TxMintValue maryEraOnwards (C.negateValue value) (C.BuildTxWith witnesses)

    buildAssetMint :: Tokens -> Maybe (C.Value, Map C.PolicyId (C.ScriptWitness C.WitCtxMint era))
    buildAssetMint tokens@(Tokens tokenMap) = do
      value <- tokensToCardanoValue tokens
      witnesses <- fold <$> traverse buildMintWitness (Map.keys $ Map.mapKeys policyId tokenMap)
      pure (value, witnesses)

    buildMintWitness :: PolicyId -> Maybe (Map C.PolicyId (C.ScriptWitness C.WitCtxMint era))
    buildMintWitness policyId = do
      policyId' <- toCardanoPolicyId policyId
      C.ScriptInEra lang script <- Map.lookup (coerce policyId) scripts
      witness <- case script of
        C.PlutusScript v script' ->
          pure $
            C.PlutusScriptWitness
              lang
              v
              (PScript script')
              C.NoScriptDatumForMint
              (C.unsafeHashableScriptData $ C.ScriptDataConstructor 1 []) -- This corresponds to the Burn action in the validator.
              (C.ExecutionUnits 0 0)
        _ -> Nothing
      pure $ Map.singleton policyId' witness
