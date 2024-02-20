{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Server.DTO.ApiError where

import Control.Category ((>>>))
import Data.Aeson (ToJSON (toJSON), Value (Null), object, (.:), (.=))
import qualified Data.Aeson.Types as A
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Traversable (for)
import qualified Language.Marlowe.Runtime.History.Api as H
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
  CoinSelectionError (..),
  ConstraintError (..),
  CreateBuildupError (..),
  CreateError (..),
  LoadMarloweContextError (..),
  WithdrawError (..),
 )
import Language.Marlowe.Runtime.Web (ScriptHash (..), Tokens, TxOutRef)
import Language.Marlowe.Runtime.Web.Server.DTO (DTO, FromDTO (fromDTO), HasDTO, ToDTO, toDTO)
import Language.Marlowe.Runtime.Web.Types (ApiError (..))

instance HasDTO WithdrawError where
  type DTO WithdrawError = ApiError

instance ToDTO WithdrawError where
  toDTO = \case
    WithdrawEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "WithdrawEraUnsupported" Null 503
    WithdrawConstraintError err -> constraintErrorToApiError err
    EmptyPayouts -> ApiError "Empty payouts" "EmptyPayouts" Null 400
    WithdrawLoadHelpersContextFailed err -> ApiError ("Failed to load helper-script context: " <> show err) "WithdrawLoadHelperContextFailed" Null 503

tagged :: String -> [A.Pair] -> Value
tagged tag = object . (:) ("tag" .= tag)

constraintErrorToApiError :: ConstraintError -> ApiError
constraintErrorToApiError err = ApiError (show err) errorCode details statusCode
  where
    coinSelectionErrorDetails :: CoinSelectionError -> Value
    coinSelectionErrorDetails = \case
      NoCollateralFound txOutRefs ->
        tagged
          "NoCollateralFound"
          ["txOutRefs" .= toJSON (toDTO txOutRefs)]
      InsufficientLovelace required available ->
        tagged
          "InsufficientLovelace"
          [ "required" .= required
          , "available" .= available
          ]
      InsufficientTokens tokens ->
        tagged
          "InsufficientTokens"
          ["tokens" .= toJSON (toDTO tokens)]

    details = case err of
      InvalidTokenQuantity assetId quantity ->
        tagged
          "InvalidTokenQuantity"
          ["assetId" .= toJSON (toDTO assetId), "quantity" .= quantity]
      MintingUtxoNotFound txOutRef ->
        tagged
          "MintingUtxoNotFound"
          ["txOutRef" .= toJSON (toDTO txOutRef)]
      RoleTokenNotFound assetId ->
        tagged
          "RoleTokenNotFound"
          ["assetId" .= toJSON (toDTO assetId)]
      ToCardanoError -> tagged "ToCardanoError" []
      MissingMarloweInput -> tagged "MissingMarloweInput" []
      PayoutNotFound txOutRef ->
        tagged
          "PayoutNotFound"
          ["txOutRef" .= toJSON (toDTO txOutRef)]
      InvalidPayoutDatum txOutRef datum ->
        tagged
          "InvalidPayoutDatum"
          [ "txOutRef" .= toJSON (toDTO txOutRef)
          , "datum" .= toJSON datum
          ]
      InvalidHelperDatum txOutRef datum ->
        tagged
          "InvalidHelperDatum"
          [ "txOutRef" .= toJSON (toDTO txOutRef)
          , "datum" .= toJSON datum
          ]
      InvalidPayoutScriptAddress txOutRef address ->
        tagged
          "InvalidPayoutScriptAddress"
          [ "txOutRef" .= toJSON (toDTO txOutRef)
          , "address" .= toJSON (toDTO address)
          ]
      CalculateMinUtxoFailed msg ->
        tagged
          "CalculateMinUtxoFailed"
          ["msg" .= msg]
      CoinSelectionFailed coinSelectionFailed ->
        tagged
          "CoinSelectionFailed"
          ["coinSelectionFailed" .= coinSelectionErrorDetails coinSelectionFailed]
      BalancingError msg ->
        tagged
          "BalancingError"
          ["msg" .= msg]
      MarloweInputInWithdraw -> tagged "MarloweInputInWithdraw" []
      MarloweOutputInWithdraw -> tagged "MarloweOutputInWithdraw" []
      PayoutOutputInWithdraw -> tagged "PayoutOutputInWithdraw" []
      PayoutInputInCreateOrApply -> tagged "PayoutInputInCreateOrApply" []
      UnknownPayoutScript scriptHash ->
        tagged
          "UnknownPayoutScript"
          ["scriptHash" .= toJSON scriptHash]
      HelperScriptNotFound tokenName ->
        tagged
          "HelperScriptNotFound"
          ["tokenName" .= toJSON (toDTO tokenName)]

    statusCode = case err of
      InvalidTokenQuantity _ _ -> 400
      MintingUtxoNotFound _ -> 400
      RoleTokenNotFound _ -> 400
      PayoutNotFound _ -> 400
      CoinSelectionFailed _ -> 400
      ToCardanoError -> 500
      MissingMarloweInput -> 500
      InvalidPayoutDatum _ _ -> 500
      InvalidHelperDatum _ _ -> 500
      InvalidPayoutScriptAddress _ _ -> 500
      CalculateMinUtxoFailed _ -> 500
      BalancingError _ -> 500
      MarloweInputInWithdraw -> 500
      MarloweOutputInWithdraw -> 500
      PayoutOutputInWithdraw -> 500
      PayoutInputInCreateOrApply -> 500
      UnknownPayoutScript _ -> 500
      HelperScriptNotFound _ -> 503

    errorCode = case err of
      InvalidTokenQuantity _ _ -> "InvalidTokenQuantity"
      MintingUtxoNotFound _ -> "MintingUtxoNotFound"
      RoleTokenNotFound _ -> "RoleTokenNotFound"
      ToCardanoError -> "ToCardanoError"
      MissingMarloweInput -> "MissingMarloweInput"
      PayoutNotFound _ -> "PayoutNotFound"
      InvalidPayoutDatum _ _ -> "InvalidPayoutDatum"
      InvalidHelperDatum _ _ -> "InvalidHelperDatum"
      InvalidPayoutScriptAddress _ _ -> "InvalidPayoutScriptAddress"
      CalculateMinUtxoFailed _ -> "CalculateMinUtxoFailed"
      CoinSelectionFailed _ -> "CoinSelectionFailed"
      BalancingError _ -> "BalancingError"
      MarloweInputInWithdraw -> "MarloweInputInWithdraw"
      MarloweOutputInWithdraw -> "MarloweOutputInWithdraw"
      PayoutOutputInWithdraw -> "PayoutOutputInWithdraw"
      PayoutInputInCreateOrApply -> "PayoutInputInCreateOrApply"
      UnknownPayoutScript _ -> "UnknownPayoutScript"
      HelperScriptNotFound _ -> "HelperScriptNotFound"

decodeConstraintError :: A.Value -> A.Parser ConstraintError
decodeConstraintError = A.withObject "ConstraintError" $ \o -> do
  let fromDTO' :: forall a. (FromDTO a) => String -> DTO a -> A.Parser a
      fromDTO' errMsg = fromDTO >>> maybe (fail errMsg) pure

      decodeDTOProp :: forall a. (A.FromJSON (DTO a), FromDTO a) => A.Key -> String -> A.Parser a
      decodeDTOProp label errMsg = o .: label >>= (fromDTO >>> maybe (fail errMsg) pure)

      decodeTxOutRef = decodeDTOProp "txOutRef" "Unable to decode TxOutRef"

  tag :: Text <- o .: "tag"
  case tag of
    "MintingUtxoNotFound" -> MintingUtxoNotFound <$> decodeTxOutRef
    "RoleTokenNotFound" ->
      RoleTokenNotFound <$> do
        assetIdDTO <- o .: "assetId"
        maybe (fail "Unable to decode AssetId") pure (fromDTO assetIdDTO)
    "ToCardanoError" -> pure ToCardanoError
    "MissingMarloweInput" -> pure MissingMarloweInput
    "PayoutNotFound" -> PayoutNotFound <$> decodeTxOutRef
    "InvalidPayoutDatum" ->
      InvalidPayoutDatum
        <$> decodeTxOutRef
        <*> do
          datum <- o .: "datum"
          maybe (fail "Unable to decode Datum") pure datum
    "InvalidHelperDatum" ->
      InvalidHelperDatum
        <$> decodeTxOutRef
        <*> o .: "datum"
    "InvalidPayoutScriptAddress" ->
      InvalidPayoutScriptAddress
        <$> decodeTxOutRef
        <*> do
          addressDTO <- o .: "address"
          maybe (fail "Unable to decode Address") pure (fromDTO addressDTO)
    "CalculateMinUtxoFailed" -> CalculateMinUtxoFailed <$> o .: "msg"
    "CoinSelectionFailed" ->
      CoinSelectionFailed <$> do
        coinSelectionError :: Text <- o .: "coinSelectionFailed"
        case coinSelectionError of
          "NoCollateralFound" ->
            NoCollateralFound <$> do
              (rawTxOutRefs :: [TxOutRef]) <- o .: "txOutRefs"
              txOutRefs <- for rawTxOutRefs $ fromDTO' "Unable to decode TxOutRefs"
              pure $ Set.fromList txOutRefs
          "InsufficientLovelace" -> InsufficientLovelace <$> o .: "required" <*> o .: "available"
          "InsufficientTokens" ->
            InsufficientTokens <$> do
              (rawTokens :: Tokens) <- o .: "tokens"
              fromDTO' "Unable to decode Tokens" rawTokens
          _ -> fail "Unable to decode CoinSelectionError"
    "BalancingError" -> BalancingError <$> o .: "msg"
    "MarloweInputInWithdraw" -> pure MarloweInputInWithdraw
    "MarloweOutputInWithdraw" -> pure MarloweOutputInWithdraw
    "PayoutOutputInWithdraw" -> pure PayoutOutputInWithdraw
    "PayoutInputInCreateOrApply" -> pure PayoutInputInCreateOrApply
    "UnknownPayoutScript" ->
      UnknownPayoutScript <$> do
        scriptHash :: ScriptHash <- o .: "scriptHash"
        fromDTO' "Unable to decode ScriptHash" scriptHash
    "HelperScriptNotFound" -> HelperScriptNotFound <$> o .: "tokenName"
    _ -> fail "Unable to decode ConstraintError"

extractCreationErrorDetails :: H.ExtractCreationError -> Value
extractCreationErrorDetails = \case
  H.TxIxNotFound -> tagged "TxIxNotFound" []
  H.ByronAddress -> tagged "ByronAddress" []
  H.NonScriptAddress -> tagged "NonScriptAddress" []
  H.InvalidScriptHash -> tagged "InvalidScriptHash" []
  H.NoCreateDatum -> tagged "NoCreateDatum" []
  H.InvalidCreateDatum -> tagged "InvalidCreateDatum" []
  H.NotCreationTransaction -> tagged "NotCreationTransaction" []

extractMarloweTransactionErrorDetails :: H.ExtractMarloweTransactionError -> Value
extractMarloweTransactionErrorDetails = \case
  H.TxInNotFound -> tagged "TxInNotFound" []
  H.NoRedeemer -> tagged "NoRedeemer" []
  H.InvalidRedeemer -> tagged "InvalidRedeemer" []
  H.NoTransactionDatum -> tagged "NoTransactionDatum" []
  H.InvalidTransactionDatum -> tagged "InvalidTransactionDatum" []
  H.NoPayoutDatum txOutRef ->
    tagged
      "NoPayoutDatum"
      ["txOutRef" .= toJSON (toDTO txOutRef)]
  H.InvalidPayoutDatum txOutRef ->
    tagged
      "InvalidPayoutDatum"
      ["txOutRef" .= toJSON (toDTO txOutRef)]
  H.InvalidValidityRange -> tagged "InvalidValidityRange" []
  H.SlotConversionFailed -> tagged "SlotConversionFailed" []
  H.MultipleContractInputs txOutRefs ->
    tagged
      "MultipleContractInputs"
      ["txOutRefs" .= toJSON (toDTO txOutRefs)]

loadMarloweContextErrorToApiError :: LoadMarloweContextError -> ApiError
loadMarloweContextErrorToApiError err = ApiError (show err) errorCode details statusCode
  where
    details = case err of
      LoadMarloweContextErrorNotFound -> tagged "LoadMarloweContextErrorNotFound" []
      LoadMarloweContextErrorVersionMismatch version ->
        tagged
          "LoadMarloweContextErrorVersionMismatch"
          ["version" .= toJSON (toDTO version)]
      LoadMarloweContextToCardanoError -> tagged "LoadMarloweContextToCardanoError" []
      MarloweScriptNotPublished scriptHash ->
        tagged
          "MarloweScriptNotPublished"
          ["scriptHash" .= toJSON (toDTO scriptHash)]
      PayoutScriptNotPublished scriptHash ->
        tagged
          "PayoutScriptNotPublished"
          ["scriptHash" .= toJSON scriptHash]
      ExtractCreationError extractCreationError ->
        tagged
          "ExtractCreationError"
          ["extractCreationError" .= extractCreationErrorDetails extractCreationError]
      ExtractMarloweTransactionError extractMarloweTransactionError ->
        tagged
          "ExtractMarloweTransactionError"
          ["extractMarloweTransactionError" .= extractMarloweTransactionErrorDetails extractMarloweTransactionError]

    statusCode = case err of
      LoadMarloweContextErrorNotFound -> 404
      LoadMarloweContextErrorVersionMismatch _ -> 400
      LoadMarloweContextToCardanoError -> 500
      MarloweScriptNotPublished _ -> 500
      PayoutScriptNotPublished _ -> 500
      ExtractCreationError _ -> 500
      ExtractMarloweTransactionError _ -> 500

    errorCode = case err of
      LoadMarloweContextErrorNotFound -> "LoadMarloweContextErrorNotFound"
      LoadMarloweContextErrorVersionMismatch _ -> "LoadMarloweContextErrorVersionMismatch"
      LoadMarloweContextToCardanoError -> "LoadMarloweContextToCardanoError"
      MarloweScriptNotPublished _ -> "MarloweScriptNotPublished"
      PayoutScriptNotPublished _ -> "PayoutScriptNotPublished"
      ExtractCreationError _ -> "ExtractCreationError"
      ExtractMarloweTransactionError _ -> "ExtractMarloweTransactionError"

createBuildupErrorToApiError :: CreateBuildupError -> ApiError
createBuildupErrorToApiError err = ApiError (show err) errorCode details statusCode
  where
    details = case err of
      MintingUtxoSelectionFailed -> tagged "MintingUtxoSelectionFailed" []
      AddressesDecodingFailed address ->
        tagged
          "AddressesDecodingFailed"
          ["addresses" .= toJSON (toDTO address)]
      InvalidInitialState ->
        tagged
          "InvalidInitialState"
          []
      MintingScriptDecodingFailed _ -> tagged "MintingScriptDecodingFailed" []

    statusCode = case err of
      MintingUtxoSelectionFailed -> 400
      AddressesDecodingFailed _ -> 500
      InvalidInitialState -> 500
      MintingScriptDecodingFailed _ -> 500

    errorCode = case err of
      MintingUtxoSelectionFailed -> "MintingUtxoSelectionFailed"
      AddressesDecodingFailed _ -> "AddressesDecodingFailed"
      InvalidInitialState -> "InvalidInitialState"
      MintingScriptDecodingFailed _ -> "MintingScriptDecodingFailed"

instance HasDTO CreateError where
  type DTO CreateError = ApiError

instance ToDTO CreateError where
  toDTO = \case
    CreateEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "CreateEraUnsupported" Null 503
    CreateConstraintError err -> constraintErrorToApiError err
    CreateLoadMarloweContextFailed err -> loadMarloweContextErrorToApiError err
    CreateBuildupFailed err -> createBuildupErrorToApiError err
    CreateToCardanoError -> ApiError "Internal error" "CreateToCardanoError" Null 400
    CreateSafetyAnalysisError safetyAnalysisError -> do
      let details =
            object
              ["safetyAnalysisProcessFailed" .= safetyAnalysisError]
      ApiError "Safety analysis failed" "SafetyAnalysisFailed" details 400
    CreateSafetyAnalysisFailed errors -> ApiError "Contract unsafe, refusing to create" "SafetyAnalysisFailed" (toJSON errors) 400
    CreateContractNotFound -> ApiError "Contract not found" "ContractNotFound" Null 404
    ProtocolParamNoUTxOCostPerByte ->
      ApiError
        "Internal error. Unable to compute min Ada deposit bound because of probably server misconfiguration"
        "ProtocolParamNoUTxOCostPerByte"
        Null
        500
    InsufficientMinAdaDeposit required ->
      ApiError "Min Ada deposit insufficient." "InsufficientMinAdaDeposit" (object ["minimumRequiredDeposit" .= required]) 400
    CreateLoadHelpersContextFailed err -> ApiError ("Failed to load helper-script context: " <> show err) "CreateLoadHelperContextFailed" Null 503

applyInputsConstraintsBuildupErrorToApiError :: ApplyInputsConstraintsBuildupError -> ApiError
applyInputsConstraintsBuildupErrorToApiError err = ApiError (show err) errorCode details statusCode
  where
    details = case err of
      InvalidMarloweState -> tagged "InvalidMarloweState" []
      MarloweComputeTransactionFailed transactionError ->
        tagged
          "MarloweComputeTransactionFailed"
          ["transactionError" .= toJSON transactionError]
      UnableToDetermineTransactionTimeout -> tagged "UnableToDetermineTransactionTimeout" []

    statusCode = case err of
      InvalidMarloweState -> 400
      MarloweComputeTransactionFailed _ -> 400
      UnableToDetermineTransactionTimeout -> 400

    errorCode = case err of
      InvalidMarloweState -> "InvalidMarloweState"
      MarloweComputeTransactionFailed _ -> "MarloweComputeTransactionFailed"
      UnableToDetermineTransactionTimeout -> "UnableToDetermineTransactionTimeout"

instance HasDTO ApplyInputsError where
  type DTO ApplyInputsError = ApiError

instance ToDTO ApplyInputsError where
  toDTO = \case
    ApplyInputsEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "ApplyInputsEraUnsupported" Null 503
    ApplyInputsConstraintError err -> constraintErrorToApiError err
    ApplyInputsLoadMarloweContextFailed err -> loadMarloweContextErrorToApiError err
    ApplyInputsConstraintsBuildupFailed err -> applyInputsConstraintsBuildupErrorToApiError err
    ApplyInputsContractContinuationNotFound -> ApiError "Contract continuation not found" "ContractContinuationNotFound" Null 404
    ApplyInputsSafetyAnalysisError safetyAnalysisError -> do
      let details =
            object
              ["safetyAnalysisProcessFailed" .= safetyAnalysisError]
      ApiError "Safety analysis failed" "SafetyAnalysisFailed" details 400
    ScriptOutputNotFound -> ApiError "Script output not found" "ScriptOutputNotFound" Null 400
    SlotConversionFailed _ -> ApiError "Slot conversion failed" "SlotConversionFailed" Null 400
    TipAtGenesis -> ApiError "Internal error" "TipAtGenesis" Null 500
    ValidityLowerBoundTooHigh _ _ -> ApiError "Validity lower bound too high" "ValidityLowerBoundTooHigh" Null 400
    ApplyInputsLoadHelpersContextFailed err -> ApiError ("Failed to load helper-script context: " <> show err) "ApplyInputsLoadHelperContextFailed" Null 503

statusCodeLoadMarloweContextError :: LoadMarloweContextError -> Int
statusCodeLoadMarloweContextError = \case
  LoadMarloweContextErrorNotFound -> 404
  LoadMarloweContextErrorVersionMismatch _ -> 400
  LoadMarloweContextToCardanoError -> 500
  MarloweScriptNotPublished _ -> 500
  PayoutScriptNotPublished _ -> 500
  ExtractCreationError _ -> 500
  ExtractMarloweTransactionError _ -> 500
