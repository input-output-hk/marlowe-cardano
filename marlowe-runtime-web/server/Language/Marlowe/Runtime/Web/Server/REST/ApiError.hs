{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Server.REST.ApiError where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (ToJSON (toJSON), Value (Null), encode, object, (.=))
import qualified Data.Aeson.Types as A
import Data.Maybe (fromMaybe)
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
import Language.Marlowe.Runtime.Web.Server.DTO (DTO, HasDTO, ToDTO, toDTO)
import Servant (ServerError (ServerError))

data ApiError = ApiError
  { message :: String
  , errorCode :: String
  , details :: Value
  , statusCode :: Int
  }

toServerError :: ApiError -> ServerError
toServerError err = do
  let ApiError message errorCode details statusCode = err
      body =
        encode $
          object
            [ "message" .= message
            , "errorCode" .= errorCode
            , "details" .= details
            ]
      phrase = fromMaybe "" $ lookup statusCode reasons
      headers = [("Content-Type", "application/json")]
  ServerError statusCode phrase body headers
  where
    reasons =
      [ (300, "Multiple Choices")
      , (301, "Moved Permanently")
      , (302, "Found")
      , (303, "See Other")
      , (304, "Not Modified")
      , (305, "Use Proxy")
      , (307, "Temporary Redirect")
      , (400, "Bad Request")
      , (401, "Unauthorized")
      , (402, "Payment Required")
      , (403, "Forbidden")
      , (404, "Not Found")
      , (405, "Method Not Allowed")
      , (406, "Not Acceptable")
      , (407, "Proxy Authentication Required")
      , (409, "Conflict")
      , (410, "Gone")
      , (411, "Length Required")
      , (412, "Precondition Failed")
      , (413, "Request Entity Too Large")
      , (414, "Request-URI Too Large")
      , (415, "Unsupported Media Type")
      , (416, "Request range not satisfiable")
      , (417, "Expectation Failed")
      , (418, "I'm a teapot")
      , (422, "Unprocessable Entity")
      , (500, "Internal Server Error")
      , (501, "Not Implemented")
      , (502, "Bad Gateway")
      , (503, "Service Unavailable")
      , (504, "Gateway Time-out")
      , (505, "HTTP Version not supported")
      ]

serverErrorFromDTO :: (ToDTO e, DTO e ~ ApiError) => e -> ServerError
serverErrorFromDTO = toServerError . toDTO

throwDTOError :: (ToDTO e, DTO e ~ ApiError, MonadError ServerError m) => e -> m a
throwDTOError = throwError . serverErrorFromDTO

badRequest :: String -> Maybe String -> ServerError
badRequest msg errorCode = toServerError . ApiError msg (fromMaybe "BadRequest" errorCode) Null $ 400

badRequest'' :: (ToJSON a) => String -> String -> a -> ServerError
badRequest'' msg errorCode json = toServerError . ApiError msg errorCode (toJSON json) $ 400

badRequest' :: String -> ServerError
badRequest' msg = badRequest msg Nothing

notFound :: String -> Maybe String -> ServerError
notFound msg errorCode = toServerError . ApiError msg (fromMaybe "NotFound" errorCode) Null $ 404

notFoundWithErrorCode :: String -> String -> ServerError
notFoundWithErrorCode msg = notFound msg . Just

notFound' :: String -> ServerError
notFound' msg = notFound msg Nothing

rangeNotSatisfiable :: String -> Maybe String -> ServerError
rangeNotSatisfiable msg errorCode = toServerError . ApiError msg (fromMaybe "RangeNotSatisfiable" errorCode) Null $ 416

rangeNotSatisfiable' :: String -> ServerError
rangeNotSatisfiable' msg = rangeNotSatisfiable msg Nothing

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
      AddressDecodingFailed address ->
        tagged
          "AddressDecodingFailed"
          ["address" .= toJSON (toDTO address)]
      MintingScriptDecodingFailed _ -> tagged "MintingScriptDecodingFailed" []

    statusCode = case err of
      MintingUtxoSelectionFailed -> 400
      AddressDecodingFailed _ -> 500
      MintingScriptDecodingFailed _ -> 500

    errorCode = case err of
      MintingUtxoSelectionFailed -> "MintingUtxoSelectionFailed"
      AddressDecodingFailed _ -> "AddressDecodingFailed"
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
      MarloweComputeTransactionFailed transactionError ->
        tagged
          "MarloweComputeTransactionFailed"
          ["transactionError" .= toJSON transactionError]
      UnableToDetermineTransactionTimeout -> tagged "UnableToDetermineTransactionTimeout" []

    statusCode = case err of
      MarloweComputeTransactionFailed _ -> 400
      UnableToDetermineTransactionTimeout -> 400

    errorCode = case err of
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
