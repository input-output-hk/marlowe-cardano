{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Server.REST.ApiError where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (ToJSON (toJSON), Value (Null), encode, object, (.=))
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Transaction.Api (
  ApplyInputsConstraintsBuildupError (..),
  ApplyInputsError (..),
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
    WithdrawConstraintError err -> ApiError (show err) "ConstraintError" Null case err of
      MintingUtxoNotFound _ -> 400
      RoleTokenNotFound _ -> 400
      PayoutNotFound _ -> 400
      CoinSelectionFailed _ -> 400
      _ -> 500
    EmptyPayouts -> ApiError "Empty payouts" "EmptyPayouts" Null 400

instance HasDTO CreateError where
  type DTO CreateError = ApiError

instance ToDTO CreateError where
  toDTO = \case
    CreateEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "WithdrawEraUnsupported" Null 503
    CreateConstraintError err -> ApiError (show err) "ConstraintError" Null case err of
      MintingUtxoNotFound _ -> 400
      RoleTokenNotFound _ -> 400
      PayoutNotFound _ -> 400
      CoinSelectionFailed _ -> 400
      _ -> 500
    CreateLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> ApiError "Marlowe contract not found" "MarloweContractNotFound" Null 404
    CreateLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> ApiError "Marlowe contract version mismatch" "MarloweContractVersionMismatch" Null 400
    CreateLoadMarloweContextFailed LoadMarloweContextToCardanoError -> ApiError "Internal error" "LoadMarloweContextToCardanoError" Null 500
    CreateLoadMarloweContextFailed (MarloweScriptNotPublished _) -> ApiError "Internal error" "MarloweScriptNotPublished" Null 500
    CreateLoadMarloweContextFailed (PayoutScriptNotPublished _) -> ApiError "Internal error" "PayoutScriptNotPublished" Null 500
    CreateLoadMarloweContextFailed (ExtractCreationError _) -> ApiError "Internal error" "ExtractCreationError" Null 500
    CreateLoadMarloweContextFailed (ExtractMarloweTransactionError _) -> ApiError "Internal error" "ExtractMarloweTransactionError" Null 500
    CreateBuildupFailed MintingUtxoSelectionFailed -> ApiError "Minting UTxO selection failed" "MintingUtxoSelectionFailed" Null 400
    CreateBuildupFailed (AddressDecodingFailed _) -> ApiError "Internal error" "AddressDecodingFailed" Null 500
    CreateBuildupFailed (MintingScriptDecodingFailed _) -> ApiError "Internal error" "MintingScriptDecodingFailed" Null 500
    CreateToCardanoError -> ApiError "Internal error" "CreateToCardanoError" Null 400
    CreateSafetyAnalysisError _ -> ApiError "Safety analysis failed" "InternalError" Null 400
    CreateContractNotFound -> ApiError "Contract not found" "Not found" Null 404

instance HasDTO ApplyInputsError where
  type DTO ApplyInputsError = ApiError

instance ToDTO ApplyInputsError where
  toDTO = \case
    ApplyInputsEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "WithdrawEraUnsupported" Null 503
    ApplyInputsConstraintError err -> ApiError (show err) "ConstraintError" Null case err of
      MintingUtxoNotFound _ -> 400
      RoleTokenNotFound _ -> 400
      PayoutNotFound _ -> 400
      CoinSelectionFailed _ -> 400
      _ -> 500
    ScriptOutputNotFound -> ApiError "Script output not found" "ScriptOutputNotFound" Null 400
    ApplyInputsLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> ApiError "Marlowe contract not found" "MarloweContractNotFound" Null 404
    ApplyInputsLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> ApiError "Marlowe contract version mismatch" "MarloweContractVersionMismatch" Null 400
    ApplyInputsLoadMarloweContextFailed LoadMarloweContextToCardanoError -> ApiError "Internal error" "LoadMarloweContextToCardanoError" Null 500
    ApplyInputsLoadMarloweContextFailed (MarloweScriptNotPublished _) -> ApiError "Internal error" "MarloweScriptNotPublished" Null 500
    ApplyInputsLoadMarloweContextFailed (PayoutScriptNotPublished _) -> ApiError "Internal error" "PayoutScriptNotPublished" Null 500
    ApplyInputsLoadMarloweContextFailed (ExtractCreationError _) -> ApiError "Internal error" "ExtractCreationError" Null 500
    ApplyInputsLoadMarloweContextFailed (ExtractMarloweTransactionError _) -> ApiError "Internal error" "ExtractMarloweTransactionError" Null 500
    ApplyInputsConstraintsBuildupFailed (MarloweComputeTransactionFailed err) -> ApiError ("Marlowe compute transaction failed: " <> err) "MarloweComputeTransactionFailed" Null 400
    ApplyInputsConstraintsBuildupFailed UnableToDetermineTransactionTimeout -> ApiError "Unable to determine transaction timeout" "UnableToDetermineTransactionTimeout" Null 400
    SlotConversionFailed _ -> ApiError "Slot conversion failed" "SlotConversionFailed" Null 400
    TipAtGenesis -> ApiError "Internal error" "TipAtGenesis" Null 500
    ValidityLowerBoundTooHigh _ _ -> ApiError "Validity lower bound too high" "ValidityLowerBoundTooHigh" Null 400
