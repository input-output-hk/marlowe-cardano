{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | This module defines the data-transfer object (DTO) translation layer for
-- the web server. DTOs are the types served by the API, which notably include
-- no cardano-api dependencies and have nice JSON representations. This module
-- describes how they are mapped to the internal API types of the runtime.

module Language.Marlowe.Runtime.Web.Server.REST.Error
  where

import Control.Monad.Except (MonadError(throwError))
import Data.Aeson (Value(Null), encode, object, (.=))
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Core.Api (MarloweVersionTag(..))
import Language.Marlowe.Runtime.Transaction.Api
  (ConstraintError(..), CreateBuildupError(..), CreateError(..), LoadMarloweContextError(..))
import Language.Marlowe.Runtime.Web.Server.DTO (DTO, HasDTO, ToDTO, toDTO)
import Servant (ServerError(ServerError))

data ApiError = ApiError
  { message :: String
  , errorCode :: String
  , details :: Value
  , statusCode :: Int
  }

toServerError :: ApiError -> ServerError
toServerError err = do
  let
    ApiError message errorCode details statusCode = err
    body = encode $ object
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

badRequest :: String -> ServerError
badRequest msg = toServerError . ApiError ("Bad Request: " <> msg) "BadRequest" Null $ 400

instance HasDTO (CreateError 'V1) where
  type DTO (CreateError 'V1) = ApiError

instance ToDTO (CreateError 'V1) where
  toDTO = \case
    CreateConstraintError (MintingUtxoNotFound _) -> ApiError "Minting UTxO not found" "MintingUtxoNotFound" Null 500
    CreateConstraintError (RoleTokenNotFound _) -> ApiError "Role token not found" "RoleTokenNotFound" Null 403
    CreateConstraintError ToCardanoError -> ApiError "Internal error" "InternalError" Null 500
    CreateConstraintError MissingMarloweInput -> ApiError "Internal error" "InternalError" Null 500
    CreateConstraintError (PayoutInputNotFound _) -> ApiError "Internal error" "InternalError" Null 500
    CreateConstraintError (CalculateMinUtxoFailed _) -> ApiError "Internal error" "InternalError" Null 500
    CreateConstraintError (CoinSelectionFailed msg) -> ApiError ("Coin selection failed: " <> msg) "CoinSelectionFailed" Null 400
    CreateConstraintError (BalancingError _) -> ApiError "Internal error" "InternalError" Null 500
    CreateLoadMarloweContextFailed LoadMarloweContextErrorNotFound -> ApiError "Marlowe contract not found" "MarloweContractNotFound" Null 404
    CreateLoadMarloweContextFailed (LoadMarloweContextErrorVersionMismatch _) -> ApiError "Marlowe contract version mismatch" "MarloweContractVersionMismatch" Null 400
    CreateLoadMarloweContextFailed (HandshakeFailed _) -> ApiError "Internal error" "InternalError" Null 500
    CreateLoadMarloweContextFailed LoadMarloweContextToCardanoError -> ApiError "Internal error" "InternalError" Null 500
    CreateLoadMarloweContextFailed (MarloweScriptNotPublished _) -> ApiError "Internal error" "InternalError" Null 500
    CreateLoadMarloweContextFailed (PayoutScriptNotPublished _) -> ApiError "Internal error" "InternalError" Null 500
    CreateLoadMarloweContextFailed (ExtractCreationError _) -> ApiError "Internal error" "InternalError" Null 500
    CreateLoadMarloweContextFailed (ExtractMarloweTransactionError _) -> ApiError "Internal error" "InternalError" Null 500
    CreateBuildupFailed MintingUtxoSelectionFailed -> ApiError "Minting UTxO selection failed" "MintingUtxoSelectionFailed" Null 400
    CreateBuildupFailed (AddressDecodingFailed _) -> ApiError "Internal error" "InternalError" Null 500
    CreateBuildupFailed (MintingScriptDecodingFailed _) -> ApiError "Internal error" "InternalError" Null 500
    CreateToCardanoError -> ApiError "Internal error" "InternalError" Null 400

