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
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import GHC.Generics (C1, Constructor (..), D1, Generic (..), M1 (..), type (:+:) (..))
import Language.Marlowe.Runtime.Transaction.Api (
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
    WithdrawConstraintError err -> apiError' err statusCodeConstraintError
    EmptyPayouts -> ApiError "Empty payouts" "EmptyPayouts" Null 400
    WithdrawLoadHelperContextFailed err -> ApiError ("Failed to load helper-script context: " <> show err) "WithdrawLoadHelperContextFailed" Null 503

instance HasDTO CreateError where
  type DTO CreateError = ApiError

instance ToDTO CreateError where
  toDTO = \case
    CreateEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "CreateEraUnsupported" Null 503
    CreateConstraintError err -> apiError' err statusCodeConstraintError
    CreateLoadMarloweContextFailed err -> apiError' err statusCodeLoadMarloweContextError
    CreateBuildupFailed err -> apiError err case err of
      MintingUtxoSelectionFailed -> 400
      AddressDecodingFailed _ -> 500
      MintingScriptDecodingFailed _ -> 500
    CreateToCardanoError -> ApiError "Internal error" "CreateToCardanoError" Null 400
    CreateSafetyAnalysisError _ -> ApiError "Safety analysis failed" "SafetyAnalysisFailed" Null 400
    CreateContractNotFound -> ApiError "Contract not found" "Not found" Null 404
    ProtocolParamNoUTxOCostPerByte -> ApiError "Unable to compute min Ada deposit bound" "Internal error" Null 500
    InsufficientMinAdaDeposit required -> ApiError "Min Ada deposit insufficient." "Bad Request" (object ["minimumRequiredDeposit" .= required]) 400
    RequiresSingleThreadToken -> ApiError "Exactly one thread token name is required." "RequiresSingleThreadToken" Null 400
    CreateLoadHelperContextFailed err -> ApiError ("Failed to load helper-script context: " <> show err) "CreateLoadHelperContextFailed" Null 503

instance HasDTO ApplyInputsError where
  type DTO ApplyInputsError = ApiError

instance ToDTO ApplyInputsError where
  toDTO = \case
    ApplyInputsEraUnsupported era -> ApiError ("Current network era not supported: " <> show era) "ApplyInputsEraUnsupported" Null 503
    ApplyInputsConstraintError err -> apiError' err statusCodeConstraintError
    ApplyInputsLoadMarloweContextFailed err -> apiError' err statusCodeLoadMarloweContextError
    ApplyInputsConstraintsBuildupFailed err -> apiError err 400
    ScriptOutputNotFound -> ApiError "Script output not found" "ScriptOutputNotFound" Null 400
    SlotConversionFailed _ -> ApiError "Slot conversion failed" "SlotConversionFailed" Null 400
    TipAtGenesis -> ApiError "Internal error" "TipAtGenesis" Null 500
    ValidityLowerBoundTooHigh _ _ -> ApiError "Validity lower bound too high" "ValidityLowerBoundTooHigh" Null 400
    ApplyInputsLoadHelperContextFailed err -> ApiError ("Failed to load helper-script context: " <> show err) "ApplyInputsLoadHelperContextFailed" Null 503

statusCodeConstraintError :: ConstraintError -> Int
statusCodeConstraintError = \case
  MintingUtxoNotFound _ -> 400
  RoleTokenNotFound _ -> 400
  PayoutNotFound _ -> 400
  CoinSelectionFailed _ -> 400
  ToCardanoError -> 500
  MissingMarloweInput -> 500
  InvalidPayoutDatum _ _ -> 500
  InvalidPayoutScriptAddress _ _ -> 500
  CalculateMinUtxoFailed _ -> 500
  BalancingError _ -> 500
  MarloweInputInWithdraw -> 500
  MarloweOutputInWithdraw -> 500
  PayoutOutputInWithdraw -> 500
  PayoutInputInCreateOrApply -> 500
  UnknownPayoutScript _ -> 500
  HelperScriptNotFound _ -> 503

statusCodeLoadMarloweContextError :: LoadMarloweContextError -> Int
statusCodeLoadMarloweContextError = \case
  LoadMarloweContextErrorNotFound -> 404
  LoadMarloweContextErrorVersionMismatch _ -> 400
  LoadMarloweContextToCardanoError -> 500
  MarloweScriptNotPublished _ -> 500
  PayoutScriptNotPublished _ -> 500
  ExtractCreationError _ -> 500
  ExtractMarloweTransactionError _ -> 500

apiError :: (Show a, HasConstructor (Rep a), Generic a, ToJSON a) => a -> Int -> ApiError
apiError err = ApiError (show err) (constructorToString err) (toJSON err)

apiError' :: (Show a, HasConstructor (Rep a), Generic a, ToJSON a) => a -> (a -> Int) -> ApiError
apiError' err f = let statusCode = f err in apiError err statusCode

constructorToString :: (HasConstructor (Rep a)) => (Generic a) => a -> String
constructorToString = constructorName . from

class HasConstructor (f :: Type -> Type) where
  constructorName :: f x -> String

instance (HasConstructor f) => HasConstructor (D1 c f) where
  constructorName (M1 x) = constructorName x

instance (HasConstructor x, HasConstructor y) => HasConstructor (x :+: y) where
  constructorName (L1 l) = constructorName l
  constructorName (R1 r) = constructorName r

instance (Constructor c) => HasConstructor (C1 c f) where
  constructorName = conName
