{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Adapter.Server.ApiError where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (ToJSON (toJSON), Value (Null), encode, object, (.:), (.=))
import qualified Data.Aeson.Decoding as A
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
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (DTO, HasDTO, ToDTO, toDTO)
import Language.Marlowe.Runtime.Web.Server.DTO (DTO, ToDTO, toDTO)
import Language.Marlowe.Runtime.Web.Types (ApiError (..))
import Servant (ServerError (ServerError))

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

fromServerError :: ServerError -> Maybe ApiError
fromServerError err = do
  let ServerError statusCode _ body _ = err
      parser (A.Object o) = do
        message <- o .: "message"
        errorCode <- o .: "errorCode"
        details <- o .: "details"
        pure $ ApiError message errorCode details statusCode
      parser _ = fail "Unable to decode ApiError"
  json <- A.decode body
  A.parseMaybe parser json

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
