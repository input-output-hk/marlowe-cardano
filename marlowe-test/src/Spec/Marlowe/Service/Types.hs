

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Service.Types
  ( Request(..)
  , Response(..)
  ) where


import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Plutus.V1.Ledger.Api (POSIXTime(..))

import qualified Data.Aeson as A (Value(Object, String), object, withObject, (.:), (.=))
import qualified Data.Aeson.Types as A (Parser)
import qualified Language.Marlowe.Core.V1.Semantics as Marlowe
import qualified Language.Marlowe.Core.V1.Semantics.Types as Marlowe


data Request =
    TestRoundtripSerialization
    {
      typeSerialized :: String
    , valueSerialized :: A.Value
    }
  | GenerateRandomValue
    {
      typeSerialized :: String
    }
  | ComputeTransaction
    {
      transactionInput :: Marlowe.TransactionInput
    , contract :: Marlowe.Contract
    , state :: Marlowe.State
    }
  | PlayTrace
    {
      transactionInputs :: [Marlowe.TransactionInput]
    , contract :: Marlowe.Contract
    , initialTime :: POSIXTime
    }
    deriving (Eq, Show)

instance FromJSON Request where
  parseJSON =
    A.withObject "Request"
      $ \o ->
        (o A..: "request" :: A.Parser String)
          >>= \case
            "test-roundtrip-serialization" -> TestRoundtripSerialization <$> o A..: "typeId" <*> o A..: "json"
            "generate-random-value"        -> GenerateRandomValue <$> o A..: "typeId"
            "compute-transaction"          -> ComputeTransaction <$> o A..: "transactionInput" <*> o A..: "coreContract" <*> o A..: "state"
            "playtrace"                    -> PlayTrace <$> o A..: "transactionInputs" <*> o A..: "coreContract" <*> (POSIXTime <$> o A..: "initialTime")
            request                        -> fail $ "Request not understood: " <> show request <> "."

instance ToJSON Request where
  toJSON TestRoundtripSerialization{..} =
    A.object
      [
        "request" A..= ("test-roundtrip-serialization" :: String)
      , "typeId" A..= typeSerialized
      , "json"  A..= valueSerialized
      ]
  toJSON GenerateRandomValue{..} =
    A.object
      [
        "request" A..= ("generate-random-value" :: String)
      , "typeId" A..= typeSerialized
      ]
  toJSON ComputeTransaction{..} =
    A.object
      [
        "request" A..= ("compute-transaction" :: String)
      , "transactionInput" A..= transactionInput
      , "coreContract" A..= contract
      , "state" A..= state
      ]
  toJSON PlayTrace{..} =
    A.object
      [
        "request" A..= ("playtrace" :: String)
      , "transactionInputs" A..= transactionInputs
      , "coreContract" A..= contract
      , "initialTime" A..= getPOSIXTime initialTime
      ]


data Response =
    InvalidRequest
    {
      errorInvalid :: String
    }
  | UnknownRequest
  | RequestResponse
    {
      valueResponse :: A.Value
    }
  | RequestNotImplemented
  | RequestTimeOut
  | ResponseFailure
    {
      failureResponse :: String
    }
    deriving (Eq, Ord, Read, Show)

instance FromJSON Response where
    parseJSON (A.String "UnknownRequest") = return UnknownRequest
    parseJSON (A.String "RequestNotImplemented") = return RequestNotImplemented
    parseJSON (A.Object v) = (InvalidRequest <$> v A..: "invalid-request") <|> (RequestResponse <$> v A..: "request-response")
    parseJSON _ = fail "Response must be either a string or an A.object"

instance ToJSON Response where
  toJSON UnknownRequest = "UnknownRequest"
  toJSON RequestNotImplemented = "RequestNotImplemented"
  toJSON RequestTimeOut = "RequestTimeOut"
  toJSON (InvalidRequest err) = A.object . pure $ "invalid-request" A..= err
  toJSON (RequestResponse res) = A.object . pure $ "request-response" A..= res
  toJSON (ResponseFailure err) = A.object . pure $ "invalid-request" A..= err
