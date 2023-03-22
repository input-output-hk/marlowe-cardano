-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Marlowe types for the test service client.
--
-----------------------------------------------------------------------------


{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Service.Types
  ( -- * Types
    Request(..)
  , Response(..)
  , Seed(..)
  , Size(..)
  ) where


import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Plutus.V1.Ledger.Api (POSIXTime(..))

import qualified Data.Aeson as A (Value(Object, String), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Types as A (Parser)
import qualified Language.Marlowe.Core.V1.Semantics as Marlowe
import qualified Language.Marlowe.Core.V1.Semantics.Types as Marlowe

newtype Size = Size Int deriving (Eq, Show, ToJSON, FromJSON)
newtype Seed = Seed Int deriving (Eq, Show, ToJSON, FromJSON)

data Request =
    TestRoundtripSerialization
    {
      typeSerialized :: String
    , valueSerialized :: A.Value
    }
  | GenerateRandomValue
    {
      typeSerialized :: String
    , size :: Maybe Size
    , seed :: Maybe Seed
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
  | EvalValue
    {
      environment :: Marlowe.Environment
    , state :: Marlowe.State
    , value :: Marlowe.Value Marlowe.Observation
    }
  | EvalObservation
    {
      environment :: Marlowe.Environment
    , state :: Marlowe.State
    , observation :: Marlowe.Observation
    }
    deriving (Eq, Show)

instance FromJSON Request where
  parseJSON =
    A.withObject "Request"
      $ \o ->
        (o A..: "request" :: A.Parser String)
          >>= \case
            "test-roundtrip-serialization" -> TestRoundtripSerialization <$> o A..: "typeId" <*> o A..: "json"
            "generate-random-value"        -> GenerateRandomValue <$> o A..: "typeId" <*> o A..:? "size" <*> o A..:? "seed"
            "compute-transaction"          -> ComputeTransaction <$> o A..: "transactionInput" <*> o A..: "coreContract" <*> o A..: "state"
            "playtrace"                    -> PlayTrace <$> o A..: "transactionInputs" <*> o A..: "coreContract" <*> (POSIXTime <$> o A..: "initialTime")
            "eval-value"                   -> EvalValue <$> o A..: "environment" <*> o A..: "state" <*> o A..: "value"
            "eval-observation"             -> EvalObservation <$> o A..: "environment" <*> o A..: "state" <*> o A..: "observation"
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
      , "size" A..= size
      , "seed" A..= seed
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
  toJSON EvalValue{..} =
    A.object
      [
        "request" A..= ("eval-value" :: String)
      , "environment" A..= environment
      , "state" A..= state
      , "value" A..= value
      ]
  toJSON EvalObservation{..} =
    A.object
      [
        "request" A..= ("eval-value" :: String)
      , "environment" A..= environment
      , "state" A..= state
      , "observation" A..= observation
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
