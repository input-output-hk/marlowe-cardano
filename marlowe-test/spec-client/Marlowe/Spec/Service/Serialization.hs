

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Marlowe.Spec.Service.Serialization
  ( SerializationResponse(..)
  , knownJsonTypes
  , roundtripSerialization
  ) where


import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Jsonable (JsonableType(JsonableType), KnownJsonable, isKnownJson, roundTripJsonable)
import Data.Proxy (Proxy(..))
import Spec.Marlowe.Semantics.Arbitrary ()

import qualified Data.Aeson as A (Value, object, withObject, (.:), (.=))
import qualified Language.Marlowe.Core.V1.Semantics.Types as Marlowe


data SerializationResponse =
    SerializationSuccess
    {
      valueReserialized :: A.Value
    }
  | UnknownType
    {
      unknownType :: String
    }
  | SerializationError
    {
      serializationError :: String
    }
  deriving (Eq, Ord, Read, Show)

instance ToJSON SerializationResponse where
  toJSON SerializationSuccess{..} = A.object . pure $ "serialization-success" A..= valueReserialized
  toJSON UnknownType{..} = A.object . pure $ "unknown-type" A..= unknownType
  toJSON SerializationError{..} = A.object . pure $ "serialization-error" A..= serializationError

instance FromJSON SerializationResponse where
  parseJSON =
    A.withObject "SerializationResponse"
      $ \o ->
            (SerializationSuccess <$> o A..: "serialization-success")
        <|> (UnknownType <$> o A..: "unknown-type")
        <|> (SerializationError <$> o A..: "serialization-error")


roundtripSerialization :: String -> A.Value -> SerializationResponse
roundtripSerialization typeSerialized valueSerialized =
  if isKnownJson knownJsonTypes typeSerialized
    then case roundTripJsonable knownJsonTypes typeSerialized valueSerialized of
             Right valueReserialized -> SerializationSuccess{..}
             Left serializationError      -> SerializationError{..}
    else UnknownType typeSerialized


knownJsonTypes :: KnownJsonable
knownJsonTypes =
  [
    JsonableType "Core.Action" (Proxy :: Proxy Marlowe.Action)
  , JsonableType "Core.Bound" (Proxy :: Proxy Marlowe.Bound)
  , JsonableType "Core.Case" (Proxy :: Proxy (Marlowe.Case Marlowe.Contract))
  , JsonableType "Core.ChoiceId" (Proxy :: Proxy Marlowe.ChoiceId)
  , JsonableType "Core.Contract" (Proxy :: Proxy Marlowe.Contract)
  , JsonableType "Core.Token" (Proxy :: Proxy Marlowe.Token)
  , JsonableType "Core.Payee" (Proxy :: Proxy Marlowe.Payee)
  , JsonableType "Core.Input" (Proxy :: Proxy Marlowe.Input)
  , JsonableType "Core.Observation" (Proxy :: Proxy Marlowe.Observation)
  , JsonableType "Core.Value" (Proxy :: Proxy (Marlowe.Value Marlowe.Observation))
  , JsonableType "Core.Party" (Proxy :: Proxy Marlowe.Party)
  , JsonableType "Core.State" (Proxy :: Proxy Marlowe.State)
{- FIXME: Implement any missing `JSON` and `Arbitrary` instances.
  , JsonableType "Core.Payment" (Proxy :: Proxy Marlowe.Payment)
  , JsonableType "Core.TransactionError" (Proxy :: Proxy Marlowe.TransactionError)
  , JsonableType "Core.TransactionOutput" (Proxy :: Proxy Marlowe.TransactionOutput)
  , JsonableType "Core.TransactionWarning" (Proxy :: Proxy Marlowe.TransactionWarning)
  , JsonableType "Core.Transaction" (Proxy :: Proxy Marlowe.Transaction)
-}
  ]
