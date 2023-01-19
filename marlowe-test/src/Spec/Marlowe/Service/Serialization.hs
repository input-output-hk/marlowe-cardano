-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Test of Marlowe's Cardano JSON implementation against the reference implementation.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Service.Serialization
  ( -- * Types
    SerializationResponse(..)
  , knownJsonTypes
    -- * Testing
  , roundtripSerialization
  ) where


import Control.Applicative ((<|>))
import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Jsonable (JsonableType(JsonableType), KnownJsonable, isKnownJson, roundTripJsonable)
import Data.Proxy (Proxy(..))
import Spec.Marlowe.Semantics.Arbitrary ()

import qualified Data.Aeson as A (Value, object, withObject, (.:), (.=))
import qualified Language.Marlowe.Core.V1.Semantics as Marlowe
import qualified Language.Marlowe.Core.V1.Semantics.Types as Marlowe


-- | Response to a round-trip serialization request.
data SerializationResponse =
    -- | Success.
    SerializationSuccess
    {
      valueReserialized :: A.Value  -- ^ The reserialized value.
    }
    -- | The type was not known.
  | UnknownType
    {
      unknownType :: String  -- ^ The type.
    }
    -- | The deserialization or serialization failed.
  | SerializationError
    {
      serializationError :: String  -- ^ The error message.
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


-- | Deserialize and then serialize a value.
roundtripSerialization
  :: String  -- ^ The key to the type.
  -> A.Value  -- ^ The value.
  -> SerializationResponse  -- ^ The result.
roundtripSerialization typeSerialized valueSerialized =
  if isKnownJson knownJsonTypes typeSerialized
    then case roundTripJsonable knownJsonTypes typeSerialized valueSerialized of
             Right valueReserialized -> SerializationSuccess{..}
             Left serializationError -> SerializationError{..}
    else UnknownType typeSerialized


-- | List of known types that can be serialized and deserialized as JSON.
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
  , JsonableType "Core.Payment" (Proxy :: Proxy Marlowe.Payment)
  , JsonableType "Core.Transaction" (Proxy :: Proxy Marlowe.TransactionInput)
  , JsonableType "Core.TransactionOutput" (Proxy :: Proxy Marlowe.TransactionOutput)
  , JsonableType "Core.TransactionWarning" (Proxy :: Proxy Marlowe.TransactionWarning)
  , JsonableType "Core.TransactionError" (Proxy :: Proxy Marlowe.TransactionError)
  , JsonableType "Core.IntervalError" (Proxy :: Proxy Marlowe.IntervalError)
  ]
