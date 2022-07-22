module Main
  ( Request(..)
  , SerializationResponse(..)
  , TypeId(..)
  , main
  , testRoundtripSerializationJson
  ) where

import Prelude

import Data.Argonaut
  ( class DecodeJson
  , class EncodeJson
  , Json
  , decodeJson
  , encodeJson
  , printJsonDecodeError
  , stringify
  )
import Data.Argonaut.Extra (getProp, object, requireProp)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import JsonStream (createJsonStream)
import Language.Marlowe.Core.V1.Semantics (computeTransaction) as C
import Language.Marlowe.Core.V1.Semantics.Types
  ( Contract
  , State
  , TransactionInput
  ) as C
import Language.Marlowe.Extended.V1 as EM
import Node.Process (stdin)
import Type.Proxy (Proxy(..))

newtype TypeId = TypeId String

derive newtype instance Show TypeId
instance DecodeJson TypeId where
  decodeJson j = TypeId <$> decodeJson j

data Request transport
  = TestRoundtripSerialization TypeId transport
  | GenerateRandomValue TypeId
  | ComputeTransaction C.TransactionInput C.State C.Contract

instance DecodeJson (Request Json) where
  decodeJson =
    object "Request" do
      requestType <- requireProp "request"
      mTypeId <- getProp "typeId"
      mJson <- getProp "json"
      mTransactionInput <- getProp "transactionInput"
      mState <- getProp "state"
      mContract <- getProp "coreContract"
      case requestType of
        "test-roundtrip-serialization" -> pure $
          (TestRoundtripSerialization <$> mTypeId <*> mJson)
        "generate-random-value" -> pure $ (GenerateRandomValue <$> mTypeId)
        "compute-transaction" -> pure $
          (ComputeTransaction <$> mTransactionInput <*> mState <*> mContract)
        _ -> pure Nothing

-- type Response transport
--   = Either ResponseError (ResponseSuccess transport)

-- data ResponseSuccess transport
--   = SerializationSuccess TypeId transport
--   | RandomValue TypeId transport

data SerializationResponse transport
  = SerializationSuccess transport
  | UnknownType TypeId
  | SerializationError String

testRoundtripSerializationJson :: TypeId -> Json -> SerializationResponse Json
testRoundtripSerializationJson typeId = case typeId of
  TypeId "Extended.Timeout" -> roundTrip (Proxy :: Proxy EM.Timeout)
  TypeId "Extended.Value" -> roundTrip (Proxy :: Proxy EM.Value)
  TypeId "Extended.Observation" -> roundTrip (Proxy :: Proxy EM.Observation)
  TypeId "Extended.Action" -> roundTrip (Proxy :: Proxy EM.Action)
  TypeId "Extended.Payee" -> roundTrip (Proxy :: Proxy EM.Payee)
  TypeId "Extended.Case" -> roundTrip (Proxy :: Proxy EM.Case)
  TypeId "Extended.Contract" -> roundTrip (Proxy :: Proxy EM.Contract)
  TypeId "Extended.Module" -> roundTrip (Proxy :: Proxy EM.Module)

  _ -> const $ UnknownType typeId
  where
  roundTrip
    :: forall a
     . DecodeJson a
    => EncodeJson a
    => Proxy a
    -> Json
    -> SerializationResponse Json
  roundTrip _ json = case decodeJson json of
    Right (x :: a) -> SerializationSuccess $ encodeJson x
    Left err -> SerializationError $ printJsonDecodeError err

handleRequest :: Json -> Effect Unit
handleRequest req = case decodeJson req of
  Left _ -> log "cant understand request"
  Right (TestRoundtripSerialization typeId json) ->
    case testRoundtripSerializationJson typeId json of
      SerializationSuccess jsonBack -> log $ stringify jsonBack
      UnknownType _ -> log $ "unknown type " <> show typeId
      SerializationError err -> log $ "serialization error: " <> err
  Right (GenerateRandomValue _) -> log "not implemented"
  Right (ComputeTransaction input state contract) -> log $ stringify
    $ encodeJson
    $ C.computeTransaction input state contract

main :: Effect Unit
main = createJsonStream
  { stream: stdin
  , slizeSize: 4096
  , beginSeparator: "```"
  , endSeparator: "```"
  , onJson: handleRequest
  , onError: \err -> log $ "Error: " <> show err
  , onFinish: log "Finished!"
  }
