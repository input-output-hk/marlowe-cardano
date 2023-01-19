-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Wrapping values that can be converted to and from JSON.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Data.Jsonable
  ( -- * Types
    Jsonable(..)
  , JsonableType(..)
  , KnownJsonable
    -- * Conversion
  , fromJsonable
  , isKnownJson
  , toJsonable
    -- * Testing
  , arbitraryJsonable
  , checkRoundTripJsonable
  , generateJsonable
  , roundTripJsonable
  ) where


import Data.Aeson (FromJSON(..), Result(..), ToJSON(..), Value, fromJSON)
import Data.Proxy (Proxy(..))
import Test.Tasty.QuickCheck (Arbitrary(..), Gen)


-- | A list of known types with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
type KnownJsonable = [JsonableType]


-- | A type with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
data JsonableType = forall a. (ToJSON a, FromJSON a, Arbitrary a) => JsonableType {typeKey :: String, typeValue :: Proxy a}


-- | A value with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
data Jsonable = forall a . (ToJSON a, FromJSON a, Arbitrary a) => Jsonable {jsonableType :: String, jsonableValue :: a}


-- | Deserialize a JSON value.
toJsonable
  :: KnownJsonable  -- ^ A list of known types with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
  -> String   -- ^ The key for the type.
  -> Value  -- ^ The JSON value.
  -> Either String Jsonable  -- ^ The deserialized value, or an error message.
toJsonable known s v =
  let
    fromJson' :: forall a . FromJSON a => Proxy a -> Either String a
    fromJson' _ =
      case fromJSON v :: Result a of
        Success x -> Right x
        Error msg -> Left msg
  in
    case filter ((s ==) . typeKey) known of
      [JsonableType{..}] -> Jsonable typeKey <$> fromJson' typeValue
      _                  -> Left $ "JSON serialization not supported for " <> s <> "."


-- | Check for a known JSON type.
isKnownJson
  :: KnownJsonable  -- ^ A list of known types with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
  -> String   -- ^ The key for the type.
  -> Bool  -- ^ Whether the type is in the list of known JSON types.
isKnownJson known s = length (filter ((s ==) . typeKey) known) == 1


-- | Serialize a JSON value.
fromJsonable
  :: Jsonable  -- ^ The value.
  -> Value  -- ^ The JSON.
fromJsonable Jsonable{..} = toJSON jsonableValue


-- | Peform round-trip deserialization and serialization of a JSON value.
roundTripJsonable
  :: KnownJsonable  -- ^ A list of known types with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
  -> String  -- ^ The key for the type.
  -> Value  -- ^ The JSON value.
  -> Either String Value  -- ^ There the re-serialized value, or an error message.
roundTripJsonable known = (fmap fromJsonable .) . toJsonable known


-- | Check that round-trip re-serialization doesn't alter a JSON value.
checkRoundTripJsonable
  :: KnownJsonable  -- ^ A list of known types with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
  -> String  -- ^ The key for the type.
  -> Value  -- ^ The JSON value.
  -> Bool  -- ^ Whether the JSON value is unchanged.
checkRoundTripJsonable known s v = roundTripJsonable known s v == Right v


-- | Generate an arbitrary value.
arbitraryJsonable
  :: JsonableType  -- ^ The type to generate.
  -> Gen Jsonable  -- ^ The generator.
arbitraryJsonable JsonableType{..} =
  let
    gen :: Arbitrary a => Proxy a -> Gen a
    gen _ = arbitrary
  in
    Jsonable typeKey <$> gen typeValue


-- | Generate an arbitrary value.
generateJsonable
  :: KnownJsonable  -- ^ A list of known types with `FromJSON`, `ToJSON`, and `Arbitrary` instances.
  -> String  -- ^ The key for the type.
  -> Either String (Gen Value)  -- ^ The JSON value.
generateJsonable known s =
  case filter ((s ==) . typeKey) known of
    [jt] -> Right . fmap fromJsonable $ arbitraryJsonable jt
    _    -> Left $ "JSON serialization not supported for " <> s <> "."
