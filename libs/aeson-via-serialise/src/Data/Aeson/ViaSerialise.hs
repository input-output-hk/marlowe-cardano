{-# LANGUAGE ImportQualifiedPost #-}

module Data.Aeson.ViaSerialise
  where

import qualified Codec.CBOR.Write as CBOR.Write
import Codec.Serialise (Serialise, decode, deserialiseOrFail, encode)
import qualified Data.Aeson as Aeson
import Data.ByteString.Base16.Aeson as Base16.Aeson
import qualified Data.ByteString.Lazy as BSL

newtype ViaSerialise a = ViaSerialise a

unViaSerialise (ViaSerialise a) = a

instance Serialise a => Aeson.ToJSON (ViaSerialise a) where
    toJSON (ViaSerialise a) = Base16.Aeson.byteStringToJSON . CBOR.Write.toStrictByteString . encode $ a

instance Serialise a => Aeson.FromJSON (ViaSerialise a) where
    parseJSON v = ViaSerialise <$> do
      EncodeBase16 bs <- Aeson.parseJSON v
      case deserialiseOrFail . BSL.fromStrict $ bs of
        Left err  -> fail (show err)
        Right res -> pure res


encodeToJSON :: Serialise a => a -> Aeson.Value
encodeToJSON a = Aeson.toJSON (ViaSerialise a)

decodeToJSON :: Serialise a => Aeson.Value -> Aeson.Result a
decodeToJSON v = unViaSerialise <$> Aeson.fromJSON v
