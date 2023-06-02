{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
module PlutusTx.Builtins.Aeson
  where

import PlutusCore.Data.Aeson ()
import qualified PlutusTx.Prelude as PlutusTx

import Data.ByteString.Base16.Aeson (EncodeBase16(..))
import Data.ByteString.Base16.Aeson as Base16.Aeson

import Codec.Serialise (Serialise(decode, encode))
import Data.Aeson (FromJSON(parseJSON), ToJSON(toJSON))
import qualified Data.Aeson as JSON
import qualified Data.Functor
import PlutusCore.Data
import qualified PlutusTx
import PlutusTx.Builtins.Internal (BuiltinData(..))

instance ToJSON PlutusTx.BuiltinByteString where
    toJSON = Base16.Aeson.byteStringToJSON . PlutusTx.fromBuiltin

instance FromJSON PlutusTx.BuiltinByteString where
    parseJSON v = do
      EncodeBase16 bs <- parseJSON v
      pure $ PlutusTx.toBuiltin bs

instance ToJSON PlutusTx.BuiltinData where
  toJSON = toJSON . PlutusTx.builtinDataToData

instance FromJSON PlutusTx.BuiltinData where
  parseJSON v = parseJSON v Data.Functor.<&> PlutusTx.dataToBuiltinData

instance Serialise PlutusTx.BuiltinData where
  encode = encode . PlutusTx.builtinDataToData
  decode = PlutusTx.dataToBuiltinData <$> decode


