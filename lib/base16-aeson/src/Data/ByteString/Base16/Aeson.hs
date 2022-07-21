-- | Encoding and decoding of 'ByteString' and serialisable values
--   as base16 encoded JSON strings
module Data.ByteString.Base16.Aeson
  ( byteStringToJSON
  , byteStringFromJSON
  , Base16(..)
  , unBase16
  )
  where

import Data.Aeson (FromJSON, ToJSON (toJSON))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as A
import Data.Bifunctor (first)
import qualified Data.ByteString as BSS
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE

newtype Base16 = Base16 BSS.ByteString

unBase16 (Base16 bs) = bs

instance ToJSON Base16 where
  toJSON (Base16 bs) = toJSON . encodeBase16 $ bs

instance FromJSON Base16 where
  parseJSON v = do
    let base16Parser = either (fail . T.unpack) pure . decodeBase16 . TE.encodeUtf8
    Base16 <$> Aeson.withText "ByteString" base16Parser v


byteStringToJSON :: BSS.ByteString -> Aeson.Value
byteStringToJSON = toJSON . Base16

byteStringFromJSON :: Aeson.Value -> Aeson.Result BSS.ByteString
byteStringFromJSON v = unBase16 <$> Aeson.fromJSON v
