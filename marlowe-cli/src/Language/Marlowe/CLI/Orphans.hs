{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Orphan instances for the Marlowe CLI tool.
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
module Language.Marlowe.CLI.Orphans (

) where

import Cardano.Api (
  AddressAny (..),
  AsType (AsAddressAny),
  BlockHeader (..),
  BlockNo (..),
  SlotNo (..),
  deserialiseAddress,
  serialiseAddress,
 )
import Data.Aeson (
  FromJSON (..),
  FromJSONKey,
  ToJSON (..),
  ToJSONKey,
  Value (..),
  object,
  withObject,
  withText,
  (.:),
  (.=),
 )
import Data.ByteString.Short (ShortByteString, fromShort, toShort)

import Codec.CBOR.Write qualified as Write
import Codec.Serialise qualified as S
import Control.Monad ((>=>))
import Data.Aeson qualified as JSON
import Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16 (decode, encode)
import Data.ByteString.Char8 qualified as BS8 (pack, unpack)
import Data.ByteString.Lazy qualified as BSL
import Data.Scientific (floatingOrInteger, scientific)
import Data.String (IsString (..))
import Data.Text qualified as T (unpack)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as E
import Plutus.V1.Ledger.SlotConfig (SlotConfig (..))
import PlutusLedgerApi.V1 (TokenName (..), fromBuiltin)
import PlutusLedgerApi.V1 qualified as PV1
import PlutusLedgerApi.V1.Bytes qualified as Bytes
import PlutusLedgerApi.V1.Value (AssetClass, tokenName)
import PlutusTx.AssocMap qualified as Map

{- note [Roundtripping token names]
How to properly roundtrip a token name that is not valid UTF-8 through PureScript
without a big rewrite of the API?
We prefix it with a zero byte so we can recognize it when we get a bytestring value back,
and we serialize it base16 encoded, with 0x in front so it will look as a hex string.
(Browsers don't render the zero byte.)
-}

instance ToJSON TokenName where
  toJSON =
    JSON.object
      . pure
      . (,) "unTokenName"
      . JSON.toJSON
      . fromTokenName
        (\bs -> Text.cons '\NUL' (asBase16 bs))
        (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)
    where
      -- copied from 'Plutus.V1.Ledger.Value' because not exported
      asBase16 :: BS.ByteString -> Text.Text
      asBase16 bs = Text.concat ["0x", Bytes.encodeByteString bs]

      fromTokenName :: (BS.ByteString -> r) -> (Text.Text -> r) -> TokenName -> r
      fromTokenName handleByteString handleText (TokenName bs) = either (\_ -> handleByteString $ fromBuiltin bs) handleText $ E.decodeUtf8' (fromBuiltin bs)

instance FromJSON TokenName where
  parseJSON =
    JSON.withObject "TokenName" $ \o -> do
      raw <- o .: "unTokenName"
      fromJSONText raw
    where
      fromText = tokenName . E.encodeUtf8 . Text.pack . fromString . Text.unpack
      fromJSONText t = case Text.take 3 t of
        "\NUL0x" -> either fail (pure . tokenName) . Base16.decode . E.encodeUtf8 . Text.drop 3 $ t
        "\NUL\NUL\NUL" -> pure . fromText . Text.drop 2 $ t
        _ -> pure . fromText $ t

instance ToJSON ShortByteString where
  toJSON = toJSON . BS8.unpack . Base16.encode . fromShort

instance FromJSON ShortByteString where
  parseJSON =
    withText "ShortByteString" $
      \t ->
        case Base16.decode . BS8.pack $ T.unpack t of
          Right bytes -> pure $ toShort bytes
          Left message -> fail message

instance ToJSON AddressAny where
  toJSON = String . serialiseAddress

instance FromJSON AddressAny where
  parseJSON =
    withText "AddressAny" $
      \s ->
        case deserialiseAddress AsAddressAny s of
          Just address -> pure address
          Nothing -> fail $ "Failed to parse address \"" <> T.unpack s <> "\"."

instance Show BlockHeader where
  show (BlockHeader slotNo blockHash blockNo) =
    "BlockHeader ("
      <> show slotNo
      <> ") ("
      <> show blockHash
      <> ") ("
      <> show blockNo
      <> ")"

instance ToJSON BlockHeader where
  toJSON (BlockHeader (SlotNo slotNo) blockHash (BlockNo blockNo)) =
    object
      [ "slot" .= slotNo
      , "hash" .= blockHash
      , "block" .= blockNo
      ]

instance FromJSON BlockHeader where
  parseJSON =
    withObject "BlockHeader" $
      \o ->
        BlockHeader
          <$> (SlotNo <$> o .: "slot")
          <*> (o .: "hash")
          <*> (BlockNo <$> o .: "block")

encodeByteString :: BS.ByteString -> Text.Text
encodeByteString = E.decodeUtf8 . Base16.encode

decodeByteString :: JSON.Value -> Parser BS.ByteString
decodeByteString = JSON.withText "ByteString" (either fail pure . Base16.decode . E.encodeUtf8)

instance ToJSON PV1.BuiltinByteString where
  toJSON = JSON.String . encodeByteString . PV1.fromBuiltin

instance FromJSON PV1.BuiltinByteString where
  parseJSON v = PV1.toBuiltin <$> decodeByteString v

-- 'POSIXTime' instances

-- | Custom `FromJSON` instance which allows to parse a JSON number to a
-- 'POSIXTime' value. The parsed JSON value MUST be an 'Integer' or else the
-- parsing fails.
instance JSON.FromJSON PV1.POSIXTime where
  parseJSON v@(JSON.Number n) =
    either
      (\_ -> prependFailure "parsing POSIXTime failed, " (typeMismatch "Integer" v))
      (return . PV1.POSIXTime)
      (floatingOrInteger n :: Either Double Integer)
  parseJSON invalid =
    prependFailure "parsing POSIXTime failed, " (typeMismatch "Number" invalid)

-- | Custom 'ToJSON' instance which allows to simply convert a 'POSIXTime'
-- value to a JSON number.
instance JSON.ToJSON PV1.POSIXTime where
  toJSON (PV1.POSIXTime n) = JSON.Number $ scientific n 0

instance ToJSON PV1.CurrencySymbol where
  toJSON c =
    JSON.object
      [
        ( "unCurrencySymbol"
        , JSON.String
            . encodeByteString
            . fromBuiltin
            . PV1.unCurrencySymbol
            $ c
        )
      ]

instance FromJSON PV1.CurrencySymbol where
  parseJSON =
    JSON.withObject "CurrencySymbol" $ \o -> do
      raw <- o .: "unCurrencySymbol"
      bytes <- decodeByteString raw
      pure $ PV1.CurrencySymbol $ PV1.toBuiltin bytes

instance ToJSON PV1.BuiltinData where
  toJSON = toJSON . PV1.builtinDataToData

instance FromJSON PV1.BuiltinData where
  parseJSON = fmap PV1.dataToBuiltinData . parseJSON

instance S.Serialise PV1.BuiltinData where
  encode = S.encode . PV1.builtinDataToData
  decode = PV1.dataToBuiltinData <$> S.decode

deriving anyclass instance ToJSON SlotConfig
deriving anyclass instance FromJSON SlotConfig
deriving anyclass instance ToJSON PV1.ScriptHash
deriving anyclass instance FromJSON PV1.ScriptHash
deriving anyclass instance ToJSONKey PV1.ScriptHash
deriving anyclass instance FromJSONKey PV1.ScriptHash
deriving anyclass instance ToJSON PV1.PubKeyHash
deriving anyclass instance FromJSON PV1.PubKeyHash
deriving anyclass instance FromJSONKey PV1.PubKeyHash
deriving anyclass instance ToJSONKey PV1.PubKeyHash
deriving anyclass instance ToJSON PV1.Credential
deriving anyclass instance FromJSON PV1.Credential
deriving anyclass instance ToJSON PV1.StakingCredential
deriving anyclass instance FromJSON PV1.StakingCredential
deriving anyclass instance ToJSON PV1.Address
deriving anyclass instance FromJSON PV1.Address
deriving anyclass instance ToJSON PV1.DatumHash
deriving anyclass instance FromJSON PV1.DatumHash
deriving anyclass instance ToJSONKey PV1.DatumHash
deriving anyclass instance FromJSONKey PV1.DatumHash
deriving anyclass instance ToJSON PV1.Datum
deriving anyclass instance FromJSON PV1.Datum
deriving anyclass instance ToJSON PV1.Redeemer
deriving anyclass instance FromJSON PV1.Redeemer
deriving via (JSONViaSerialise PV1.Data) instance ToJSON PV1.Data
deriving via (JSONViaSerialise PV1.Data) instance FromJSON PV1.Data

deriving anyclass instance ToJSON AssetClass
deriving anyclass instance FromJSON AssetClass

deriving anyclass instance ToJSON PV1.Value
deriving anyclass instance FromJSON PV1.Value

instance (ToJSON v, ToJSON k) => ToJSON (Map.Map k v) where
  toJSON = JSON.toJSON . Map.toList

instance (FromJSON v, FromJSON k) => FromJSON (Map.Map k v) where
  parseJSON v = Map.fromList <$> JSON.parseJSON v

-- | Newtype for deriving 'ToJSON' and 'FromJSON' for types that have a 'Serialise'
-- instance by just encoding the serialized bytes as a JSON string.
newtype JSONViaSerialise a = JSONViaSerialise a

encodeSerialise :: (S.Serialise a) => a -> Text.Text
encodeSerialise = encodeByteString . Write.toStrictByteString . S.encode

decodeSerialise :: (S.Serialise a) => JSON.Value -> Parser a
decodeSerialise = decodeByteString >=> go
  where
    go bs =
      case first show $ S.deserialiseOrFail $ BSL.fromStrict bs of
        Left e -> fail e
        Right v -> pure v

instance (S.Serialise a) => JSON.ToJSON (JSONViaSerialise a) where
  toJSON (JSONViaSerialise a) = JSON.String $ encodeSerialise a

instance (S.Serialise a) => JSON.FromJSON (JSONViaSerialise a) where
  parseJSON v = JSONViaSerialise <$> decodeSerialise v
