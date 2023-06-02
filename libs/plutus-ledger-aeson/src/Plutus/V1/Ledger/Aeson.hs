{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Plutus.V1.Ledger.Aeson
  where

import Data.Aeson (FromJSON(parseJSON), FromJSONKey, ToJSON(toJSON), ToJSONKey, (.:))
import qualified Data.Aeson as JSON

import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Tx

import qualified Codec.CBOR.Write as CBOR.Write
import Codec.Serialise as Serialise
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as JSON
import qualified Data.ByteString as BS
import Data.ByteString.Base16.Aeson as Base16.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.Scientific (floatingOrInteger, scientific)
import Data.String (IsString(fromString))
import qualified Data.Text as Text
import qualified Data.Text.Encoding as E
import qualified Flat
import qualified Plutus.V1.Ledger.Bytes as Bytes
import Plutus.V1.Ledger.Scripts
import Plutus.V1.Ledger.Value
import qualified PlutusTx.AssocMap as Map
import qualified PlutusTx.Builtins
import PlutusTx.Builtins.Aeson ()

import Data.Hashable (Hashable)


{- Note [JSON instances for Script]
The JSON instances for Script are partially hand-written rather than going via the Serialise
instance directly. The reason for this is to *avoid* the size checks that are in place in the
Serialise instance. These are only useful for deserialisation checks on-chain, whereas the
JSON instances are used for e.g. transmitting validation events, which often include scripts
with the data arguments applied (which can be very big!).
-}
instance ToJSON Script where
    -- See note [JSON instances for Script]
    toJSON (Script p) = Base16.Aeson.byteStringToJSON . CBOR.Write.toStrictByteString . Serialise.encode . Flat.flat $ p

instance FromJSON Script where
    -- See note [JSON instances for Script]
    parseJSON v = do
      (EncodeBase16 bs) <- parseJSON v
      let
        lbs = BSL.fromStrict bs
      case Flat.unflatWith Flat.decode lbs of
        Left err  -> fail (show err)
        Right res -> pure $ Script res

deriving anyclass instance ToJSON DatumHash
deriving anyclass instance FromJSON DatumHash
deriving anyclass instance FromJSONKey DatumHash
deriving anyclass instance ToJSONKey DatumHash
deriving anyclass instance Serialise DatumHash

deriving anyclass instance ToJSON RedeemerHash
deriving anyclass instance FromJSON RedeemerHash
deriving anyclass instance ToJSONKey RedeemerHash
deriving anyclass instance FromJSONKey RedeemerHash
deriving anyclass instance Serialise RedeemerHash

deriving anyclass instance ToJSON MintingPolicyHash
deriving anyclass instance FromJSON MintingPolicyHash
deriving anyclass instance ToJSONKey MintingPolicyHash
deriving anyclass instance FromJSONKey MintingPolicyHash
deriving anyclass instance Serialise MintingPolicyHash

deriving anyclass instance ToJSON StakeValidatorHash
deriving anyclass instance FromJSON StakeValidatorHash
deriving anyclass instance ToJSONKey StakeValidatorHash
deriving anyclass instance FromJSONKey StakeValidatorHash
deriving anyclass instance Serialise StakeValidatorHash

deriving anyclass instance ToJSON ScriptHash
deriving anyclass instance FromJSON ScriptHash
deriving anyclass instance ToJSONKey ScriptHash
deriving anyclass instance FromJSONKey ScriptHash
deriving anyclass instance Serialise ScriptHash

deriving anyclass instance ToJSON ValidatorHash
deriving anyclass instance FromJSON ValidatorHash
deriving anyclass instance ToJSONKey ValidatorHash
deriving anyclass instance FromJSONKey ValidatorHash
deriving anyclass instance Serialise ValidatorHash

deriving newtype instance ToJSON Context
deriving newtype instance FromJSON Context

deriving anyclass instance ToJSON StakeValidator
deriving anyclass instance FromJSON StakeValidator

deriving anyclass instance ToJSON MintingPolicy
deriving anyclass instance FromJSON MintingPolicy

deriving anyclass instance ToJSON Validator
deriving anyclass instance FromJSON Validator

deriving anyclass instance ToJSON Redeemer
deriving anyclass instance FromJSON Redeemer
deriving anyclass instance Serialise Redeemer

deriving anyclass instance ToJSON Datum
deriving anyclass instance FromJSON Datum
deriving anyclass instance Serialise Datum


instance ToJSON CurrencySymbol where
  toJSON c =
    JSON.object
      [ ( "unCurrencySymbol"
        , Base16.Aeson.byteStringToJSON .
          PlutusTx.Builtins.fromBuiltin .
          unCurrencySymbol $
          c)
      ]

instance FromJSON CurrencySymbol where
  parseJSON =
    JSON.withObject "CurrencySymbol" $ \object -> do
      raw <- object .: "unCurrencySymbol"
      EncodeBase16 bytes <- parseJSON raw
      pure $ CurrencySymbol $ PlutusTx.Builtins.toBuiltin bytes

deriving anyclass instance Hashable CurrencySymbol
deriving newtype instance Serialise CurrencySymbol

deriving anyclass instance Hashable TokenName
deriving newtype instance Serialise TokenName

{- note [Roundtripping token names]
How to properly roundtrip a token name that is not valid UTF-8 through PureScript
without a big rewrite of the API?
We prefix it with a zero byte so we can recognize it when we get a bytestring value back,
and we serialize it base16 encoded, with 0x in front so it will look as a hex string.
(Browsers don't render the zero byte.)
-}

instance ToJSON TokenName where
    toJSON = JSON.object . pure . (,) "unTokenName" . JSON.toJSON .
        fromTokenName
            (Text.cons '\NUL' . asBase16)
            (\t -> case Text.take 1 t of "\NUL" -> Text.concat ["\NUL\NUL", t]; _ -> t)
      where
        -- copied from 'Plutus.V1.Ledger.Value' because not exported
        asBase16 :: BS.ByteString -> Text.Text
        asBase16 bs = Text.concat ["0x", Bytes.encodeByteString bs]

        fromTokenName :: (BS.ByteString -> r) -> (Text.Text -> r) -> TokenName -> r
        fromTokenName handleBytestring handleText (TokenName bs) =
          either (\_ -> handleBytestring $ PlutusTx.Builtins.fromBuiltin bs) handleText $ E.decodeUtf8' (PlutusTx.Builtins.fromBuiltin bs)

instance FromJSON TokenName where
    parseJSON =
        JSON.withObject "TokenName" $ \object -> do
        raw <- object .: "unTokenName"
        fromJSONText raw
        where
            fromText = tokenName . E.encodeUtf8 . Text.pack . fromString . Text.unpack
            fromJSONText t = case Text.take 3 t of
                "\NUL0x"       -> do
                  EncodeBase16 bs <- parseJSON (Aeson.String $ Text.drop 3 t)
                  pure $ tokenName bs
                "\NUL\NUL\NUL" -> pure . fromText . Text.drop 2 $ t
                _              -> pure . fromText $ t

deriving anyclass instance ToJSON AssetClass
deriving anyclass instance FromJSON AssetClass
deriving anyclass instance Hashable AssetClass
deriving newtype instance Serialise AssetClass

deriving anyclass instance ToJSON Value
deriving anyclass instance FromJSON Value
deriving anyclass instance Hashable Value
deriving newtype instance Serialise Value

-- Orphan instances for 'PlutusTx.Map' to make this work
instance (ToJSON v, ToJSON k) => ToJSON (Map.Map k v) where
    toJSON = JSON.toJSON . Map.toList

instance (FromJSON v, FromJSON k) => FromJSON (Map.Map k v) where
    parseJSON v = Map.fromList <$> JSON.parseJSON v

deriving anyclass instance (Hashable k, Hashable v) => Hashable (Map.Map k v)
deriving anyclass instance (Serialise k, Serialise v) => Serialise (Map.Map k v)

-- | Custom `FromJSON` instance which allows to parse a JSON number to a
-- 'POSIXTime' value. The parsed JSON value MUST be an 'Integer' or else the
-- parsing fails.
instance JSON.FromJSON POSIXTime where
  parseJSON v@(JSON.Number n) =
      either (\_ -> JSON.prependFailure "parsing POSIXTime failed, " (JSON.typeMismatch "Integer" v))
             (return . POSIXTime)
             (floatingOrInteger n :: Either Double Integer)
  parseJSON invalid =
      JSON.prependFailure "parsing POSIXTime failed, " (JSON.typeMismatch "Number" invalid)

-- | Custom 'ToJSON' instance which allows to simply convert a 'POSIXTime'
-- value to a JSON number.
instance JSON.ToJSON POSIXTime where
  toJSON (POSIXTime n) = JSON.Number $ scientific n 0

deriving newtype instance Serialise POSIXTime
deriving newtype instance Hashable POSIXTime

deriving anyclass instance JSON.ToJSON ScriptError
deriving anyclass instance JSON.FromJSON ScriptError


deriving anyclass instance (ToJSON a) => ToJSON (Interval a)
deriving anyclass instance (FromJSON a) => FromJSON (Interval a)

deriving anyclass instance (ToJSON a) => ToJSON (LowerBound a)
deriving anyclass instance (FromJSON a) => FromJSON (LowerBound a)

deriving anyclass instance (ToJSON a) => ToJSON (UpperBound a)
deriving anyclass instance (FromJSON a) => FromJSON (UpperBound a)

deriving anyclass instance (ToJSON a) => ToJSON (Extended a)
deriving anyclass instance (FromJSON a) => FromJSON (Extended a)


deriving newtype instance Serialise LedgerBytes
deriving anyclass instance FromJSONKey LedgerBytes
deriving anyclass instance ToJSONKey LedgerBytes

instance ToJSON LedgerBytes where
    toJSON = toJSON . EncodeBase16 . Bytes.bytes

instance FromJSON LedgerBytes where
    parseJSON v = do
      EncodeBase16 bs <- parseJSON v
      return $ Bytes.fromBytes bs

deriving anyclass instance ToJSON RedeemerPtr
deriving anyclass instance FromJSON RedeemerPtr
deriving anyclass instance ToJSONKey RedeemerPtr
deriving anyclass instance FromJSONKey RedeemerPtr
deriving anyclass instance Serialise RedeemerPtr

deriving anyclass instance ToJSON ScriptTag
deriving anyclass instance FromJSON ScriptTag
deriving anyclass instance Serialise ScriptTag

deriving anyclass instance ToJSON TxIn
deriving anyclass instance FromJSON TxIn
deriving anyclass instance Serialise TxIn

deriving anyclass instance ToJSON TxOut
deriving anyclass instance FromJSON TxOut
deriving anyclass instance Serialise TxOut

deriving anyclass instance ToJSON TxInType
deriving anyclass instance FromJSON TxInType
deriving anyclass instance Serialise TxInType

deriving anyclass instance ToJSON TxOutRef
deriving anyclass instance FromJSON TxOutRef
deriving anyclass instance ToJSONKey TxOutRef
deriving anyclass instance FromJSONKey TxOutRef
deriving anyclass instance Serialise TxOutRef

deriving anyclass instance ToJSON TxId
deriving anyclass instance FromJSON TxId
deriving anyclass instance ToJSONKey TxId
deriving anyclass instance FromJSONKey TxId
deriving anyclass instance Serialise TxId


deriving anyclass instance ToJSON Address
deriving anyclass instance FromJSON Address
deriving anyclass instance Serialise Address

deriving anyclass instance ToJSON Credential
deriving anyclass instance FromJSON Credential
deriving anyclass instance Serialise Credential

deriving anyclass instance ToJSON StakingCredential
deriving anyclass instance FromJSON StakingCredential
deriving anyclass instance Serialise StakingCredential

deriving anyclass instance ToJSON PubKeyHash
deriving anyclass instance FromJSON PubKeyHash
deriving anyclass instance FromJSONKey PubKeyHash
deriving anyclass instance ToJSONKey PubKeyHash
deriving newtype instance Serialise PubKeyHash

