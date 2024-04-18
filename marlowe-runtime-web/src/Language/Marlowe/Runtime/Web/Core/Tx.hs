{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Marlowe.Runtime.Web.Core.Tx (
  TxBodyInAnyEra (..),
  TxOutRef (..),
  TxId (..),
  TextEnvelope (..),
  TxStatus (..),
) where

import Control.Lens ((&), (.~), (?~))
import Control.Monad ((<=<))
import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey (fromJSONKey),
  FromJSONKeyFunction (FromJSONKeyTextParser),
  ToJSON (toJSON),
  ToJSONKey (toJSONKey),
  Value (String),
  object,
  withObject,
  withText,
  (.:),
 )
import Data.Aeson.Types (parseFail, toJSONKeyText)
import Data.ByteString (ByteString)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..))
import Servant (
  FromHttpApiData (parseUrlPiece),
  Proxy (..),
  ToHttpApiData (toUrlPiece),
 )

import Cardano.Api (IsShelleyBasedEra, TxBody)
import Control.DeepSeq (NFData)
import Data.OpenApi (
  HasEnum (enum_),
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  ToParamSchema,
  ToSchema,
  declareSchemaRef,
  example,
  pattern,
  properties,
  required,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import Data.Text (Text, splitOn)
import qualified Data.Text as T
import Data.Word (Word16)
import Language.Marlowe.Runtime.Web.Adapter.ByteString (hasLength)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()

data TxBodyInAnyEra where
  TxBodyInAnyEra :: (IsShelleyBasedEra era) => TxBody era -> TxBodyInAnyEra

newtype TxId = TxId {unTxId :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON) via Base16
instance NFData TxId

data TxOutRef = TxOutRef
  { txId :: TxId
  , txIx :: Word16
  }
  deriving (Show, Eq, Ord, Generic)

instance NFData TxOutRef

data TextEnvelope = TextEnvelope
  { teType :: Text
  , teDescription :: Text
  , teCborHex :: Base16
  }
  deriving (Show, Eq, Ord, Generic)

data TxStatus
  = Unsigned
  | Submitted
  | Confirmed
  deriving (Show, Eq, Ord, Generic)

instance NFData TxStatus

instance FromHttpApiData TxOutRef where
  parseUrlPiece t = case splitOn "#" t of
    [idText, ixText] -> TxOutRef <$> parseUrlPiece idText <*> parseUrlPiece ixText
    _ -> case parseUrlPiece @TxId t of
      Right _ -> Left "Expected [a-fA-F0-9]{64}#[0-9]+ (hint: do you need to URL-encode the '#' as \"%23\"?)"
      _ -> Left "Expected [a-fA-F0-9]{64}#[0-9]+"

instance ToHttpApiData TxOutRef where
  toUrlPiece TxOutRef{..} = toUrlPiece txId <> "#" <> toUrlPiece txIx

instance FromJSON TxOutRef where
  parseJSON =
    withText "TxOutRef" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance FromJSONKey TxOutRef where
  fromJSONKey = FromJSONKeyTextParser $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema TxOutRef where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "TxOutRef") $
        mempty
          & type_ ?~ OpenApiString
          & OpenApi.description
            ?~ "A reference to a transaction output with a transaction ID and index."
          & pattern ?~ "^[a-fA-F0-9]{64}#[0-9]+$"
          & example ?~ "98d601c9307dd43307cf68a03aad0086d4e07a789b66919ccf9f7f7676577eb7#1"

instance ToParamSchema TxOutRef where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description
        ?~ "A reference to a transaction output with a transaction ID and index. The value must be URL encoded by replacing the '#' character with %23."
      & pattern ?~ "^[a-fA-F0-9]{64}%23[0-9]+$"
      & example ?~ "98d601c9307dd43307cf68a03aad0086d4e07a789b66919ccf9f7f7676577eb7%231"

instance ToJSON TxOutRef where
  toJSON = String . toUrlPiece

instance ToJSONKey TxOutRef where
  toJSONKey = toJSONKeyText toUrlPiece

instance FromHttpApiData TxId where
  parseUrlPiece = fmap TxId . (hasLength 32 . unBase16 <=< parseUrlPiece)

instance FromJSON TxId where
  parseJSON =
    withText "TxId" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToSchema TxId where
  declareNamedSchema = pure . NamedSchema (Just "TxId") . toParamSchema

instance ToParamSchema TxId where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded identifier of a Cardano transaction"
      & pattern ?~ "^[a-fA-F0-9]{64}$"

instance ToJSON TextEnvelope where
  toJSON TextEnvelope{..} =
    object
      [ ("type", toJSON teType)
      , ("description", toJSON teDescription)
      , ("cborHex", toJSON teCborHex)
      ]

instance FromJSON TextEnvelope where
  parseJSON =
    withObject
      "TextEnvelope"
      ( \obj ->
          TextEnvelope
            <$> obj
              .: "type"
            <*> obj
              .: "description"
            <*> obj
              .: "cborHex"
      )

instance ToSchema TextEnvelope where
  declareNamedSchema _ = do
    textSchema <- declareSchemaRef (Proxy @Text)
    let typeSchema =
          mempty
            & type_ ?~ OpenApiString
            & OpenApi.description
              ?~ "What type of data is encoded in the CBOR Hex. Valid values include \"Tx <era>\", \"TxBody <era>\", and \"ShelleyTxWitness <era>\" where <era> is one of \"BabbageEra\", \"ConwayEra\"."
    pure $
      NamedSchema (Just "TextEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & required .~ ["type", "description", "cborHex"]
          & properties
            .~ [ ("type", Inline typeSchema)
               , ("description", textSchema)
               , ("cborHex", textSchema)
               ]

instance ToJSON TxStatus where
  toJSON Unsigned = String "unsigned"
  toJSON Submitted = String "submitted"
  toJSON Confirmed = String "confirmed"

instance FromJSON TxStatus where
  parseJSON (String "unsigned") = pure Unsigned
  parseJSON (String "submitted") = pure Submitted
  parseJSON (String "confirmed") = pure Confirmed
  parseJSON _ = parseFail "invalid status"

instance ToSchema TxStatus where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "TxStatus") $
        mempty
          & type_ ?~ OpenApiString
          & enum_ ?~ ["unsigned", "submitted", "confirmed"]
          & OpenApi.description ?~ "The status of a transaction on the local node."
