{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Core.Tip (ChainTip (..)) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  KeyValue ((.=)),
  ToJSON (toJSON),
  eitherDecodeStrict,
  object,
  withObject,
  (.:?),
 )
import Data.Aeson.Text (encodeToLazyText)
import Data.Aeson.Types (parseFail)
import Data.Bifunctor (first)
import Data.OpenApi (
  HasType (..),
  OpenApiType (..),
  Reference (..),
  Referenced (..),
  ToParamSchema,
  oneOf,
  properties,
  required,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as TL
import Data.Time (UTCTime)
import Data.Time.Format.ISO8601 (iso8601Show)

import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData (parseUrlPiece),
  Proxy (..),
  ToHttpApiData (toUrlPiece),
 )

import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )

data ChainTip
  = ChainTipGenesis UTCTime
  | ChainTip BlockHeader UTCTime
  deriving (Show, Eq, Ord)

instance ToJSON ChainTip where
  toJSON = \case
    ChainTipGenesis time -> object ["genesisTimeUTC" .= iso8601Show time]
    ChainTip blockHeader time ->
      object
        [ "blockHeader" .= blockHeader
        , "slotTimeUTC" .= iso8601Show time
        ]

instance FromJSON ChainTip where
  parseJSON =
    withObject
      "ChainTip"
      ( \obj -> do
          genesisTimeUTC <- obj .:? "genesisTimeUTC"
          blockHeader <- obj .:? "blockHeader"
          slotTimeUTC <- obj .:? "slotTimeUTC"
          case (genesisTimeUTC, blockHeader, slotTimeUTC) of
            (Nothing, Just blockHeader', Just slotTimeUTC') -> pure $ ChainTip blockHeader' slotTimeUTC'
            (Just genesisTimeUTC', Nothing, Nothing) -> pure $ ChainTipGenesis genesisTimeUTC'
            _ -> parseFail "Invalid keys, expecting ([\"genesisTimeUTC\"] | [\"blockHeader\", \"slotTimeUTC\"])"
      )

instance ToHttpApiData ChainTip where
  toUrlPiece = TL.toStrict . encodeToLazyText

instance FromHttpApiData ChainTip where
  parseUrlPiece = first T.pack . eitherDecodeStrict . encodeUtf8

instance ToParamSchema ChainTip where
  toParamSchema _ =
    mempty
      & oneOf ?~ [Inline genesisSchema, Inline tipSchema]
      & OpenApi.description ?~ "The latest known point in the chain on a peer."
    where
      genesisSchema =
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("genesisTimeUTC", Inline $ toParamSchema $ Proxy @UTCTime)
               ]
          & required .~ ["genesisTimeUTC"]

      tipSchema =
        mempty
          & type_ ?~ OpenApiObject
          & properties
            .~ [ ("blockHeader", Ref $ Reference "BlockHeader")
               , ("slotTimeUTC", Inline $ toParamSchema $ Proxy @UTCTime)
               ]
          & required .~ ["blockHeader", "slotTimeUTC"]
