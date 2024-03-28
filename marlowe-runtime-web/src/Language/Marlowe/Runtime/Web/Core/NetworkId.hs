{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId (..)) where

import Control.Lens ((&), (?~))
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  Referenced (..),
  ToParamSchema,
  ToSchema,
  declareSchemaRef,
  enum_,
  oneOf,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import Data.Word (Word32)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData,
  Proxy (..),
  ToHttpApiData (toUrlPiece),
 )
import Servant.API (FromHttpApiData (..))

data NetworkId
  = Mainnet
  | Testnet Word32
  deriving (Show, Eq, Ord)

instance ToSchema NetworkId where
  declareNamedSchema _ = do
    let mainnetSchema =
          mempty
            & type_ ?~ OpenApiString
            & enum_ ?~ ["mainnet"]
    testnetSchema <- declareSchemaRef (Proxy @Word32)
    pure $
      NamedSchema (Just "NetworkId") $
        mempty
          & oneOf ?~ [Inline mainnetSchema, testnetSchema]

instance ToHttpApiData NetworkId where
  toUrlPiece = \case
    Mainnet -> "mainnet"
    Testnet n -> toUrlPiece n

instance FromHttpApiData NetworkId where
  parseUrlPiece = \case
    "mainnet" -> pure Mainnet
    n -> Testnet <$> parseUrlPiece n

instance ToParamSchema NetworkId where
  toParamSchema _ =
    mempty
      & oneOf ?~ [Inline (mempty & type_ ?~ OpenApiString), Inline (mempty & type_ ?~ OpenApiInteger)]
      & OpenApi.description ?~ "The latest known point in the chain on a peer."
