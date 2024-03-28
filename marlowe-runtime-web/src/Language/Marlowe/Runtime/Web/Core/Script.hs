{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Core.Script (ScriptHash (..)) where

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString (ByteString)
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  ToParamSchema,
  ToSchema,
  pattern,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (ToHttpApiData)

import Language.Marlowe.Runtime.Web.Core.Base16 (Base16 (..))

newtype ScriptHash = ScriptHash {unScriptHash :: ByteString}
  deriving (Eq, Ord, Generic)
  deriving (Show, ToHttpApiData, ToJSON, FromJSON) via Base16

instance ToSchema ScriptHash where
  declareNamedSchema proxy = pure $ NamedSchema (Just "ScriptHash") $ toParamSchema proxy

instance ToParamSchema ScriptHash where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "The hex-encoded hash of a Plutus script"
      & pattern ?~ "^[a-fA-F0-9]*$"
