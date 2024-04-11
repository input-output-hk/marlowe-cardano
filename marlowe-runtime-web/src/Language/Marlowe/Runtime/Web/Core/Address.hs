{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Marlowe.Runtime.Web.Core.Address (
  Address (..),
  StakeAddress (..),
) where

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  ToParamSchema,
  ToSchema,
  example,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (
  FromHttpApiData,
 )
import Servant.API (ToHttpApiData)

newtype Address = Address {unAddress :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

instance ToSchema Address where
  declareNamedSchema = pure . NamedSchema (Just "Address") . toParamSchema

instance ToParamSchema Address where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "A cardano address, in Bech32 format"
      & example ?~ "addr1w94f8ywk4fg672xasahtk4t9k6w3aql943uxz5rt62d4dvq8evxaf"

newtype StakeAddress = StakeAddress {unStakeAddress :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON)

instance ToSchema StakeAddress where
  declareNamedSchema = pure . NamedSchema (Just "StakeAddress") . toParamSchema

instance ToParamSchema StakeAddress where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "A cardano stake address, in Bech32 format"
      & example ?~ "stake1ux7lyy9nhecm033qsmel9awnr22up6jadlzkrxufr78w82gsfsn0d"
