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

module Language.Marlowe.Runtime.Web.Core.Party (
  Party (..),
) where

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, FromJSONKey, ToJSON, ToJSONKey)
import Data.OpenApi (
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiString),
  ToParamSchema (..),
  ToSchema (..),
 )
import qualified Data.OpenApi as OpenApi
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData, ToHttpApiData)

newtype Party = Party {unParty :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, ToHttpApiData, FromHttpApiData, ToJSON, FromJSON, FromJSONKey, ToJSONKey)

instance ToSchema Party where
  declareNamedSchema proxy = pure $ NamedSchema (Just "Party") $ toParamSchema proxy

instance ToParamSchema Party where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "Party (A role name or an Address)"
