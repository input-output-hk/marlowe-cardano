{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.Web.Adapter.CommaList (
  CommaList (..),
) where

import Control.Lens ((&), (?~))
import Data.Aeson (FromJSON, ToJSON)
import Data.Char (isSpace)
import Data.OpenApi (
  HasType (..),
  OpenApiType (..),
  ToParamSchema,
  ToSchema,
  toParamSchema,
 )
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsList)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData (parseQueryParam, parseUrlPiece),
  ToHttpApiData (toQueryParam, toUrlPiece),
 )

newtype CommaList a = CommaList {unCommaList :: [a]}
  deriving (Eq, Ord, Generic, Functor)
  deriving newtype (Show, ToJSON, FromJSON, IsList)

instance ToParamSchema (CommaList a) where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & OpenApi.description ?~ "A comma-separated list of values"

instance (ToSchema a) => ToSchema (CommaList a)

instance (ToHttpApiData a) => ToHttpApiData (CommaList a) where
  toUrlPiece = T.intercalate "," . fmap toUrlPiece . unCommaList
  toQueryParam = T.intercalate "," . fmap toQueryParam . unCommaList

instance (FromHttpApiData a) => FromHttpApiData (CommaList a) where
  parseUrlPiece =
    fmap CommaList
      . traverse (parseUrlPiece . T.dropWhileEnd isSpace . T.dropWhile isSpace)
      . splitOnNonEmpty ","
  parseQueryParam =
    fmap CommaList
      . traverse (parseQueryParam . T.dropWhileEnd isSpace . T.dropWhile isSpace)
      . splitOnNonEmpty ","

splitOnNonEmpty :: Text -> Text -> [Text]
splitOnNonEmpty sep t
  | T.null t = []
  | otherwise = T.splitOn sep t
