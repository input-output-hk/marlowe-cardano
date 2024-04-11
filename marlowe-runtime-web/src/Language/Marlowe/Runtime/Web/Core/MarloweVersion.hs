module Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion (..)) where

import Control.DeepSeq (NFData)
import Control.Lens ((&), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  withText,
 )
import Data.Aeson.Types (parseFail)
import Data.Foldable (fold)
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  ToSchema,
  enum_,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import Data.Text (intercalate)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant (
  FromHttpApiData (parseUrlPiece),
  ToHttpApiData (toUrlPiece),
 )

data MarloweVersion = V1
  deriving (Show, Eq, Ord, Generic)

instance NFData MarloweVersion

instance ToJSON MarloweVersion where
  toJSON V1 = String "v1"

instance FromJSON MarloweVersion where
  parseJSON =
    withText "MarloweVersion" $ either (parseFail . T.unpack) pure . parseUrlPiece

instance ToHttpApiData MarloweVersion where
  toUrlPiece V1 = "v1"

instance FromHttpApiData MarloweVersion where
  parseUrlPiece "v1" = Right V1
  parseUrlPiece _ =
    Left $
      fold @[]
        [ "expected one of "
        , intercalate "; " ["v1"]
        ]

instance ToSchema MarloweVersion where
  declareNamedSchema _ =
    pure $
      NamedSchema (Just "MarloweVersion") $
        mempty
          & type_ ?~ OpenApiString
          & OpenApi.description ?~ "A version of the Marlowe language."
          & enum_ ?~ ["v1"]
