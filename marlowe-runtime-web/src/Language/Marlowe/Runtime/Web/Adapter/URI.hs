module Language.Marlowe.Runtime.Web.Adapter.URI (
  uriFromJSON,
  uriToJSON,
) where

import Data.Aeson.Types (
  Parser,
  Value (String),
  parseFail,
  withText,
 )
import Network.URI (parseURI)
import Servant (URI)

import qualified Data.Text as T

uriFromJSON :: Value -> Parser URI
uriFromJSON = withText "URI" $ maybe (parseFail "invalid URI") pure . parseURI . T.unpack

uriToJSON :: URI -> Value
uriToJSON = String . T.pack . show
