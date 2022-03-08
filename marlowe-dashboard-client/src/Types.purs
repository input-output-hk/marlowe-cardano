module Types where

import Prologue

import Data.Argonaut (Json, fromString)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Variant (Variant)
import Data.Variant as Variant
import Errors (class Debuggable, class Explain)
import Servant.PureScript (AjaxError)
import Text.Pretty (text)
import Type.Proxy (Proxy(..))

type JsonAjaxError = AjaxError JsonDecodeError Json

type JsonAjaxErrorRow r = (jsonAjaxError :: AjaxError JsonDecodeError Json | r)

jsonAjaxError :: forall r. JsonAjaxError -> Variant (JsonAjaxErrorRow r)
jsonAjaxError = Variant.inj (Proxy :: Proxy "jsonAjaxError")

data MetadataNotFoundError = MetadataNotFoundError

instance Explain MetadataNotFoundError where
  explain _ = text
    "We weren't able to find the necesary information to display the contract"

-- TODO: Right now the metadata is tried to be found via a Contract per se, eventually
--       we will use a hash, and when we do, we could add that information here
instance Debuggable MetadataNotFoundError where
  debuggable _ = fromString "Contract metadata not found"

type MetadataNotFoundErrorRow r =
  (metadataNotFoundError :: MetadataNotFoundError | r)

metadataNotFoundError :: forall r. Variant (MetadataNotFoundErrorRow r)
metadataNotFoundError = Variant.inj (Proxy :: Proxy "metadataNotFoundError")
  MetadataNotFoundError

type JsonDecodeErrorRow r = (jsonDecodeError :: JsonDecodeError | r)

jsonDecodeError :: forall r. JsonDecodeError -> Variant (JsonDecodeErrorRow r)
jsonDecodeError = Variant.inj (Proxy :: Proxy "jsonDecodeError")

type AjaxResponse
  = Either JsonAjaxError

type DecodedAjaxError
  = Either JsonAjaxError JsonDecodeError

type DecodedAjaxResponse
  = Either DecodedAjaxError

