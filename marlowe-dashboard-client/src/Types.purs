module Types where

import Prologue

import Data.Argonaut (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Data.Variant (Variant)
import Data.Variant as Variant
import Servant.PureScript (AjaxError)
import Type.Proxy (Proxy(..))

type JsonAjaxError = AjaxError JsonDecodeError Json

type JsonAjaxErrorRow r = (jsonAjaxError :: AjaxError JsonDecodeError Json | r)

jsonAjaxError :: forall r. JsonAjaxError -> Variant (JsonAjaxErrorRow r)
jsonAjaxError = Variant.inj (Proxy :: Proxy "jsonAjaxError")

type MetadataNotFoundErrorRow r = (metadataNotFoundError :: Unit | r)

metadataNotFoundError :: forall r. Variant (MetadataNotFoundErrorRow r)
metadataNotFoundError = Variant.inj (Proxy :: Proxy "metadataNotFoundError")
  unit

type JsonDecodeErrorRow r = (jsonDecodeError :: JsonDecodeError | r)

jsonDecodeError :: forall r. JsonDecodeError -> Variant (JsonDecodeErrorRow r)
jsonDecodeError = Variant.inj (Proxy :: Proxy "jsonDecodeError")

type AjaxResponse
  = Either JsonAjaxError

type DecodedAjaxError
  = Either JsonAjaxError JsonDecodeError

type DecodedAjaxResponse
  = Either DecodedAjaxError

