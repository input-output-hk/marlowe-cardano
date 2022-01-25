module Types where

import Prologue

import Data.Argonaut (Json)
import Data.Argonaut.Decode (JsonDecodeError)
import Network.RemoteData (RemoteData)
import Servant.PureScript (AjaxError)

type JsonAjaxError = AjaxError JsonDecodeError Json

type AjaxResponse
  = Either JsonAjaxError

type WebData
  = RemoteData JsonAjaxError

type DecodedAjaxError
  = Either JsonAjaxError JsonDecodeError

type DecodedAjaxResponse
  = Either DecodedAjaxError

type DecodedWebData
  = RemoteData DecodedAjaxError

type NotFoundAjaxError
  = Maybe JsonAjaxError

type NotFoundAjaxResponse
  = Either NotFoundAjaxError

type NotFoundWebData
  = RemoteData NotFoundAjaxError
