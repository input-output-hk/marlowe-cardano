module Types
  ( AjaxResponse
  , DecodedAjaxError
  , DecodedAjaxResponse
  , DecodedWebData
  , NotFoundAjaxError
  , NotFoundAjaxResponse
  , NotFoundWebData
  , WebData
  ) where

import Prologue
import Data.Argonaut.Decode (JsonDecodeError)
import Network.RemoteData (RemoteData)
import Servant.PureScript (AjaxError)

type AjaxResponse = Either AjaxError

type WebData = RemoteData AjaxError

type DecodedAjaxError = Either AjaxError JsonDecodeError

type DecodedAjaxResponse = Either DecodedAjaxError

type DecodedWebData = RemoteData DecodedAjaxError

type NotFoundAjaxError = Maybe AjaxError

type NotFoundAjaxResponse = Either NotFoundAjaxError

type NotFoundWebData = RemoteData NotFoundAjaxError
