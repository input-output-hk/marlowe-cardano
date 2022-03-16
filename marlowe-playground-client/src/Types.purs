module Types where

import Data.Argonaut (Json, JsonDecodeError)
import Network.RemoteData (RemoteData)
import Servant.PureScript (AjaxError)

type JsonAjaxError = AjaxError JsonDecodeError Json

data WarningAnalysisError
  = WarningAnalysisAjaxError JsonAjaxError
  | WarningAnalysisIsExtendedMarloweError

type WebData = RemoteData JsonAjaxError

type WarningAnalysisData = RemoteData WarningAnalysisError

data MarloweError = MarloweError String
