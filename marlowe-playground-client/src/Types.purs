module Types where

import Network.RemoteData (RemoteData)
import Servant.PureScript (AjaxError)

data WarningAnalysisError
  = WarningAnalysisAjaxError AjaxError
  | WarningAnalysisIsExtendedMarloweError

type WebData = RemoteData AjaxError

type WarningAnalysisData = RemoteData WarningAnalysisError

data MarloweError
  = MarloweError String
