module Types where

import Prelude

import Data.Argonaut (Json, JsonDecodeError)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Network.RemoteData (RemoteData)
import Servant.PureScript (AjaxError)

type JsonAjaxError = AjaxError JsonDecodeError Json

data WarningAnalysisError
  = WarningAnalysisAjaxError JsonAjaxError
  | WarningAnalysisIsExtendedMarloweError

type WebData = RemoteData JsonAjaxError

type WarningAnalysisData = RemoteData WarningAnalysisError

data MarloweError = MarloweError String

data WebpackBuildMode = Production | Development

derive instance Generic WebpackBuildMode _

instance Show WebpackBuildMode where
  show = genericShow

type Env = { webpackBuildMode :: WebpackBuildMode }

