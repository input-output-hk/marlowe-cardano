module Breadcrumb where

import Prologue

import Affjax.StatusCode (StatusCode)
import Data.Argonaut (class EncodeJson, Json, encodeJson)
import Data.Argonaut as A
import Data.Compactable (compact)
import Data.HTTP.Method (Method)
import Data.Newtype (unwrap)
import Effect (Effect)
import Foreign.Object as FO

data Level
  = Debug
  | Info
  | Log
  | Warning
  | Error
  | Fatal
  | Critical

instance EncodeJson Level where
  encodeJson = encodeJson <<< case _ of
    Debug -> "debug"
    Info -> "info"
    Log -> "log"
    Warning -> "warning"
    Error -> "error"
    Fatal -> "fatal"
    Critical -> "critical"

newtype Breadcrumb = Breadcrumb
  { category :: Maybe String
  , message :: Maybe String
  , level :: Maybe Level
  , data :: Maybe BreadcrumbData
  }

data BreadcrumbData
  = DefaultBreadcrumb (Maybe Json)
  | DebugBreadcrumb (Maybe Json)
  | ErrorBreadcrumb (Maybe Json)
  | NavigationBreadcrumb { from :: String, to :: String }
  | HttpBreadcrumb
      { url :: Maybe String
      , method :: Maybe Method
      , statusCode :: Maybe StatusCode
      , reason :: Maybe String
      }
  | InfoBreadcrumb (Maybe Json)
  | QueryBreadcrumb (Maybe Json)
  | UiBreadcrumb (Maybe Json)
  | UserBreadcrumb (Maybe Json)

instance EncodeJson Breadcrumb where
  encodeJson (Breadcrumb breadcrumb) = A.fromObject
    $ FO.fromFoldable
    $ compact
        [ Tuple "type" <<< encodeType <$> breadcrumb.data
        , Tuple "level" <<< encodeJson <$> breadcrumb.level
        , Tuple "category" <<< encodeJson <$> breadcrumb.category
        , Tuple "message" <<< encodeJson <$> breadcrumb.message
        , Tuple "data" <$> (encodeData =<< breadcrumb.data)
        ]
    where
    encodeType = encodeJson <<< case _ of
      DefaultBreadcrumb _ -> "default"
      DebugBreadcrumb _ -> "debug"
      ErrorBreadcrumb _ -> "error"
      NavigationBreadcrumb _ -> "navigation"
      HttpBreadcrumb _ -> "http"
      InfoBreadcrumb _ -> "info"
      QueryBreadcrumb _ -> "query"
      UiBreadcrumb _ -> "ui"
      UserBreadcrumb _ -> "user"
    encodeData = case _ of
      DefaultBreadcrumb payload -> payload
      DebugBreadcrumb payload -> payload
      ErrorBreadcrumb payload -> payload
      NavigationBreadcrumb payload -> Just $ encodeJson payload
      HttpBreadcrumb payload -> Just $ A.fromObject
        $ FO.fromFoldable
        $ compact
            [ Tuple "url" <<< encodeJson <$> payload.url
            , Tuple "method" <<< encodeJson <<< show <$> payload.method
            , Tuple "statusCode" <<< encodeJson <<< unwrap <$>
                payload.statusCode
            , Tuple "reason" <<< encodeJson <$> payload.reason
            ]
      InfoBreadcrumb payload -> payload
      QueryBreadcrumb payload -> payload
      UiBreadcrumb payload -> payload
      UserBreadcrumb payload -> payload

foreign import _addBreadcrumb :: Json -> Effect Unit

addBreadcrumb :: Breadcrumb -> Effect Unit
addBreadcrumb = _addBreadcrumb <<< encodeJson
