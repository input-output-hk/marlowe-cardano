module Forms.Types where

import Prelude

import Data.FormURLEncoded.Query (Query) as FormURLEncoded
import Data.Generic.Rep (class Generic)
import Data.Map.Ordered.OMap (OMap)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Polyform.Batteries.UrlEncoded (Errors) as UrlEncoded

type RenderWidgetFn :: Type -> Type -> Type -> Type
type RenderWidgetFn state err widget =
  state -> FormURLEncoded.Query -> UrlEncoded.Errors err -> widget

newtype WidgetId = WidgetId String

derive instance Newtype WidgetId _
derive instance Generic WidgetId _
derive newtype instance Eq WidgetId
derive newtype instance Ord WidgetId
instance Show WidgetId where
  show = genericShow

type Widgets widget = OMap WidgetId widget

type RenderFn state err widget =
  state -> FormURLEncoded.Query -> UrlEncoded.Errors err -> Widgets widget
