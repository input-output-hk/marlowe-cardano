module Forms.Types where

import Data.FormURLEncoded.Query (Query) as FormURLEncoded
import Data.Map.Ordered.OMap (OMap)
import Polyform.Batteries.UrlEncoded (Errors) as UrlEncoded

type RenderWidgetFn :: Type -> Type -> Type -> Type
type RenderWidgetFn state err widget =
  state -> FormURLEncoded.Query -> UrlEncoded.Errors err -> widget

type Widgets widget = OMap String widget

type RenderFn state err widget =
  state -> FormURLEncoded.Query -> UrlEncoded.Errors err -> Widgets widget
