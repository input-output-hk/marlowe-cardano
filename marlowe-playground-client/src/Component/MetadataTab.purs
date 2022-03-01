module Component.MetadataTab
  ( component
  , module Exports
  , render
  ) where

import Prologue

import Component.MetadataTab.Types (MetadataAction(..))
import Component.MetadataTab.Types (MetadataAction(..)) as Exports
import Component.MetadataTab.View (metadataView)
import Contrib.Halogen.Components.Sortable.Hook (useApplySortable)
import Effect.Class (class MonadEffect)
import Halogen (Component) as H
import Halogen (ComponentSlot)
import Halogen.HTML (HTML)
import Halogen.HTML (slot) as HH
import Halogen.Hooks (raise)
import Halogen.Hooks as Hooks
import MainFrame.Types (ChildSlots)
import Marlowe.Extended.Metadata (MetaData, MetadataHintInfo)
import Type.Prelude (Proxy(..))

type State
  = Unit

type Input
  =
  { metadataHintInfo :: MetadataHintInfo
  , metadata :: MetaData
  }

component :: forall m q. MonadEffect m => H.Component q Input MetadataAction m
component =
  Hooks.component \{ outputToken } { metadata, metadataHintInfo } -> Hooks.do
    let
      raise' = raise outputToken

    -- | These hooks modify internal state through handlers
    -- | so we have to turn all our handlers are `HookM m Unit`.
    timeParameterDescriptions <- useApplySortable
      (raise' <<< MoveTimeParameterDescription)
    valueParameterInfos <- useApplySortable
      (raise' <<< MoveValueParameterDescription)

    let
      dragging = { timeParameterDescriptions, valueParameterInfos }
      handlers =
        { raise: raise'
        , dragging
        }
    Hooks.pure do
      metadataView handlers metadataHintInfo metadata

render
  :: forall a m
   . MonadEffect m
  => Input
  -> (MetadataAction -> a)
  -> HTML (ComponentSlot ChildSlots m a) a
render = HH.slot (Proxy :: Proxy "metadata") unit component
