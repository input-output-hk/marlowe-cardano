module Component.MetadataTab
  ( component
  , module Exports
  , render
  ) where

import Prologue
import Component.MetadataTab.Types (MetadataAction(..)) as Exports
import Component.MetadataTab.Types (MetadataAction)
import Component.MetadataTab.View (metadataView)
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

component :: forall m q. H.Component q Input MetadataAction m
component =
  Hooks.component \{ outputToken } { metadata, metadataHintInfo } -> Hooks.do
    let
      update a = raise outputToken a
    Hooks.pure do
      map update $ metadataView metadataHintInfo metadata

render
  :: forall a m
   . Input
  -> (MetadataAction -> a)
  -> HTML (ComponentSlot ChildSlots m a) a
render = HH.slot (Proxy :: Proxy "metadata") unit component
