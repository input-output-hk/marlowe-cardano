module Contrib.Web.Event.EventTarget where

import Prelude

import Effect (Effect)
import Web.Event.Event (EventType)
import Web.Event.EventTarget (EventListener, EventTarget)

-- | Newer version of purescript-web-events exposes this function
-- | this is just a copy of it
foreign import addEventListenerWithOptions
  :: EventType
  -> EventListener
  -> { capture :: Boolean
     , once :: Boolean
     , passive :: Boolean
     }
  -> EventTarget
  -> Effect Unit
