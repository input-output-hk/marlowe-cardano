module Contrib.HTML.Window.Child where

import Prelude

import Contrib.Foreign (foreignDecodeJson)
import Contrib.Web.Event.EventTarget (addEventListenerWithOptions)
import Control.Alt ((<|>))
import Data.Argonaut (Json)
import Data.Either (Either(..), note)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Web.Event.Event (EventType(..))
import Web.Event.EventTarget (eventListener, removeEventListener)
import Web.HTML (Window)
import Web.HTML.Window as Window
import Web.Socket.Event.MessageEvent as MessageEvent

type Opts = { url :: String, features :: String, window :: Window }

beforeunloadEventType :: EventType
beforeunloadEventType = EventType "beforeunload"

-- | Nearly the same as `Window.open` but we await here till the child
-- | window is closed.
-- | Same origin is required - use relative urls here.
-- | `Nothing` indicates `open` error.
openAwaitClosed :: Opts -> Aff (Maybe Unit)
openAwaitClosed { url, features, window } = do
  liftEffect (Window.open url "_blank" features window) >>= case _ of
    Just window' -> makeAff resolver
      where
      windowEventTarget = Window.toEventTarget window'
      resolver cb = do
        listener <- eventListener \_ -> cb $ Right (Just unit)
        let opts = { capture: false, passive: true, once: true }
        -- This listener cleanups itself when window is closed
        addEventListenerWithOptions beforeunloadEventType listener opts
          windowEventTarget
        -- We can return a nonCanceler because the waitForEvent is called with a finally
        pure nonCanceler
    Nothing -> pure Nothing

messageEventType :: EventType
messageEventType = EventType "message"

-- | Just to make signature a bit more readable ;-)
data OpenAwaitMsgErr
  = WindowOpenError
  | MissingMsg
  | MsgDecodingError

-- | Spawn child window and await till its sends message or is closed.
-- |
-- | Same origin is required - use relative urls here.
-- |
-- | FIXME: This implementation not complately safe API because we just await the message
-- | without actually checking the `source` of the `message` event.
-- | I'm not sure how to handle WindoProxy correcty - how to compare it to the window
-- |
openAwaitMsg :: Opts -> Aff (Either OpenAwaitMsgErr Json)
openAwaitMsg opts@{ window } = do
  msgListenerRef <- liftEffect $ Ref.new Nothing
  let
    windowEventTarget = Window.toEventTarget window
    awaitMsg = makeAff resolver
      where
      resolver cb = do
        listener <-
          eventListener \event -> do
            let
              res = note MsgDecodingError do
                foreignValue <- MessageEvent.data_ <$> MessageEvent.fromEvent
                  event
                foreignDecodeJson foreignValue
            cb $ Right res

        let listenerOpts = { capture: false, passive: true, once: true }
        addEventListenerWithOptions messageEventType listener listenerOpts
          windowEventTarget
        Ref.write (Just listener) msgListenerRef
        -- We can return a nonCanceler because the waitForEvent is called with a finally
        pure nonCanceler

    openAwaitClosed' = openAwaitClosed opts >>= case _ of
      Nothing -> pure $ Left WindowOpenError
      Just _ -> do
        -- If window was closed without passing any message
        -- we have to cleanup the msg listener.
        liftEffect $ Ref.read msgListenerRef >>= traverse_ \listener ->
          removeEventListener messageEventType listener false windowEventTarget
        pure $ Left MissingMsg

  openAwaitClosed' <|> awaitMsg
