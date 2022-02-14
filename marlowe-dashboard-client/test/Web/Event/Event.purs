module Test.Web.Event.Event where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Error, error)
import Test.Web.DOM.DomType (class DOMType, typeName)
import Type.Proxy (Proxy(..))
import Web.Clipboard.ClipboardEvent (ClipboardEvent)
import Web.Clipboard.ClipboardEvent as ClipboardEvent
import Web.Event.CustomEvent (CustomEvent)
import Web.Event.CustomEvent as CustomEvent
import Web.Event.Event (Event)
import Web.HTML.Event.BeforeUnloadEvent (BeforeUnloadEvent)
import Web.HTML.Event.BeforeUnloadEvent as BeforeUnloadEvent
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent as DragEvent
import Web.HTML.Event.ErrorEvent (ErrorEvent)
import Web.HTML.Event.ErrorEvent as ErrorEvent
import Web.HTML.Event.HashChangeEvent (HashChangeEvent)
import Web.HTML.Event.HashChangeEvent as HashChangeEvent
import Web.HTML.Event.PageTransitionEvent (PageTransitionEvent)
import Web.HTML.Event.PageTransitionEvent as PageTransitionEvent
import Web.HTML.Event.PopStateEvent (PopStateEvent)
import Web.HTML.Event.PopStateEvent as PopStateEvent
import Web.HTML.Event.TrackEvent (TrackEvent)
import Web.HTML.Event.TrackEvent as TrackEvent
import Web.Socket.Event.CloseEvent (CloseEvent)
import Web.Socket.Event.CloseEvent as CloseEvent
import Web.Socket.Event.MessageEvent (MessageEvent)
import Web.Socket.Event.MessageEvent as MessageEvent
import Web.Storage.Event.StorageEvent (StorageEvent)
import Web.Storage.Event.StorageEvent as StorageEvent
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.UIEvent.CompositionEvent (CompositionEvent)
import Web.UIEvent.CompositionEvent as CompositionEvent
import Web.UIEvent.FocusEvent (FocusEvent)
import Web.UIEvent.FocusEvent as FocusEvent
import Web.UIEvent.InputEvent (InputEvent)
import Web.UIEvent.InputEvent as InputEvent
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.MouseEvent as MouseEvent
import Web.UIEvent.UIEvent (UIEvent)
import Web.UIEvent.UIEvent as UIEvent
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WheelEvent
import Web.XHR.ProgressEvent (ProgressEvent)
import Web.XHR.ProgressEvent as ProgressEvent

expectFromEvent
  :: forall a m
   . MonadError Error m
  => DOMType a
  => IsEvent a
  => Event
  -> m a
expectFromEvent =
  maybe
    ( throwError
        $ error
        $ "Unable to downcast Node to " <> typeName (Proxy :: _ a)
    )
    pure <<< fromEvent

class IsEvent a where
  toEvent :: a -> Event
  fromEvent :: Event -> Maybe a

instance IsEvent Event where
  toEvent = identity
  fromEvent = Just

instance IsEvent ClipboardEvent where
  toEvent = ClipboardEvent.toEvent
  fromEvent = ClipboardEvent.fromEvent

instance IsEvent CustomEvent where
  toEvent = CustomEvent.toEvent
  fromEvent = CustomEvent.fromEvent

instance IsEvent BeforeUnloadEvent where
  toEvent = BeforeUnloadEvent.toEvent
  fromEvent = BeforeUnloadEvent.fromEvent

instance IsEvent DragEvent where
  toEvent = DragEvent.toEvent
  fromEvent = DragEvent.fromEvent

instance IsEvent ErrorEvent where
  toEvent = ErrorEvent.toEvent
  fromEvent = ErrorEvent.fromEvent

instance IsEvent HashChangeEvent where
  toEvent = HashChangeEvent.toEvent
  fromEvent = HashChangeEvent.fromEvent

instance IsEvent PageTransitionEvent where
  toEvent = PageTransitionEvent.toEvent
  fromEvent = PageTransitionEvent.fromEvent

instance IsEvent PopStateEvent where
  toEvent = PopStateEvent.toEvent
  fromEvent = PopStateEvent.fromEvent

instance IsEvent TrackEvent where
  toEvent = TrackEvent.toEvent
  fromEvent = TrackEvent.fromEvent

instance IsEvent CloseEvent where
  toEvent = CloseEvent.toEvent
  fromEvent = CloseEvent.fromEvent

instance IsEvent MessageEvent where
  toEvent = MessageEvent.toEvent
  fromEvent = MessageEvent.fromEvent

instance IsEvent StorageEvent where
  toEvent = StorageEvent.toEvent
  fromEvent = StorageEvent.fromEvent

instance IsEvent TouchEvent where
  toEvent = TouchEvent.toEvent
  fromEvent = TouchEvent.fromEvent

instance IsEvent CompositionEvent where
  toEvent = CompositionEvent.toEvent
  fromEvent = CompositionEvent.fromEvent

instance IsEvent FocusEvent where
  toEvent = FocusEvent.toEvent
  fromEvent = FocusEvent.fromEvent

instance IsEvent InputEvent where
  toEvent = InputEvent.toEvent
  fromEvent = InputEvent.fromEvent

instance IsEvent KeyboardEvent where
  toEvent = KeyboardEvent.toEvent
  fromEvent = KeyboardEvent.fromEvent

instance IsEvent MouseEvent where
  toEvent = MouseEvent.toEvent
  fromEvent = MouseEvent.fromEvent

instance IsEvent UIEvent where
  toEvent = UIEvent.toEvent
  fromEvent = UIEvent.fromEvent

instance IsEvent WheelEvent where
  toEvent = WheelEvent.toEvent
  fromEvent = WheelEvent.fromEvent

instance IsEvent ProgressEvent where
  toEvent = ProgressEvent.toEvent
  fromEvent = ProgressEvent.fromEvent
