module Test.Web.Monad where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Test.Web.Config (Config)
import Web.DOM.ParentNode (QuerySelector(..), querySelector)
import Web.Event.EventTarget
  ( addEventListener
  , eventListener
  , removeEventListener
  )
import Web.HTML (window)
import Web.HTML.Event.EventTypes as ET
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

class MonadAff m <= MonadTest m where
  config :: m Config
  getContainer :: m HTMLElement
  withContainer :: forall a. HTMLElement -> m a -> m a

-- | Waits for the document to load.
-- | Copied from Halogen.Aff.Util
awaitLoad :: Aff HTMLDocument.HTMLDocument
awaitLoad = makeAff \callback -> do
  rs <- readyState =<< Window.document =<< window
  case rs of
    Loading -> do
      et <- Window.toEventTarget <$> window
      listener <- eventListener
        ( \_ -> do
            document <- Window.document =<< window
            callback (Right document)
        )
      addEventListener ET.domcontentloaded listener false et
      pure $ effectCanceler
        (removeEventListener ET.domcontentloaded listener false et)
    _ -> do
      document <- Window.document =<< window
      callback $ Right document
      pure nonCanceler

withBody :: forall a m. MonadTest m => m a -> m a
withBody ma = do
  document <- liftAff awaitLoad
  body <- liftEffect
    $ querySelector (QuerySelector "body")
    $ HTMLDocument.toParentNode document
  bodyEl <- liftAff $ maybe
    (throwError (error "Could not find body"))
    pure
    (HTMLElement.fromElement =<< body)
  withContainer bodyEl ma
