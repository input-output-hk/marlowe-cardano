module Test.Web.Monad where

import Prelude

import Control.Monad.Cont (ContT, mapContT)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, mapExceptT)
import Control.Monad.Maybe.Trans (MaybeT, mapMaybeT)
import Control.Monad.RWS (RWST, mapRWST)
import Control.Monad.Reader (ReaderT, mapReaderT)
import Control.Monad.State (StateT, mapStateT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT, mapWriterT)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Effect.Aff (Aff, effectCanceler, makeAff, nonCanceler)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Web.DOM (Element)
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
import Web.HTML.Window as Window

class MonadAff m <= MonadTest m where
  -- config :: m Config TODO
  getContainer :: m Element
  withContainer :: forall a. Element -> m a -> m a

instance MonadTest m => MonadTest (ReaderT r m) where
  getContainer = lift getContainer
  withContainer container = mapReaderT $ withContainer container

instance (Monoid w, MonadTest m) => MonadTest (WriterT w m) where
  getContainer = lift getContainer
  withContainer container = mapWriterT $ withContainer container

instance MonadTest m => MonadTest (StateT s m) where
  getContainer = lift getContainer
  withContainer container = mapStateT $ withContainer container

instance MonadTest m => MonadTest (ContT r m) where
  getContainer = lift getContainer
  withContainer container = mapContT $ withContainer container

instance MonadTest m => MonadTest (ExceptT e m) where
  getContainer = lift getContainer
  withContainer container = mapExceptT $ withContainer container

instance MonadTest m => MonadTest (MaybeT m) where
  getContainer = lift getContainer
  withContainer container = mapMaybeT $ withContainer container

instance (Monoid w, MonadTest m) => MonadTest (RWST r w s m) where
  getContainer = lift getContainer
  withContainer container = mapRWST $ withContainer container

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
  bodyEl <- liftAff $ maybe (throwError (error "Could not find body")) pure body
  withContainer bodyEl ma
