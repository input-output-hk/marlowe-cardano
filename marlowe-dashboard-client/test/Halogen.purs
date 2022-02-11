module Test.Halogen where

import Prelude

import Control.Monad.Error.Class (class MonadError, throwError)
import Control.Monad.Reader (class MonadAsk, runReaderT)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff, Error, bracket, error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen (Component)
import Halogen.Aff (awaitBody)
import Halogen.VDom.Driver (HalogenIO, runUI)
import Test.Web (runTestM)
import Test.Web.Event.User (runUserM)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Web.DOM.Document as Document
import Web.DOM.Node as Node
import Web.HTML (window)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement as HTMLElement
import Web.HTML.Window as Window

runUITest
  :: forall query input output
   . Component query input output Aff
  -> input
  -> ( forall m
        . MonadUser m
       => MonadTest m
       => MonadAsk (HalogenIO query output Aff) m
       => MonadAff m
       => MonadError Error m
       => m Unit
     )
  -> Aff Unit
runUITest component input test =
  bracket getContainer removeContainer \(Tuple _ container) -> do
    driver <- runUI component input container
    runTestM (HTMLElement.toElement container)
      $ runUserM Nothing
      $ runReaderT test driver
  where
  getContainer = do
    document <-
      liftEffect $ map HTMLDocument.toDocument $ Window.document =<< window
    body <- awaitBody
    container <-
      maybe (throwError $ error "Failed to cast Element to HTMLElement") pure
        <<< HTMLElement.fromElement
        =<< liftEffect (Document.createElement "div" document)
    liftEffect $ Node.appendChild
      (HTMLElement.toNode container)
      (HTMLElement.toNode body)
    pure $ Tuple body container
  removeContainer (Tuple body container) = do
    liftEffect $ Node.removeChild
      (HTMLElement.toNode container)
      (HTMLElement.toNode body)
