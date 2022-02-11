module Test.Web.DOM.Assertions where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.String (toLower)
import Effect.Aff (Error)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Assertions (shouldEqual)
import Web.DOM (Element, Node)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent)

shouldHaveText
  :: forall m. MonadError Error m => MonadEffect m => Node -> String -> m Unit
shouldHaveText node text =
  flip shouldEqual text =<< liftEffect (textContent node)

shouldHaveId
  :: forall m
   . MonadError Error m
  => MonadEffect m
  => Element
  -> String
  -> m Unit
shouldHaveId element expected =
  flip shouldEqual expected =<< liftEffect (Element.id element)

shouldHaveTagName
  :: forall m. MonadError Error m => Element -> String -> m Unit
shouldHaveTagName element expected =
  toLower expected `shouldEqual` toLower (Element.tagName element)
