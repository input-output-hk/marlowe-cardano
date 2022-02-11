module Test.Web.DOM.Assertions where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.String (toLower)
import Effect.Aff (Error)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Assertions (shouldEqual)
import Test.Web.DOM.Element (class IsElement, toElement)
import Test.Web.DOM.Node (class IsNode, toNode)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent)

shouldHaveText
  :: forall node m
   . IsNode node
  => MonadError Error m
  => MonadEffect m
  => node
  -> String
  -> m Unit
shouldHaveText node text =
  flip shouldEqual text =<< liftEffect (textContent $ toNode node)

shouldHaveId
  :: forall element m
   . MonadError Error m
  => IsElement element
  => MonadEffect m
  => element
  -> String
  -> m Unit
shouldHaveId element expected =
  flip shouldEqual expected =<< liftEffect (Element.id $ toElement element)

shouldHaveTagName
  :: forall element m
   . MonadError Error m
  => IsElement element
  => element
  -> String
  -> m Unit
shouldHaveTagName element expected =
  toLower expected `shouldEqual` toLower (Element.tagName $ toElement element)
