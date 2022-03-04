module Test.Web.DOM.Assertions where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Effect.Aff (Error, error, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Test.Spec.Assertions (fail, shouldEqual)
import Test.Web.DOM.DomType (class DOMType, typeName)
import Test.Web.DOM.Element (class IsElement, toElement)
import Test.Web.DOM.Node (class IsNode, fromNode, toNode)
import Type.Proxy (Proxy(..))
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element as Element
import Web.DOM.Node (textContent)
import Web.HTML (HTMLButtonElement)
import Web.HTML.HTMLButtonElement as ButtonElement

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
  toLower (Element.tagName $ toElement element) `shouldEqual` toLower expected

shouldHaveClass
  :: forall element m
   . MonadError Error m
  => MonadEffect m
  => IsElement element
  => element
  -> String
  -> m Unit
shouldHaveClass element className = liftEffect do
  classNames <- Element.classList $ toElement element
  unlessM (DOMTokenList.contains classNames className) do
    fail $ "class not found: " <> className

shouldNotHaveClass
  :: forall element m
   . MonadError Error m
  => MonadEffect m
  => IsElement element
  => element
  -> String
  -> m Unit
shouldNotHaveClass element className = liftEffect do
  classNames <- Element.classList $ toElement element
  whenM (DOMTokenList.contains classNames className) do
    fail $ "class not found: " <> className

shouldNotBeDisabled
  :: forall m
   . MonadError Error m
  => MonadEffect m
  => HTMLButtonElement
  -> m Unit
shouldNotBeDisabled element = do
  isDisabled <- liftEffect $ ButtonElement.disabled element
  isDisabled `shouldEqual` false

shouldBeDisabled
  :: forall m
   . MonadError Error m
  => MonadEffect m
  => HTMLButtonElement
  -> m Unit
shouldBeDisabled element = do
  isDisabled <- liftEffect $ ButtonElement.disabled element
  isDisabled `shouldEqual` true

shouldCast
  :: forall el1 el2 m
   . IsNode el1
  => IsNode el2
  => DOMType el1
  => DOMType el2
  => MonadError Error m
  => MonadEffect m
  => el1
  -> m el2
shouldCast el1 = case fromNode $ toNode el1 of
  Nothing -> throwError $ error $ "Cannot cast from "
    <> typeName (Proxy :: _ el1)
    <> " to "
    <> typeName (Proxy :: _ el2)
  Just el2 -> pure el2
