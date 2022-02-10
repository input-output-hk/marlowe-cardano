module Test.Main where

import Prologue

import Data.String (toLower)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Test.Web (runTestMInBody)
import Test.Web.DOM.Query (findBy, getBy, role)
import Test.Web.Event.User (click, runUserM)
import Test.Web.Monad (getContainer)
import Web.ARIA (ARIARole(..))
import Web.DOM (Element)
import Web.DOM.Element as Element
import Web.DOM.Node (textContent)

foreign import setupTestApp :: Element -> Effect Unit

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "DOM tests" do
    it "works with JSDOM" do
      runUserM Nothing $ runTestMInBody do
        body <- getContainer
        toLower (Element.tagName body) `shouldEqual` "body"
        liftEffect $ setupTestApp body
        paragraph <- getBy $ role Paragraph
        pid <- liftEffect $ Element.id paragraph
        pid `shouldEqual` "para"
        text <- liftEffect $ textContent $ Element.toNode paragraph
        text `shouldEqual` "Test content"
        click =<< findBy (role Button)
        text' <- liftEffect $ textContent $ Element.toNode paragraph
        text' `shouldEqual` "It worked!"
