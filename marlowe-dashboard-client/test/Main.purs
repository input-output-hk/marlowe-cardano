module Test.Main where

import Prologue

import Control.Monad.State (class MonadState)
import Data.Int (decimal)
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Halogen (Component)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties.ARIA as HP
import Test.Data.Address.Bech32 as Bech32Address
import Test.Data.Address.Bech32.DataPart as Bech32DataPart
import Test.Data.Address.Bech32.DataPart.CodePoint as DataPartCodePoint
import Test.Data.Address.Bech32.HRP as Bech32HRP
import Test.Data.Address.Bech32.HRP.CodePoint as HRPCodePoint
import Test.Halogen (expectMessages, runUITest)
import Test.Halogen as TH
import Test.Marlowe.Run.Action.Eval (runScriptedTest)
import Test.Spec (Spec, describe, it, parallel)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (defaultConfig, runSpec')
import Test.Web (runTestMInBody)
import Test.Web.DOM.Assertions (shouldHaveId, shouldHaveTagName, shouldHaveText)
import Test.Web.DOM.Query (findBy, getBy, name, role)
import Test.Web.Event.User (click, clickM, runUserM)
import Test.Web.Monad (getContainer)
import Web.ARIA (ARIARole(..))
import Web.DOM (Element)

main :: Effect Unit
main = launchAff_ $ runSpec'
  defaultConfig { timeout = Just $ Milliseconds 5000.0 }
  [ consoleReporter ]
  do
    parallel do
      Bech32Address.spec
      Bech32DataPart.spec
      Bech32HRP.spec
      DataPartCodePoint.spec
      HRPCodePoint.spec
    testingLibrarySpec
    halogenTestingLibrarySpec
    testScripts

testScripts :: Spec Unit
testScripts = describe "Scripted scenarios" do
  runScriptedTest "create-and-restore-wallet"

-- runScriptedTest "create-contract"

-------------------------------------------------------------------------------
-- Demo tests for purescript-testing-library
-------------------------------------------------------------------------------

foreign import setupTestApp :: Element -> Effect Unit

testingLibrarySpec :: Spec Unit
testingLibrarySpec = do
  describe "testing-library" do
    it "works with JSDOM" do
      runUserM Nothing $ runTestMInBody do
        body <- getContainer
        body `shouldHaveTagName` "body"
        liftEffect $ setupTestApp body
        paragraph <- getBy role $ pure Paragraph
        paragraph `shouldHaveId` "para"
        paragraph `shouldHaveTagName` "p"
        paragraph `shouldHaveText` "Test content"
        clickM $ findBy role $ pure Button
        paragraph `shouldHaveText` "It worked!"

-------------------------------------------------------------------------------
-- Demo tests for halogen-testing-library
-------------------------------------------------------------------------------

-- Simple Counter component

data Query a
  = GetValue (Int -> a)
  | SetValue Int a

data Action
  = Receive Int
  | Increment
  | Decrement

counter :: Component Query Int Int Aff
counter = H.mkComponent
  { initialState: identity
  , render
  , eval: H.mkEval H.defaultEval
      { receive = Just <<< Receive
      , handleAction = handleAction
      , handleQuery = handleQuery
      }
  }
  where
  handleAction = case _ of
    Receive v -> H.put v
    Increment -> H.raise =<< H.modify (_ + 1)
    Decrement -> H.raise =<< H.modify (_ - 1)
  render state =
    HH.div_
      [ HH.button [ HE.onClick $ const Decrement ] [ HH.text "-" ]
      , HH.span [ HP.role "textbox" ]
          [ HH.text $ Int.toStringAs decimal state ]
      , HH.button [ HE.onClick $ const Increment ] [ HH.text "+" ]
      ]

handleQuery
  :: forall m a. Functor m => MonadState Int m => Query a -> m (Maybe a)
handleQuery = case _ of
  GetValue k -> map (Just <<< k) H.get
  SetValue n a -> do
    H.put n
    pure $ Just a

-- Component Spec

halogenTestingLibrarySpec :: Spec Unit
halogenTestingLibrarySpec = do
  describe "halogen-testing-library" do

    it "Receives the initial input" do
      runUITest counter 10 do
        span <- getBy role $ pure Textbox
        span `shouldHaveText` "10"

    it "Receives new input" do
      runUITest counter 0 do
        TH.sendInput 20
        span <- getBy role $ pure Textbox
        span `shouldHaveText` "20"

    it "Handles user interaction" do
      runUITest counter 0 do
        decrement <- getBy role do
          name "-"
          pure Button
        increment <- getBy role do
          name "+"
          pure Button
        span <- getBy role $ pure Textbox
        click increment
        span `shouldHaveText` "1"
        click decrement
        click decrement
        span `shouldHaveText` "-1"

    it "Sends messages" do
      runUITest counter 0 do
        decrement <- getBy role do
          name "-"
          pure Button
        increment <- getBy role do
          name "+"
          pure Button
        click increment
        click decrement
        expectMessages [ 1, 0 ]
        click decrement
        expectMessages [ -1 ]

    it "Handles queries" do
      runUITest counter 0 do
        increment <- getBy role do
          name "+"
          pure Button
        span <- getBy role $ pure Textbox
        click increment
        click increment
        value <- TH.request GetValue
        value `shouldEqual` Just 2
        TH.tell $ SetValue 10
        span `shouldHaveText` "10"

--     it "Can be debugged" do
--       runUITest counter 0 do
--         increment <- getBy $ role' Button byRoleDefault
--           { name = toUndefinable $ Just "+"
--           }
--         span <- getBy (role Textbox)
--         click increment
--         click increment
--         debugElement increment
--         debugElements [ increment, span ]
--         logTestingPlaygroundURL
