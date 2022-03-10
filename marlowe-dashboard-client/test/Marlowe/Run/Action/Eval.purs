module Test.Marlowe.Run.Action.Eval where

import Prologue

import Affjax as Affjax
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Concurrent.Queue as Queue
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  , try
  )
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Align (crosswalk)
import Data.Argonaut
  ( decodeJson
  , encodeJson
  , printJsonDecodeError
  , stringifyWithIndent
  )
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Array (snoc)
import Data.Bifunctor (lmap)
import Data.Bimap as Bimap
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (foldM, for_, traverse_)
import Data.Newtype (over2)
import Data.String (joinWith, null)
import Data.String.Extra (repeat)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (uncurry)
import Effect.Aff (Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Assertions (shouldEqualJson)
import Test.Control.Monad.Time (class MonadMockTime, advanceTime)
import Test.Marlowe.Run (Coenv, marloweRunTest)
import Test.Marlowe.Run.Action.Types
  ( HttpExpect(..)
  , HttpExpectContent(..)
  , HttpRespond(..)
  , HttpRespondContent(..)
  , MarloweRunAction(..)
  , MarloweRunScript
  , ScriptError(..)
  , WalletName
  , throwScriptError
  )
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , expectJsonContent
  , expectMethod
  , expectRequest
  , expectTextContent
  , expectUri
  , renderMatcherError
  )
import Test.Spec (Spec)
import Test.Spec.Assertions (fail)
import Test.Web.DOM.Assertions (shouldCast, shouldNotBeDisabled)
import Test.Web.DOM.Node (toNode)
import Test.Web.DOM.Query (findBy, getBy, nameRegex, queryBy, role, text)
import Test.Web.Event.User (click, clickM, type_)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))
import Web.DOM.Node (textContent)
import WebSocket.Support (FromSocket(..))

runScriptedTest :: String -> Spec Unit
runScriptedTest scriptName = marloweRunTest scriptName do
  scriptText <-
    liftEffect $ FS.readTextFile UTF8 $ "test/scripts/" <> scriptName <> ".json"
  script <- parseDecodeJson scriptText # case _ of
    Left e -> throwError
      $ error
      $ "Failed to parse script file:\n\n" <> printJsonDecodeError e
    Right s -> pure s
  pabWebsocketOut <- asks _.pabWebsocketOut
  httpRequests <- asks _.httpRequests
  either throwScriptError pure =<< evalScript (lmap Milliseconds <$> script)
  -- Ensure there are no unhandled HTTP requests
  httpMsg <- liftAff do
    lines <- flip tailRecM [] \errors -> do
      mRequest <- Queue.tryRead httpRequests
      pure case mRequest of
        Nothing -> Done errors
        Just request ->
          Loop
            $ snoc errors
            $ renderMatcherError request
            $ MatcherError [ "✗ An HTTP request was not expected" ]
    pure $ joinWith ("\n  " <> repeat 80 "=" <> "\n  ") lines
  -- Ensure there are no unhandled WebSocket messages
  wsMsg <- liftAff do
    lines <- flip tailRecM [] \errors -> do
      mRequest <- Queue.tryRead pabWebsocketOut
      pure case mRequest of
        Nothing -> Done errors
        Just msg ->
          Loop $ snoc errors $ stringifyWithIndent 2 $ encodeJson msg
    pure $ joinWith ("\n  " <> repeat 80 "=" <> "\n  ") lines

  let
    httpError =
      if null httpMsg then Nothing
      else Just $ joinWith "\n  "
        [ "Test finished with unhandled HTTP requests:"
        , repeat 80 "="
        , httpMsg
        ]
    wsError =
      if null wsMsg then Nothing
      else Just $ joinWith "\n  "
        [ "Test finished with unhandled WebSocket messages:"
        , repeat 80 "="
        , wsMsg
        ]
    combinedError =
      joinWith "\n  " <$> crosswalk identity [ httpError, wsError ]
  traverse_ (throwError <<< error) combinedError

evalScript
  :: forall m
   . MonadAff m
  => MonadTest m
  => MonadUser m
  => MonadError Error m
  => MonadMockHTTP m
  => MonadMockTime m
  => MonadAsk Coenv m
  => MarloweRunScript
  -> m (Either ScriptError Unit)
evalScript = runExceptT <<< void <<< foldM (map uncurry evalAction') []
  where
  evalAction'
    :: Array MarloweRunAction
    -> Milliseconds
    -> MarloweRunAction
    -> ExceptT ScriptError m (Array MarloweRunAction)
  evalAction' succeeded millis action =
    withExceptT (ScriptError succeeded action) $ ExceptT $ try do
      currentTime <- liftEffect <<< Ref.read =<< asks _.currentTime
      let currentMillis = unInstant currentTime
      let millisUntilAction = over2 Milliseconds (-) millis currentMillis
      advanceTime (millisUntilAction :: Milliseconds)
      evalAction action
      pure $ snoc succeeded action

evalAction
  :: forall m
   . MonadAff m
  => MonadTest m
  => MonadUser m
  => MonadThrow Error m
  => MonadMockHTTP m
  => MonadAsk Coenv m
  => MarloweRunAction
  -> m Unit
evalAction = case _ of
  -- Evaluate a CreateWallet action by navigating to the welcome page and
  -- performing the create wallet workflow.
  CreateWallet { walletName } -> do
    getToWelcome
    createWallet walletName

  -- Evaluate a ConfirmMnemonic action by copying the mnemonic presented to the
  -- user and re-entering it. Also save the mnemonic to the test environment.
  ConfirmMnemonic { walletName } -> do
    confirmMnemonic walletName

  -- Evaluate a UseWallet action by navigating to the welcome page and
  -- performing the restore wallet workflow.
  UseWallet { walletName } -> do
    getToWelcome
    restore walletName

  -- Evaluate a PabWebSocketSend action by reading from the output queue and
  -- checking the content against the expectations.
  PabWebSocketSend { expectPayload } -> do
    queue <- asks _.pabWebsocketOut
    msg <- liftAff $ Queue.read queue
    encodeJson msg `shouldEqualJson` expectPayload

  -- Evaluate a PabWebSocketReceive action by sending the payload to the
  -- listener.
  PabWebSocketReceive { payload } -> do
    listener <- asks _.pabWebsocketIn
    liftEffect $ HS.notify listener $ ReceiveMessage $ decodeJson payload

  HttpRequest { expect: HttpExpect expect, respond: HttpRespond respond } ->
    let
      { status, statusText, headers: headerTuples, content } = respond
      headers = uncurry ResponseHeader <$> headerTuples

      expectRequestWithBody
        :: forall a
         . ResponseFormat a
        -> Either Affjax.Error a
        -> m Unit
      expectRequestWithBody format body = expectRequest format
        ado
          traverse_ expectMethod expect.method
          traverse_ expectUri expect.uri
          for_ expect.content case _ of
            ExpectJson json -> expectJsonContent json
            ExpectText text -> expectTextContent text
          in { status, statusText, headers, body: _ } <$> body
    in
      case content of
        RespondText text ->
          expectRequestWithBody ResponseFormat.string $ Right text
        RespondJson json ->
          expectRequestWithBody ResponseFormat.json $ Right json

getToWelcome :: forall m. MonadTest m => MonadUser m => m Unit
getToWelcome = do
  dashboardLink <- queryBy role do
    nameRegex "dashboard" ignoreCase
    pure Link
  case dashboardLink of
    -- We're already on the welcome page.
    Nothing -> pure unit
    -- We need to drop the current wallet.
    Just _ -> do
      openMyWallet dropWallet
      void $ findBy text $ pure "To begin using the Marlowe Run Demo"

openMyWallet :: forall m. MonadTest m => MonadUser m => m Unit -> m Unit
openMyWallet action = do
  clickM $ getBy role do
    nameRegex "my wallet" ignoreCase
    pure Link
  card <- getBy role $ pure Dialog
  withContainer card action

dropWallet :: forall m. MonadTest m => MonadUser m => m Unit
dropWallet = do
  clickM $ getBy role do
    nameRegex "drop" ignoreCase
    pure Button

createWallet
  :: forall m
   . MonadTest m
  => MonadUser m
  => WalletName
  -> m Unit
createWallet walletName = do
  clickM $ getBy role do
    nameRegex "generate" ignoreCase
    pure Button
  dialog <- getBy role $ pure Dialog
  withContainer dialog do
    nicknameField <- getBy role do
      nameRegex "wallet nickname" ignoreCase
      pure Textbox
    click nicknameField
    type_ nicknameField walletName Nothing
    clickM $ getBy role do
      nameRegex "create wallet" ignoreCase
      pure Button

confirmMnemonic
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadAsk Coenv m
  => MonadThrow Error m
  => WalletName
  -> m Unit
confirmMnemonic walletName = do
  dialog <- getBy role $ pure Dialog
  withContainer dialog do
    mnemonicText <- findBy role $ pure Mark
    mnemonic <- liftEffect (textContent $ toNode mnemonicText)
    clickM $ getBy role do
      nameRegex "ok" ignoreCase
      pure Button
    mnemonicField <- getBy role do
      nameRegex "mnemonic phrase" ignoreCase
      pure Textbox
    type_ mnemonicField mnemonic Nothing
    okButton <- shouldCast =<< getBy role do
      nameRegex "ok" ignoreCase
      pure Button
    shouldNotBeDisabled okButton
    click okButton
    wallets <- asks _.wallets
    liftEffect $ Ref.modify_ (Bimap.insert walletName mnemonic) wallets

restore
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadAsk Coenv m
  => MonadThrow Error m
  => WalletName
  -> m Unit
restore walletName = do
  wallets <- asks _.wallets
  mMnemonic <- liftEffect $ Bimap.lookupL walletName <$> Ref.read wallets
  case mMnemonic of
    Nothing -> fail $ "Test error: unknown wallet " <> walletName
    Just mnemonic -> do
      nicknameField <- getBy role do
        nameRegex "wallet nickname" ignoreCase
        pure Textbox
      mnemonicField <- getBy role do
        nameRegex "mnemonic phrase" ignoreCase
        pure Textbox
      type_ nicknameField walletName Nothing
      type_ mnemonicField mnemonic Nothing
      clickM $ getBy role do
        nameRegex "restore wallet" ignoreCase
        pure Button
