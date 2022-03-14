module Test.Marlowe.Run.Action.Eval where

import Prologue

import Affjax as Affjax
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.ResponseFormat as ResponseFormat
import Affjax.ResponseHeader (ResponseHeader(..))
import Concurrent.Queue as Queue
import Control.Monad.Error.Class (class MonadError, throwError, try)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Align (crosswalk)
import Data.Argonaut
  ( class EncodeJson
  , Json
  , decodeJson
  , encodeJson
  , jsonNull
  , printJsonDecodeError
  , stringifyWithIndent
  )
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Array (snoc)
import Data.Bifunctor (lmap)
import Data.Bimap as Bimap
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (foldM, for_, traverse_)
import Data.HTTP.Method (Method(..))
import Data.Newtype (over2)
import Data.String
  ( Pattern(..)
  , Replacement(..)
  , joinWith
  , null
  , replaceAll
  , split
  )
import Data.String.Extra (repeat)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (uncurry)
import Effect.Aff (Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription as HS
import MarloweContract (MarloweContract(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Assertions (shouldEqualJson)
import Test.Control.Monad.Time (class MonadMockTime, advanceTime)
import Test.Marlowe.Run (Coenv, marloweRunTest)
import Test.Marlowe.Run.Action.Types
  ( CreateWalletRecord
  , HttpExpect(..)
  , HttpExpectContent(..)
  , HttpRespond(..)
  , HttpRespondContent(..)
  , MarloweRunAction(..)
  , MarloweRunScript
  , PlutusAppId
  , ScriptError(..)
  , WalletId
  , WalletName
  , renderScriptError
  )
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , expectJsonContent
  , expectJsonRequest
  , expectMethod
  , expectRequest
  , expectTextContent
  , expectUri
  , renderMatcherError
  )
import Test.Spec (Spec)
import Test.Spec.Assertions (fail)
import Test.Web.DOM.Assertions (shouldCast, shouldHaveText, shouldNotBeDisabled)
import Test.Web.DOM.Query (findBy, getBy, nameRegex, role)
import Test.Web.Event.User (click, clickM, type_)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))
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
  runError <- either (Just <<< renderScriptError) (const Nothing)
    <$> evalScript (lmap Milliseconds <$> script)
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
      joinWith "\n\n  " <$> crosswalk identity [ runError, httpError, wsError ]
  traverse_
    (throwError <<< error <<< replaceAll (Pattern "\n") (Replacement "\n  "))
    combinedError

evalScript
  :: forall m
   . MonadAff m
  => MonadTest m
  => MonadUser m
  => MonadError Error m
  => MonadMockHTTP m
  => MonadMockTime m
  => MonadTime m
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
      currentTime <- now
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
  => MonadError Error m
  => MonadMockHTTP m
  => MonadAsk Coenv m
  => MarloweRunAction
  -> m Unit
evalAction = case _ of
  -- Evaluate a CreateWallet action by performing the create wallet workflow.
  DropWallet -> do
    dropWallet

  -- Evaluate a CreateWallet action by performing the create wallet workflow.
  CreateWallet r -> createWallet r

  -- Evaluate a UseWallet action by performing the restore wallet workflow.
  UseWallet { walletName } -> do
    restore walletName

  -- Evaluate a PabWebSocketSend action by reading from the output queue and
  -- checking the content against the expectations.
  PabWebSocketSend { expectPayload } -> expectPabWebsocketSend expectPayload

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

dropWallet
  :: forall m. MonadTest m => MonadError Error m => MonadUser m => m Unit
dropWallet = openMyWallet do
  clickM $ getBy role do
    nameRegex "drop" ignoreCase
    pure Button

openMyWallet
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => m Unit
  -> m Unit
openMyWallet action = do
  clickM $ getBy role do
    nameRegex "my wallet" ignoreCase
    pure Link
  card <- findBy role $ pure Dialog
  withContainer card action

expectPlutusContractSubscribe
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadAff m
  => MonadAsk Coenv m
  => PlutusAppId
  -> m Unit
expectPlutusContractSubscribe plutusAppId = do
  expectPabWebsocketSend
    { tag: "Subscribe"
    , contents: { "Left": { unContractInstanceId: plutusAppId } }
    }

expectPlutusContractActivation
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadAff m
  => MonadAsk Coenv m
  => WalletId
  -> MarloweContract
  -> PlutusAppId
  -> m Unit
expectPlutusContractActivation walletId contractType plutusAppId = do
  expectJsonRequest ado
    expectMethod POST
    expectUri "/pab/api/contract/activate"
    expectJsonContent
      { caWallet: { prettyWalletName: jsonNull, getWalletId: walletId }
      , caID: encodeJson contractType
      }
    in { unContractInstanceId: plutusAppId }

createWallet
  :: forall m
   . MonadTest m
  => MonadError Error m
  => MonadUser m
  => MonadAsk Coenv m
  => MonadMockHTTP m
  => CreateWalletRecord
  -> m Unit
createWallet
  { walletName
  , mnemonic
  , walletId
  , pubKeyHash
  , address
  , walletCompanionId
  , marloweAppId
  } = do
  clickM $ getBy role do
    nameRegex "generate" ignoreCase
    pure Button
  dialog <- getBy role $ pure Dialog
  withContainer dialog do
    fillCreateWalletDialog
    expectCreateHttpRequest
    fillConfirmMnemonicDialog
    expectWalletActivation
  where
  fillCreateWalletDialog = do

    nicknameField <- getBy role do
      nameRegex "wallet nickname" ignoreCase
      pure Textbox
    click nicknameField
    type_ nicknameField walletName Nothing
    clickM $ getBy role do
      nameRegex "create wallet" ignoreCase
      pure Button
  expectCreateHttpRequest = do
    expectJsonRequest ado
      expectMethod POST
      expectUri "/api/wallet/v1/centralized-testnet/create"
      expectJsonContent $
        { getCreateWalletName: walletName
        , getCreatePassphrase: "fixme-allow-pass-per-wallet"
        }

      in
        { mnemonic: split (Pattern " ") mnemonic
        , walletInfo:
            { walletId
            , pubKeyHash:
                { unPaymentPubKeyHash: { getPubKeyHash: pubKeyHash }
                }
            , address
            }
        }

  fillConfirmMnemonicDialog = do
    mnemonicText <- findBy role $ pure Mark
    mnemonicText `shouldHaveText` mnemonic
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

  expectWalletActivation = do
    expectJsonRequest ado
      expectMethod GET
      expectUri $ "/pab/api/contract/instances/wallet/" <> walletId <> "?"
      in ([] :: Array Json)

    expectPlutusContractActivation walletId WalletCompanion walletCompanionId
    expectPlutusContractActivation walletId MarloweApp marloweAppId
    expectPabWebsocketSend
      { tag: "Subscribe"
      , contents: { "Right": { getPubKeyHash: pubKeyHash } }
      }
    expectPlutusContractSubscribe walletCompanionId
    expectPlutusContractSubscribe marloweAppId

restore
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadError Error m
  => MonadAsk Coenv m
  => WalletName
  -> m Unit
restore walletName = do
  wallets <- asks _.wallets
  mMnemonic <- liftEffect $ Bimap.lookupL walletName <$> Ref.read wallets
  case mMnemonic of
    Nothing -> fail $ "Test error: unknown wallet " <> walletName
    Just mnemonic -> do
      clickM $ getBy role do
        nameRegex "restore" ignoreCase
        pure Button
      dialog <- getBy role $ pure Dialog
      withContainer dialog do
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

expectPabWebsocketSend
  :: forall m a
   . MonadAsk Coenv m
  => MonadError Error m
  => EncodeJson a
  => MonadAff m
  => a
  -> m Unit
expectPabWebsocketSend expectPayload = do
  queue <- asks _.pabWebsocketOut
  msg <- liftAff $ Queue.tryRead queue
  case msg of
    Nothing -> throwError $ error $ joinWith "\n"
      [ "A websocket message was expected to be sent:"
      , encodeStringifyJson expectPayload
      ]
    Just msg' -> encodeJson msg' `shouldEqualJson` encodeJson expectPayload
