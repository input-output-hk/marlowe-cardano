-- Commands are reusable test driver actions that do exactly one thing. They
-- can be combined into flows, defined elsewhere.
module Test.Marlowe.Run.Commands where

import Prologue

import Concurrent.Queue as Queue
import Control.Logger.Capability (class MonadLogger, debug)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (untilJust)
import Control.Parallel (parOneOf)
import Data.Address (Address)
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyArray)
import Data.Argonaut.Extra (encodeStringifyJson)
import Data.Foldable (class Foldable)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.String (joinWith)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..))
import Data.UUID.Argonaut (UUID)
import Data.UUID.Argonaut as UUID
import Data.Undefinable (toUndefinable)
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Data.WalletNickname (WalletNickname)
import Effect.Aff (Error, delay, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Subscription (notify)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Run.Wallet.V1.Types (WalletInfo)
import Marlowe.Semantics (Contract, MarloweData, MarloweParams)
import MarloweContract (MarloweContract)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , ContractInstanceClientState
  )
import Test.Assertions (shouldEqualJson)
import Test.Data.Marlowe
  ( createContent
  , createEndpoint
  , createSuccessMessage
  , createWalletRequest
  , createWalletResponse
  , followEndpoint
  , followerMessage
  , restoreRequest
  , walletCompantionMessage
  )
import Test.Data.Plutus
  ( contractActivationArgs
  , instanceUpdate
  , newActiveEndpoints
  , subscribeApp
  )
import Test.Marlowe.Run (Coenv)
import Test.Network.HTTP
  ( class MonadMockHTTP
  , RequestMatcher
  , expectJsonContent
  , expectJsonRequest
  , expectMethod
  , expectUri
  )
import Test.Web.DOM.Assertions (shouldCast, shouldNotBeDisabled)
import Test.Web.DOM.Query (findBy, getBy, nameRegex, role)
import Test.Web.Event.User (click, clickM, type_)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))
import WebSocket.Support (FromSocket(..))

-------------------------------------------------------------------------------
-- UI Navigation - Dashboard Page
-------------------------------------------------------------------------------

openMyWalletDialog
  :: forall m a
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadLogger String m
  => MonadTest m
  => m a
  -> m a
openMyWalletDialog action = do
  clickLinkRegex "my wallet"
  card <- findBy role $ pure Dialog
  withContainer card action

openContactsDialog
  :: forall m a
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadLogger String m
  => MonadTest m
  => m a
  -> m a
openContactsDialog action = do
  clickLinkRegex "contacts"
  card <- findBy role $ pure Dialog
  withContainer card action

openNewContractDialog
  :: forall m a
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadLogger String m
  => MonadTest m
  => m a
  -> m a
openNewContractDialog action = do
  clickLinkRegex "create a new contract"
  card <- findBy role $ pure Dialog
  withContainer card action

typeAddress
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => String
  -> m Unit
typeAddress = typeTextboxRegex "address"

typeContractTitle
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => String
  -> m Unit
typeContractTitle = typeTextboxRegex "contract title"

typeContractValue
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => String
  -> String
  -> m Unit
typeContractValue name value = do
  textbox <- getBy role do
    nameRegex name ignoreCase
    pure Spinbutton
  type_ textbox value $ Just
    { skipClick: false
    , skipAutoClose: true
    , initialSelectionStart: toUndefinable $ Just 0
    , initialSelectionEnd: toUndefinable $ Just 10
    }

clickConfirm
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickConfirm = clickButtonRegex "confirm"

clickDrop
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickDrop = clickButtonRegex "drop"

clickNewContact
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickNewContact = clickButtonRegex "new contact"

clickSave
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickSave = clickButtonRegex "save"

clickSetup
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickSetup = clickButtonRegex "setup"

clickReview
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickReview = clickButtonRegex "review"

clickPayAndStart
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickPayAndStart = clickButtonRegex "pay and start"

-------------------------------------------------------------------------------
-- UI Navigation - Welcome Page
-------------------------------------------------------------------------------

openRestoreDialog
  :: forall m a
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m a
  -> m a
openRestoreDialog action = do
  clickButtonRegex "restore testnet wallet"
  dialog <- getBy role $ pure Dialog
  withContainer dialog action

openGenerateDialog
  :: forall m a
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m a
  -> m a
openGenerateDialog action = do
  clickButtonRegex "generate"
  dialog <- getBy role $ pure Dialog
  withContainer dialog action

typeWalletNickname
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => String
  -> m Unit
typeWalletNickname = typeTextboxRegex "wallet nickname"

typeMnemonicPhrase
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => String
  -> m Unit
typeMnemonicPhrase = typeTextboxRegex "mnemonic phrase"

clickCreateWallet
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickCreateWallet = clickButtonRegex "create wallet"

clickRestoreWallet
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => MonadLogger String m
  => m Unit
clickRestoreWallet = clickButtonRegex "restore wallet"

clickOk
  :: forall m
   . MonadAff m
  => MonadLogger String m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => m Unit
clickOk = clickButtonRegex "ok"

-------------------------------------------------------------------------------
-- UI Navigation - Helpers
-------------------------------------------------------------------------------

clickLinkRegex
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadLogger String m
  => MonadTest m
  => String
  -> m Unit
clickLinkRegex regex = do
  debug $ "click link " <> regex
  clickM $ getBy role do
    nameRegex regex ignoreCase
    pure Link

clickButtonRegex
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadLogger String m
  => MonadTest m
  => String
  -> m Unit
clickButtonRegex regex = do
  debug $ "click button " <> regex
  button <- shouldCast =<< getBy role do
    nameRegex regex ignoreCase
    pure Button
  shouldNotBeDisabled button
  click button

typeTextboxRegex
  :: forall m
   . MonadAff m
  => MonadUser m
  => MonadError Error m
  => MonadTest m
  => String
  -> String
  -> m Unit
typeTextboxRegex regex text = do
  textbox <- getBy role do
    nameRegex regex ignoreCase
    pure Textbox
  type_ textbox text Nothing

-------------------------------------------------------------------------------
-- Mock HTTP Server
-------------------------------------------------------------------------------

handlePostCreateWallet
  :: forall m
   . MonadLogger String m
  => MonadMockHTTP m
  => MonadThrow Error m
  => WalletNickname
  -> MnemonicPhrase
  -> WalletInfo
  -> m Unit
handlePostCreateWallet walletName mnemonic walletInfo =
  handleHTTPRequest POST "/api/wallet/v1/centralized-testnet/create" ado
    expectJsonContent $ createWalletRequest walletName
    in createWalletResponse mnemonic walletInfo

handlePostRestoreWallet
  :: forall m
   . MonadLogger String m
  => MonadMockHTTP m
  => MonadThrow Error m
  => WalletNickname
  -> MnemonicPhrase
  -> WalletInfo
  -> m Unit
handlePostRestoreWallet walletName mnemonic walletInfo =
  handleHTTPRequest POST "/api/wallet/v1/centralized-testnet/restore" ado
    expectJsonContent $ restoreRequest walletName mnemonic
    in walletInfo

handleGetContractInstances
  :: forall m
   . MonadLogger String m
  => MonadMockHTTP m
  => WalletId
  -> Array (ContractInstanceClientState MarloweContract)
  -> m Unit
handleGetContractInstances walletId = handleHTTPRequest GET uri <<< pure
  where
  uri = "/pab/api/contract/instances/wallet/" <> WI.toString walletId <> "?"

handlePostCreate
  :: forall m
   . MonadLogger String m
  => MonadMockHTTP m
  => UUID
  -> UUID
  -> Map String Address
  -> Contract
  -> m Unit
handlePostCreate marloweAppId reqId roles contract = do
  handlePostEndpoint marloweAppId createEndpoint ado
    expectJsonContent $ createContent reqId roles contract
    in jsonEmptyArray

handlePostFollow
  :: forall m
   . MonadLogger String m
  => MonadMockHTTP m
  => UUID
  -> MarloweParams
  -> m Unit
handlePostFollow followerAppId params = do
  handlePostEndpoint followerAppId followEndpoint ado
    expectJsonContent params
    in jsonEmptyArray

handlePostEndpoint
  :: forall a m
   . EncodeJson a
  => MonadMockHTTP m
  => MonadLogger String m
  => UUID
  -> String
  -> RequestMatcher a
  -> m Unit
handlePostEndpoint instanceId endpoint =
  handleHTTPRequest POST $ joinWith "/"
    [ "/pab/api/contract/instance"
    , UUID.toString instanceId
    , "endpoint"
    , endpoint
    ]

handlePostActivate
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadLogger String m
  => MonadAff m
  => MonadAsk Coenv m
  => WalletId
  -> MarloweContract
  -> UUID
  -> m Unit
handlePostActivate walletId contractType instanceId =
  handleHTTPRequest POST "/pab/api/contract/activate" ado
    expectJsonContent $ contractActivationArgs walletId contractType
    in PlutusAppId instanceId

handleHTTPRequest
  :: forall a m
   . EncodeJson a
  => MonadMockHTTP m
  => MonadLogger String m
  => Method
  -> String
  -> RequestMatcher a
  -> m Unit
handleHTTPRequest method uri matcher = do
  debug $ joinWith " " [ "⇵", show method, uri ]
  expectJsonRequest ado
    expectMethod method
    expectUri uri
    response <- matcher
    in response

-------------------------------------------------------------------------------
-- Mock WebSocket Server
-------------------------------------------------------------------------------

sendCreateSuccess
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadLogger String m
  => UUID
  -> UUID
  -> MarloweParams
  -> m Unit
sendCreateSuccess appId reqId =
  sendWebsocketMessage "Create success" <<< createSuccessMessage appId reqId

sendWalletCompanionUpdate
  :: forall f m
   . Foldable f
  => Functor f
  => MonadAsk Coenv m
  => MonadLogger String m
  => MonadEffect m
  => UUID
  -> f (Tuple MarloweParams MarloweData)
  -> m Unit
sendWalletCompanionUpdate companionId =
  sendWebsocketMessage "Wallet companion update" <<< walletCompantionMessage
    companionId

sendFollowerUpdate
  :: forall m
   . MonadAsk Coenv m
  => MonadLogger String m
  => MonadEffect m
  => UUID
  -> ContractHistory
  -> m Unit
sendFollowerUpdate followerId =
  sendWebsocketMessage "Follower update" <<< followerMessage followerId

sendNewActiveEndpoints
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadLogger String m
  => MonadAff m
  => MonadAsk Coenv m
  => UUID
  -> Array String
  -> m Unit
sendNewActiveEndpoints instanceId =
  sendWebsocketMessage "New active endpoints" <<< instanceUpdate instanceId <<<
    newActiveEndpoints

recvInstanceSubscribe
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadLogger String m
  => MonadAff m
  => MonadAsk Coenv m
  => UUID
  -> m Unit
recvInstanceSubscribe instanceId = do
  recvWebsocketMessage "Subscribe" $ subscribeApp instanceId

sendWebsocketMessage
  :: forall m
   . MonadAsk Coenv m
  => MonadLogger String m
  => MonadEffect m
  => String
  -> CombinedWSStreamToClient
  -> m Unit
sendWebsocketMessage description payload = do
  debug $ "↑ Send websocket message to app: " <> description
  listener <- asks _.pabWebsocketIn
  liftEffect $ notify listener $ ReceiveMessage $ Right payload

recvWebsocketMessage
  :: forall m a
   . MonadAsk Coenv m
  => MonadError Error m
  => MonadLogger String m
  => EncodeJson a
  => MonadAff m
  => String
  -> a
  -> m Unit
recvWebsocketMessage description expected = do
  debug $ "↓ Recv websocket message from app: " <> description
  queue <- asks _.pabWebsocketOut
  msg <- liftAff $ parOneOf
    [ Just <$> untilJust do
        result <- Queue.tryRead queue
        case result of
          Just a -> pure $ Just a
          Nothing -> do
            delay $ Milliseconds 10.0
            pure Nothing
    , Nothing <$ delay (Milliseconds 100.0)
    ]
  case msg of
    Nothing -> throwError $ error $ joinWith "\n"
      [ "A websocket message was expected to be sent:"
      , encodeStringifyJson expected
      ]
    Just msg' -> encodeJson msg' `shouldEqualJson` encodeJson expected
