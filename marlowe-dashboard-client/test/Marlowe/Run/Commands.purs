-- Commands are reusable test driver actions that do exactly one thing. They
-- can be combined into flows, defined elsewhere.
module Test.Marlowe.Run.Commands where

import Prologue

import Concurrent.Queue as Queue
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  )
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Writer (class MonadTell, tell)
import Control.Parallel (parOneOf)
import Data.Address (Address)
import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyArray)
import Data.Argonaut.Extra (encodeStringifyJson)
import Data.Foldable (class Foldable)
import Data.HTTP.Method (Method(..))
import Data.Map (Map)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.PubKeyHash (PubKeyHash)
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..))
import Data.UUID.Argonaut (UUID)
import Data.UUID.Argonaut as UUID
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Data.WalletNickname (WalletNickname)
import Effect.Aff (Error, delay, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Subscription (notify)
import Marlowe.PAB (PlutusAppId(..))
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
  , restoreRequest
  , restoreResponse
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
import WebSocket.Support (FromSocket(..))

-------------------------------------------------------------------------------
-- Mock HTTP Server
-------------------------------------------------------------------------------

handlePostCreateWallet
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => MonadThrow Error m
  => WalletNickname
  -> Address
  -> MnemonicPhrase
  -> PubKeyHash
  -> WalletId
  -> m Unit
handlePostCreateWallet walletName address mnemonic pubKeyHash walletId =
  handleHTTPRequest POST "/api/wallet/v1/centralized-testnet/create" ado
    expectJsonContent $ createWalletRequest walletName
    in createWalletResponse mnemonic walletId address pubKeyHash

handlePostRestoreWallet
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => MonadThrow Error m
  => WalletNickname
  -> Address
  -> MnemonicPhrase
  -> PubKeyHash
  -> WalletId
  -> m Unit
handlePostRestoreWallet walletName address mnemonic pubKeyHash walletId =
  handleHTTPRequest POST "/api/wallet/v1/centralized-testnet/restore" ado
    expectJsonContent $ restoreRequest walletName mnemonic
    in restoreResponse walletId address pubKeyHash

handleGetContractInstances
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => WalletId
  -> Array (ContractInstanceClientState MarloweContract)
  -> m Unit
handleGetContractInstances walletId = handleHTTPRequest GET uri <<< pure
  where
  uri = "/pab/api/contract/instances/wallet/" <> WI.toString walletId <> "?"

handlePostCreate
  :: forall m
   . MonadTell (Array String) m
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

handlePostEndpoint
  :: forall a m
   . EncodeJson a
  => MonadMockHTTP m
  => MonadTell (Array String) m
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
  => MonadTell (Array String) m
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
  => MonadTell (Array String) m
  => Method
  -> String
  -> RequestMatcher a
  -> m Unit
handleHTTPRequest method uri matcher = do
  tell [ joinWith " " [ "⇵", show method, uri ] ]
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
  => MonadTell (Array String) m
  => UUID
  -> UUID
  -> MarloweParams
  -> m Unit
sendCreateSuccess appId reqId =
  sendWebsocketMessage <<< createSuccessMessage appId reqId

sendWalletCompanionUpdate
  :: forall f m
   . Foldable f
  => Functor f
  => MonadAsk Coenv m
  => MonadTell (Array String) m
  => MonadEffect m
  => UUID
  -> f (Tuple MarloweParams MarloweData)
  -> m Unit
sendWalletCompanionUpdate companionId =
  sendWebsocketMessage <<< walletCompantionMessage companionId

sendNewActiveEndpoints
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => UUID
  -> Array String
  -> m Unit
sendNewActiveEndpoints instanceId =
  sendWebsocketMessage <<< instanceUpdate instanceId <<< newActiveEndpoints

recvInstanceSubscribe
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => UUID
  -> m Unit
recvInstanceSubscribe instanceId = do
  recvWebsocketMessage $ subscribeApp instanceId

sendWebsocketMessage
  :: forall m
   . MonadAsk Coenv m
  => MonadTell (Array String) m
  => MonadEffect m
  => CombinedWSStreamToClient
  -> m Unit
sendWebsocketMessage payload = do
  tell [ "↑ Send websocket message to app: " <> encodeStringifyJson payload ]
  listener <- asks _.pabWebsocketIn
  liftEffect $ notify listener $ ReceiveMessage $ Right payload

recvWebsocketMessage
  :: forall m a
   . MonadAsk Coenv m
  => MonadError Error m
  => MonadTell (Array String) m
  => EncodeJson a
  => MonadAff m
  => a
  -> m Unit
recvWebsocketMessage expected = do
  tell [ "↓ Recv websocket message from app: " <> encodeStringifyJson expected ]
  queue <- asks _.pabWebsocketOut
  msg <- liftAff $ parOneOf
    [ Just <$> Queue.read queue
    , Nothing <$ delay (Milliseconds 1000.0)
    ]
  case msg of
    Nothing -> throwError $ error $ joinWith "\n"
      [ "A websocket message was expected to be sent:"
      , encodeStringifyJson expected
      ]
    Just msg' -> encodeJson msg' `shouldEqualJson` encodeJson expected
