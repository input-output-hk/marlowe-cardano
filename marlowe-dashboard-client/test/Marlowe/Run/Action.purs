module Test.Marlowe.Run.Action where

import Prologue

import Affjax.StatusCode (StatusCode(..))
import Data.Argonaut
  ( class EncodeJson
  , Json
  , JsonDecodeError(..)
  , decodeJson
  , encodeJson
  , jsonNull
  , stringify
  , (.!=)
  , (.:)
  , (.:?)
  )
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Extra (encodeStringifyJson)
import Data.Bifunctor (lmap)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Generic.Rep (class Generic)
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Page.Welcome.RestoreWallet.Types (RestoreWalletParams)

type WalletName = String

data HttpExpectContent
  = ExpectJson Json
  | ExpectText String

derive instance Eq HttpExpectContent
derive instance Ord HttpExpectContent
derive instance Generic HttpExpectContent _

instance DecodeJson HttpExpectContent where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    case tag of
      "ExpectJson" ->
        lmap (Named "ExpectJson") $ ExpectJson <$> obj .: "content"
      "ExpectText" ->
        lmap (Named "ExpectText") $ ExpectText <$> obj .: "content"
      _ ->
        Left $ Named "tag" $ UnexpectedValue $ encodeJson tag

instance EncodeJson HttpExpectContent where
  encodeJson (ExpectJson content) = encodeJson { tag: "ExpectJson", content }
  encodeJson (ExpectText content) = encodeJson { tag: "ExpectText", content }

instance Show HttpExpectContent where
  show (ExpectJson json) = "(ExpectJson " <> stringify json <> ")"
  show (ExpectText text) = "(ExpectText " <> show text <> ")"

data HttpRespondContent
  = RespondJson Json
  | RespondText String

derive instance Eq HttpRespondContent
derive instance Ord HttpRespondContent
derive instance Generic HttpRespondContent _

instance DecodeJson HttpRespondContent where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    case tag of
      "RespondJson" ->
        lmap (Named "RespondJson") $ RespondJson <$> obj .: "content"
      "RespondText" ->
        lmap (Named "RespondText") $ RespondText <$> obj .: "content"
      _ ->
        Left $ Named "tag" $ UnexpectedValue $ encodeJson tag

instance EncodeJson HttpRespondContent where
  encodeJson (RespondJson content) = encodeJson { tag: "RespondJson", content }
  encodeJson (RespondText content) = encodeJson { tag: "RespondText", content }

instance Show HttpRespondContent where
  show (RespondJson json) = "(RespondJson " <> stringify json <> ")"
  show (RespondText text) = "(RespondText " <> show text <> ")"

newtype HttpExpect = HttpExpect
  { method :: Maybe String
  , uri :: Maybe String
  , headers :: Maybe (Array (Tuple String String))
  , content :: Maybe HttpExpectContent
  }

derive instance Eq HttpExpect
derive instance Ord HttpExpect
derive instance Generic HttpExpect _

instance Show HttpExpect where
  show = genericShow

instance DecodeJson HttpExpect where
  decodeJson json = do
    obj <- decodeJson json
    method <- obj .:? "method"
    uri <- obj .:? "uri"
    headers <- obj .:? "headers"
    content <- obj .:? "content"
    pure $ HttpExpect { method, uri, headers, content }

instance EncodeJson HttpExpect where
  encodeJson (HttpExpect { method, uri, headers, content }) = encodeJson
    { method: maybe jsonNull encodeJson method
    , uri: maybe jsonNull encodeJson uri
    , headers: maybe jsonNull encodeJson headers
    , content: maybe jsonNull encodeJson content
    }

newtype HttpRespond = HttpRespond
  { status :: StatusCode
  , statusText :: String
  , headers :: Array (Tuple String String)
  , content :: HttpRespondContent
  }

derive instance Eq HttpRespond
derive instance Ord HttpRespond
derive instance Generic HttpRespond _

instance Show HttpRespond where
  show = genericShow

instance DecodeJson HttpRespond where
  decodeJson json = do
    obj <- decodeJson json
    status <- StatusCode <$> obj .:? "status" .!= 200
    statusText <- obj .:? "statusText" .!= "OK"
    headers <- obj .:? "headers" .!= []
    content <- obj .:? "content" .!= RespondJson jsonNull
    pure $ HttpRespond { status, statusText, headers, content }

instance EncodeJson HttpRespond where
  encodeJson
    (HttpRespond { status: StatusCode status, statusText, headers, content }) =
    encodeJson { status, statusText, headers, content }

data MarloweRunAction
  = CreateWallet { walletName :: WalletName }
  | UseWallet { walletName :: WalletName }
  | PabWebSocketSend { expectPayload :: Json }
  | PabWebSocketReceive { payload :: Json }
  | HttpRequest { expect :: HttpExpect, respond :: HttpRespond }

derive instance Eq MarloweRunAction
derive instance Ord MarloweRunAction
derive instance Generic MarloweRunAction _

instance Show MarloweRunAction where
  show = case _ of
    CreateWallet a -> "(CreateWallet " <> show a <> ")"
    UseWallet a -> "(UseWallet " <> show a <> ")"
    PabWebSocketSend a -> "(PabWebSocketSend " <> encodeStringifyJson a <> ")"
    PabWebSocketReceive a ->
      "(PabWebSocketReceive " <> encodeStringifyJson a <> ")"
    HttpRequest a -> "HttpRequest " <> show a <> ")"

instance DecodeJson MarloweRunAction where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    case tag of
      "CreateWallet" ->
        lmap (Named "CreateWallet") $ CreateWallet <$> obj .: "content"
      "UseWallet" ->
        lmap (Named "UseWallet") $ UseWallet <$> obj .: "content"
      "PabWebSocketSend" ->
        lmap (Named "PabWebSocketSend") $ PabWebSocketSend <$> obj .: "content"
      "PabWebSocketReceive" ->
        lmap (Named "PabWebSocketReceive")
          $ PabWebSocketReceive <$> obj .: "content"
      "HttpRequest" ->
        lmap (Named "HttpRequest") $ HttpRequest <$> obj .: "content"
      _ ->
        Left $ Named "tag" $ UnexpectedValue $ encodeJson tag

instance EncodeJson MarloweRunAction where
  encodeJson = case _ of
    CreateWallet content -> encodeJson { tag: "CreateWallet", content }
    UseWallet content -> encodeJson { tag: "UseWallet", content }
    PabWebSocketSend content -> encodeJson { tag: "PabWebSocketSend", content }
    PabWebSocketReceive content ->
      encodeJson { tag: "PabWebSocketReceive", content }
    HttpRequest content -> encodeJson { tag: "HttpRequest", content }

type MarloweRunTestState =
  { wallets :: Bimap WalletName RestoreWalletParams
  }

type MarloweRunTestStateFn = MarloweRunTestState -> MarloweRunTestState

addWallet :: WalletName -> RestoreWalletParams -> MarloweRunTestStateFn
addWallet name params state = state
  { wallets = Bimap.insert name params state.wallets }
