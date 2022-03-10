module Test.Marlowe.Run.Action.Types where

import Prologue

import Affjax.StatusCode (StatusCode(..))
import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, withGraphics)
import Control.Monad.Error.Class (class MonadThrow, throwError)
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
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method)
import Data.HTTP.Method as Method
import Data.Maybe (maybe)
import Data.Show.Generic (genericShow)
import Data.String
  ( Pattern(..)
  , Replacement(..)
  , joinWith
  , length
  , replaceAll
  , take
  )
import Data.Time.Duration (Milliseconds)
import Effect.Aff (Error, error, message)

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
  { method :: Maybe Method
  , uri :: Maybe String
  , headers :: Maybe (Array (Tuple String String))
  , content :: Maybe HttpExpectContent
  }

derive instance Eq HttpExpect
derive instance Ord HttpExpect
derive instance Generic HttpExpect _

instance Show HttpExpect where
  show = genericShow

newtype JsonMethod = JsonMethod Method

unJsonMethod :: JsonMethod -> Method
unJsonMethod (JsonMethod method) = method

instance DecodeJson JsonMethod where
  decodeJson json = do
    str <- decodeJson json
    JsonMethod <$> either
      Right
      (const $ Left $ UnexpectedValue json)
      (Method.fromString str)

instance DecodeJson HttpExpect where
  decodeJson json = do
    obj <- decodeJson json
    method <- map unJsonMethod <$> obj .:? "method"
    uri <- obj .:? "uri"
    headers <- obj .:? "headers"
    content <- obj .:? "content"
    pure $ HttpExpect { method, uri, headers, content }

instance EncodeJson HttpExpect where
  encodeJson (HttpExpect { method, uri, headers, content }) = encodeJson
    { method: maybe jsonNull (encodeJson <<< show) method
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
  | ConfirmMnemonic { walletName :: WalletName }
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
    ConfirmMnemonic a -> "(ConfirmMnemonic " <> show a <> ")"
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
      "ConfirmMnemonic" ->
        lmap (Named "ConfirmMnemonic") $ ConfirmMnemonic <$> obj .: "content"
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
    ConfirmMnemonic content -> encodeJson { tag: "ConfirmMnemonic", content }
    UseWallet content -> encodeJson { tag: "UseWallet", content }
    PabWebSocketSend content -> encodeJson { tag: "PabWebSocketSend", content }
    PabWebSocketReceive content ->
      encodeJson { tag: "PabWebSocketReceive", content }
    HttpRequest content -> encodeJson { tag: "HttpRequest", content }

type MarloweRunScript = Array (Tuple Milliseconds MarloweRunAction)

data ScriptError = ScriptError (Array MarloweRunAction) MarloweRunAction Error

throwScriptError :: forall m a. MonadThrow Error m => ScriptError -> m a
throwScriptError (ScriptError succeeded failed e) =
  throwError $ error $ joinWith "\n    " $ join
    [ pure "Test script failed at the indicated step:"
    , pure ""
    , reportAction true <$> succeeded
    , pure $ reportAction false failed
    , pure ""
    , pure "Error was:"
    , pure ""
    , pure
        $ replaceAll (Pattern "\n") (Replacement "\n    ")
        $ "  " <> message e
    ]
  where
  reportAction success action =
    joinWith " " [ glyph, withGraphics color truncatedAction ]
    where
    actionText = show action
    truncatedAction
      | length actionText <= 120 = actionText
      | otherwise = take 191 actionText <> "…"
    color = foreground if success then Green else Red
    glyph = withGraphics (color <> bold) if success then "✓" else "✗"
