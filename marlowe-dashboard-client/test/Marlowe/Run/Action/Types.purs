module Test.Marlowe.Run.Action.Types where

import Prologue

import Affjax.StatusCode (StatusCode(..))
import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, withGraphics)
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
import Data.Array (fromFoldable, unsnoc)
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (Method)
import Data.HTTP.Method as Method
import Data.Lens (Prism', prism')
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
import Data.Tuple (uncurry)
import Data.UUID.Argonaut (UUID)
import Effect.Aff (Error, message)
import Language.Marlowe.Client (ContractHistory)
import MarloweContract (MarloweContract(..))
import Web.ARIA (ARIARole)

type WalletName = String
type WalletMnemonic = String
type WalletId = String
type PubKeyHash = String
type Address = String
type PlutusAppId = UUID
type TemplateName = String
type ContractNickname = String
type FieldName = String
type FieldValue = String
type EndpointName = String
type CurrencySymbol = String
type ScriptHash = String
type ContractField =
  { name :: FieldName
  , value :: FieldValue
  , role :: ARIARole
  }

type ContractRoles =
  { roleName :: String
  , walletName :: WalletName
  , address :: Address
  }

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

type CreateWalletRecord =
  { walletName :: WalletName
  , mnemonic :: WalletMnemonic
  , walletId :: WalletId
  , pubKeyHash :: PubKeyHash
  , address :: Address
  , walletCompanionId :: PlutusAppId
  , marloweAppId :: PlutusAppId
  }

type CreateContractRecord =
  { templateName :: TemplateName
  , contractTitle :: ContractNickname
  , fields :: Array ContractField
  , roles :: Array ContractRoles
  , marloweAppId :: PlutusAppId
  , followerId :: PlutusAppId
  , walletCompanionId :: PlutusAppId
  , walletId :: WalletId
  , currencySymbol :: CurrencySymbol
  , rolePayoutValidatorHash :: ScriptHash
  }

data AppInstance
  = MarloweAppInstance PlutusAppId
  | WalletCompanionInstance PlutusAppId
  | MarloweFollowerInstance PlutusAppId ContractHistory

_MarloweAppInstance :: Prism' AppInstance PlutusAppId
_MarloweAppInstance = prism' MarloweAppInstance case _ of
  MarloweAppInstance id -> Just id
  _ -> Nothing

_WalletCompanionInstance :: Prism' AppInstance PlutusAppId
_WalletCompanionInstance = prism' WalletCompanionInstance case _ of
  WalletCompanionInstance id -> Just id
  _ -> Nothing

_MarloweFollowerInstance
  :: Prism' AppInstance (Tuple PlutusAppId ContractHistory)
_MarloweFollowerInstance = prism' (uncurry MarloweFollowerInstance) case _ of
  MarloweFollowerInstance id history -> Just $ Tuple id history
  _ -> Nothing

instance DecodeJson AppInstance where
  decodeJson json = do
    obj <- decodeJson json
    appType <- obj .: "type"
    case appType of
      MarloweApp -> lmap (Named "MarloweAppInstance") $
        MarloweAppInstance <$> obj .: "instanceId"
      WalletCompanion -> lmap (Named "WalletCompanionInstance") $
        WalletCompanionInstance <$> obj .: "instanceId"
      MarloweFollower -> lmap (Named "MarloweFollowerInstance") $
        MarloweFollowerInstance
          <$> obj .: "instanceId"
          <*> obj .: "history"

instance EncodeJson AppInstance where
  encodeJson = case _ of
    MarloweAppInstance instanceId -> encodeJson
      { type: MarloweApp, instanceId }
    WalletCompanionInstance instanceId -> encodeJson
      { type: WalletCompanion, instanceId }
    MarloweFollowerInstance instanceId history -> encodeJson
      { type: MarloweFollower, instanceId, history }

derive instance Generic AppInstance _
derive instance Eq AppInstance
instance Show AppInstance where
  show = genericShow

data MarloweRunAction
  = CreateWallet CreateWalletRecord
  | CreateContract CreateContractRecord
  | FundWallet { walletName :: WalletName, lovelace :: BigInt }
  | AddContact { walletName :: WalletName, address :: Address }
  | DropWallet { walletId :: WalletId }
  | RestoreWallet { walletName :: WalletName, instances :: Array AppInstance }
  | ExpectNoHTTPCall

derive instance Generic MarloweRunAction _
derive instance Eq MarloweRunAction

instance Show MarloweRunAction where
  show = case _ of
    CreateWallet a -> "(CreateWallet " <> show a <> ")"
    CreateContract { templateName } -> "(CreateContract " <> templateName <> ")"
    DropWallet a -> "(DropWallet " <> show a <> ")"
    FundWallet a -> "(FundWallet " <> show a <> ")"
    AddContact a -> "(AddContact " <> show a <> ")"
    RestoreWallet a -> "(RestoreWallet " <> show a <> ")"
    ExpectNoHTTPCall -> "ExpectNoHTTPCall"

instance DecodeJson MarloweRunAction where
  decodeJson json = do
    obj <- decodeJson json
    tag <- obj .: "tag"
    case tag of
      "DropWallet" ->
        lmap (Named "DropWallet") $ DropWallet <$> obj .: "content"
      "CreateWallet" ->
        lmap (Named "CreateWallet") $ CreateWallet <$> obj .: "content"
      "FundWallet" ->
        lmap (Named "FundWallet") $ FundWallet <$> obj .: "content"
      "CreateContract" ->
        lmap (Named "CreateContract") $ CreateContract <$> obj .: "content"
      "AddContact" ->
        lmap (Named "AddContact") $ AddContact <$> obj .: "content"
      "RestoreWallet" ->
        lmap (Named "RestoreWallet") $ RestoreWallet <$> obj .: "content"
      "ExpectNoHTTPCall" ->
        pure ExpectNoHTTPCall
      _ ->
        Left $ Named "tag" $ UnexpectedValue $ encodeJson tag

instance EncodeJson MarloweRunAction where
  encodeJson = case _ of
    DropWallet content -> encodeJson { tag: "DropWallet", content }
    CreateWallet content -> encodeJson { tag: "CreateWallet", content }
    CreateContract content -> encodeJson { tag: "CreateContract", content }
    FundWallet content -> encodeJson { tag: "FundWallet", content }
    AddContact content -> encodeJson { tag: "AddContact", content }
    RestoreWallet content -> encodeJson { tag: "RestoreWallet", content }
    ExpectNoHTTPCall -> encodeJson { tag: "ExpectNoHTTPCall" }

type MarloweRunScript = Array (Tuple Milliseconds MarloweRunAction)

data ScriptError = ScriptError
  (Array MarloweRunAction)
  MarloweRunAction
  (Array String)
  Error

renderScriptError :: ScriptError -> String
renderScriptError
  (ScriptError succeededActions failedAction steps e) =
  joinWith "\n  " $ join
    [ pure "Test script failed at the indicated step:"
    , pure ""
    , reportAction true <$> succeededActions
    , pure $ reportAction false failedAction
    , unsnoc steps # fromFoldable >>= \{ init, last } -> join
        [ reportStep true <$> init
        , pure $ reportStep false last
        ]
    , pure ""
    , pure "Error was:"
    , pure ""
    , pure
        $ replaceAll (Pattern "\n") (Replacement "\n  ")
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
  reportStep success step =
    "  " <> joinWith " " [ glyph, withGraphics color step ]
    where
    color = foreground if success then Green else Red
    glyph = withGraphics (color <> bold) if success then "✓" else "✗"
