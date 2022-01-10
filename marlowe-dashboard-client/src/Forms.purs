module Forms where

import Prologue hiding (div)

import Component.Input.View as Input
import Component.Label.View as Label
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.Filterable (filter)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.MnemonicPhrase
  ( class CheckMnemonic
  , MnemonicPhrase
  , MnemonicPhraseError
  )
import Data.MnemonicPhrase as MP
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Validation.Semigroup (V(..))
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (Form)
import Halogen.Form as Form
import Halogen.Form.FormM (setInput, uniqueId)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..), fromEither)
import Polyform (Validator)
import Polyform.Validator as Validator
import Type.Proxy (Proxy(..))

data AsyncInput i e a = AsyncInput i (RemoteData e a)

derive instance eqAsyncInput :: (Eq i, Eq e, Eq a) => Eq (AsyncInput i e a)
derive instance genericAsyncInput :: Generic (AsyncInput i e a) _
derive instance functorAsyncInput :: Functor (AsyncInput i e)
instance showAsyncInput :: (Show i, Show e, Show a) => Show (AsyncInput i e a) where
  show = genericShow

type InputSlots slots =
  (input :: forall query. H.Slot query String String | slots)

_input :: Proxy "input"
_input = Proxy

inputComponent
  :: forall q m
   . H.Component
       q
       { error :: Maybe String
       , id :: String
       , label :: String
       , value :: String
       }
       String
       m
inputComponent =
  Hooks.component \{ outputToken } { value, id, label, error } -> Hooks.do
    Tuple pristine pristineId <- Hooks.useState true
    let error' = filter (\_ -> not pristine) error
    Hooks.pure do
      HH.div [ classNames [ "relative" ] ]
        [ Input.renderWithChildren
            Input.defaultInput
              { value = value
              , onChange = Just \value' -> do
                  Hooks.put pristineId false
                  Hooks.raise outputToken value'
              , invalid = isJust error'
              , id = id
              }
            \i ->
              [ Label.render Label.defaultInput
                  { for = id
                  , text = label
                  }
              , i
              ]
        , HH.label
            [ classNames
                $ Css.inputError <> maybe [ "invisible" ] (const []) error'
            , HP.for id
            ]
            [ HH.text $ fromMaybe "Valid" error ]
        ]

inputAsync
  :: forall s m e a
   . Monad m
  => String
  -> String
  -> Validator m e String a
  -> (e -> String)
  -> Form (InputSlots s) m (AsyncInput String e a) a
inputAsync baseId label validator renderError =
  Form.form \(AsyncInput value remote) -> do
    remote' <- case remote of
      NotAsked -> do
        case value of
          "" -> pure $ NotAsked
          _ -> do
            setInput (AsyncInput value Loading)
            V result <- lift $ Validator.runValidator validator value
            let newRemote = fromEither result
            setInput (AsyncInput value newRemote)
            pure newRemote
      r -> pure r
    case remote' of
      Loading ->
        mkResult value Nothing $ Just "Checking..."
      Failure e ->
        mkResult value Nothing $ Just $ renderError e
      NotAsked ->
        mkResult value Nothing Nothing
      Success a ->
        mkResult value (Just a) Nothing

  where
  mkResult value output error = do
    id <- uniqueId baseId
    let
      componentInput =
        { value
        , id
        , label
        , error
        }
    pure $ Tuple output
      [ HH.slot
          _input
          id
          inputComponent
          componentInput
          \value' -> AsyncInput value' NotAsked
      ]

input
  :: forall s m e a
   . Monad m
  => String
  -> String
  -> Validator m e String a
  -> (e -> String)
  -> Form (InputSlots s) m String a
input baseId label validator renderError = Form.form \value -> do
  V result <- lift $ Validator.runValidator validator value
  case result of
    Left e ->
      mkResult value Nothing $ Just $ renderError e
    Right a ->
      mkResult value (Just a) Nothing
  where
  mkResult value output error = do
    id <- uniqueId baseId
    let
      componentInput =
        { value
        , id
        , label
        , error
        }
    pure $ Tuple output
      [ HH.slot _input id inputComponent componentInput identity ]

walletNickname
  :: forall s m
   . Monad m
  => Set WalletNickname
  -> Form (InputSlots s) m String WalletNickname
walletNickname used =
  input "wallet-nickname" "Wallet nickname" (WN.validator used) case _ of
    WN.Empty -> "Required."
    WN.Exists -> "Already exists."
    WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."

type MnemonicPhraseInput = AsyncInput String MnemonicPhraseError MnemonicPhrase

mnemonicPhrase
  :: forall s m
   . CheckMnemonic m
  => Form (InputSlots s) m MnemonicPhraseInput MnemonicPhrase
mnemonicPhrase =
  inputAsync "wallet-mnemonic" "Mnemonic phrase" MP.validator case _ of
    MP.Empty -> "Required."
    MP.WrongWordCount -> "24 words required."
    MP.ContainsInvalidWords -> "Mnemonic phrase contains invalid words."
