module Forms where

import Prologue hiding (div)

import Component.Input.View as Input
import Component.Label.View as Label
import Css as Css
import Data.Filterable (filter)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.MnemonicPhrase
  ( class CheckMnemonic
  , MnemonicPhrase
  , MnemonicPhraseError
  )
import Data.MnemonicPhrase as MP
import Data.Set (Set)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form (AsyncInput, Form, FormHTML, FormM)
import Halogen.Form as Form
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..))
import Type.Proxy (Proxy(..))

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
  :: forall parentAction s m e a
   . Monad m
  => String
  -> String
  -> (e -> String)
  -> String
  -> RemoteData e a
  -> FormM String m (FormHTML parentAction String (InputSlots s) m)
inputAsync id label renderError value = case _ of
  Loading ->
    render $ Just "Checking..."
  Failure e ->
    render $ Just $ renderError e
  NotAsked ->
    render Nothing
  Success _ ->
    render Nothing
  where
  render error = pure
    [ HH.slot _input id inputComponent { value, id, label, error } Form.Update ]

input
  :: forall parentAction s m e a
   . Monad m
  => String
  -> String
  -> (e -> String)
  -> String
  -> Either e a
  -> FormM String m (FormHTML parentAction String (InputSlots s) m)
input id label renderError value = case _ of
  Left e ->
    render $ Just $ renderError e
  Right _ ->
    render Nothing
  where
  render error = pure
    [ HH.slot _input id inputComponent { value, id, label, error } Form.Update ]

walletNickname
  :: forall parentAction s m
   . Monad m
  => Set WalletNickname
  -> Form parentAction (InputSlots s) m String WalletNickname
walletNickname used =
  Form.mkForm
    { validator: WN.validatorExclusive used
    , render: input "wallet-nickname" "Wallet nickname" case _ of
        WN.Empty -> "Required."
        WN.Exists -> "Already exists."
        WN.DoesNotExist -> "Not found."
        WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."
    }

type MnemonicPhraseInput = AsyncInput String MnemonicPhraseError MnemonicPhrase

mnemonicPhrase
  :: forall parentAction s m
   . CheckMnemonic m
  => Form parentAction (InputSlots s) m MnemonicPhraseInput MnemonicPhrase
mnemonicPhrase =
  Form.mkAsyncForm
    { validator: MP.validator
    , render: inputAsync "wallet-mnemonic" "Mnemonic phrase" case _ of
        MP.Empty -> "Required."
        MP.WrongWordCount -> "24 words required."
        MP.ContainsInvalidWords -> "Mnemonic phrase contains invalid words."
    }
