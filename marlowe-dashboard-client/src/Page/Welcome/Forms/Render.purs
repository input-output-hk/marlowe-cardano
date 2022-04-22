module Page.Welcome.Forms.Render where

import Prologue

import Component.Button.Types as Button
import Component.Button.View (button)
import Component.Form (HasHintSlot, renderTextInput)
import Component.Progress.Circular as Progress
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Bifunctor (lmap)
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.These (These(..))
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Halogen.Css (classNames)
import Halogen.Form.Input (FieldState)
import Halogen.Form.Input as Input
import Halogen.HTML (HTML)
import Halogen.HTML as HH

type RenderOpts widget action =
  { body :: Array (HTML widget action)
  , inProgress :: Boolean
  , onCancel ::
      { action :: Maybe action
      , label :: String
      }
  , onSkip ::
      Maybe
        { action :: Maybe action
        , label :: String
        }
  , onSubmit ::
      { action :: Maybe action
      , label :: String
      }
  , title :: String
  }

renderForm
  :: forall action widget. RenderOpts widget action -> HTML widget action
renderForm { body, inProgress, onCancel, onSkip, onSubmit, title } =
  HH.div
    [ classNames [ "p-5", "lg:p-6", "space-y-4" ] ]
    $
      [ HH.h2
          [ classNames [ "font-bold" ] ]
          [ HH.text title ]
      ]
        <> body
        <>
          [ HH.div
              [ classNames [ "flex", "justify-center", "gap-4" ] ]
              if inProgress then
                [ Progress.view Progress.defaultSpec
                    { color = "text-purple"
                    , width = "w-14"
                    , height = "h-14"
                    }
                ]
              else
                [ button
                    Button.Secondary
                    onCancel.action
                    [ "flex-1" ]
                    [ HH.text onCancel.label ]
                , button
                    Button.Primary
                    onSubmit.action
                    [ "flex-1" ]
                    [ HH.text onSubmit.label ]
                ]
                  <> flip foldMap onSkip \{ action, label } -> pure $ button
                    Button.Secondary
                    action
                    [ "flex-1" ]
                    [ HH.text label ]

          ]

mkNicknameInput
  :: forall action m slots
   . MonadAff m
  => Boolean
  -> AddressBook
  -> FieldState WalletNickname
  -> Input.Input
       action
       (Either WN.WalletNicknameError Unit)
       WalletNickname
       (HasHintSlot slots)
       m
mkNicknameInput warnOnExists addressBook fieldState =
  { fieldState
  , format: WN.toString
  , validate:
      notInAddressBook <=< either This That <<< lmap Left <<< WN.fromString
  , render: \{ error, value, result } ->
      renderTextInput id label Nothing result error
        (Input.setInputProps value [])
        case _ of
          Left WN.Empty -> "Required."
          Left WN.ContainsNonAlphaNumeric ->
            "Can only contain letters and digits."
          Right _ ->
            if warnOnExists then "Warning: already exists."
            else "Already exists."
  }
  where
  id = "restore-wallet-nickname"
  label = "Wallet nickname"
  notInAddressBook nickname
    | AddressBook.containsNickname nickname addressBook =
        if warnOnExists then Both (Right unit) nickname
        else This (Right unit)
    | otherwise = That nickname

mkMnemonicInput
  :: forall action slots m
   . MonadAff m
  => FieldState MnemonicPhrase
  -> Input.Input
       action
       MP.MnemonicPhraseError
       MnemonicPhrase
       (HasHintSlot slots)
       m
mkMnemonicInput fieldState =
  { fieldState
  , format: MP.toString
  , validate: either This That <<< MP.fromString
  , render: \{ error, value, result } ->
      renderTextInput id label Nothing result error
        (Input.setInputProps value [])
        case _ of
          MP.Empty -> "Required."
          MP.WrongWordCount -> "24 words required."
          MP.ContainsInvalidWords -> "Mnemonic phrase contains invalid words."
  }
  where
  id = "restore-wallet-mnemonic"
  label = "Mnemonic phrase"
