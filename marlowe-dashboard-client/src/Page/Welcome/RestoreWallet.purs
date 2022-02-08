module Page.Welcome.RestoreWallet (component, _restoreWallet) where

import Prologue

import AppM (passphrase) as AppM
import Capability.Marlowe (class ManageMarlowe, restoreWallet)
import Component.ContractSetup.Types (_nickname)
import Component.Form (renderTextInput)
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.AddressBook (AddressBook)
import Data.Lens (is, set, (^?))
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.Variant (default, on) as Variant
import Data.Wallet (WalletDetails)
import Data.WalletNickname (WalletNickname)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.Injective (project)
import Halogen.Form.Input (FieldState)
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Network.RemoteData (RemoteData(..), _Failure, _Loading)
import Page.Welcome.Forms.Render (mkNicknameInput, renderForm)
import Page.Welcome.RestoreWallet.Types
  ( Component
  , Input
  , Msg(..)
  , RestoreWalletFields
  , RestoreWalletParams
  , _mnemonic
  )
import Store as Store
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

data Action
  = OnInit
  | OnReceive (Connected AddressBook Input)
  | OnNicknameMsg (Input.Msg Action WalletNickname)
  | OnMnemonicMsg (Input.Msg Action MnemonicPhrase)
  | OnFormSubmit Event
  | OnCancel
  | OnRestore RestoreWalletParams

type State =
  { addressBook :: AddressBook
  , fields :: RestoreWalletFields
  , result :: Maybe RestoreWalletParams
  , walletDetails :: RemoteData String WalletDetails
  }

type ChildSlots =
  ( nickname :: Input.Slot Action WalletNickname Unit
  , mnemonic :: Input.Slot Action MnemonicPhrase Unit
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

_restoreWallet = Proxy :: Proxy "restoreWallet"

component
  :: forall m
   . MonadAff m
  => ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => Component m
component = connect (selectEq _.addressBook) $ H.mkComponent
  { initialState
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< OnReceive
      , initialize = Just OnInit
      }
  , render
  }

initialState :: Connected AddressBook Input -> State
initialState { context, input: { fields } } =
  { addressBook: context
  , fields
  , result: project fields
  , walletDetails: NotAsked
  }

handleFieldMsg
  :: forall a m
   . MonadEffect m
  => ManageMarlowe m
  => Eq a
  => (FieldState a -> RestoreWalletFields -> RestoreWalletFields)
  -> Input.Msg Action a
  -> DSL m Unit
handleFieldMsg set = case _ of
  Input.Updated field -> do
    { fields } <- H.get
    { fields: newFields } <- H.modify _ { fields = set field fields }
    H.modify_ _ { result = project newFields }
    when (fields /= newFields) do
      H.raise $ FieldsUpdated newFields
  Input.Blurred -> pure unit
  Input.Focused -> pure unit
  Input.Emit action -> handleAction action

handleAction
  :: forall m. MonadEffect m => ManageMarlowe m => Action -> DSL m Unit
handleAction = case _ of
  OnInit -> do
    H.tell _nickname unit $ Input.Focus
  OnReceive input -> do
    oldState <- H.get
    let newState = initialState input
    when (oldState /= newState) $ H.put newState
  OnNicknameMsg msg -> handleFieldMsg (set (prop _nickname)) msg
  OnMnemonicMsg msg -> handleFieldMsg (set (prop _mnemonic)) msg
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnCancel -> H.raise CancelClicked
  OnRestore { nickname, mnemonic } -> do
    H.modify_ _ { walletDetails = Loading }
    response <- lift $ restoreWallet nickname mnemonic AppM.passphrase
    case response of
      Left err -> do
        H.modify_ _
          { walletDetails = err
              #
                ( Variant.default "Error from server."
                    # Variant.on
                        (Proxy :: Proxy "invalidMnemonic")
                        (const "Invalid mnemonic phrase.")
                )
              # Failure
          }
      Right walletDetails -> do
        H.modify_ _ { walletDetails = Success walletDetails }
        H.raise $ WalletRestored walletDetails

render
  :: forall m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML m
render state = do
  let
    { addressBook
    , result
    , fields
    , walletDetails
    } = state
  let serverError = fromMaybe "" $ walletDetails ^? _Failure
  let inProgress = is _Loading walletDetails
  renderForm
    { body:
        [ HH.form
            [ HE.onSubmit OnFormSubmit
            , classNames
                [ "overflow-y-auto"
                , "p-4"
                , "flex"
                , "flex-col"
                , "gap-2"
                ]
            ]
            [ nicknameInput addressBook fields.nickname
            , mnemonicInput fields.mnemonic
            ]
        , HH.p_
            [ HH.b_ [ HH.text "IMPORTANT:" ]
            -- FIXME: as part of SCP-3173, Write a section in the Marlowe Run documentation and add a link to it
            , HH.text "Do not use a real wallet phrase <read more>"
            ]
        -- TODO replace with progress buttons when refactored.
        , HH.p [ classNames Css.inputError ] [ HH.text serverError ]
        ]
    , inProgress
    , onCancel:
        { action: Just OnCancel
        , label: "Cancel"
        }
    , onSkip: Nothing
    , onSubmit:
        { action: OnRestore <$> result
        , label: "Restore wallet"
        }
    , title: "Restore testnet wallet"
    }

nicknameInput
  :: forall m
   . MonadEffect m
  => AddressBook
  -> FieldState WalletNickname
  -> ComponentHTML m
nicknameInput addressBook fieldState =
  HH.slot
    _nickname
    unit
    Input.component
    (mkNicknameInput addressBook fieldState)
    OnNicknameMsg

mnemonicInput
  :: forall m. MonadEffect m => FieldState MnemonicPhrase -> ComponentHTML m
mnemonicInput fieldState =
  HH.slot _mnemonic unit Input.component input OnMnemonicMsg
  where
  id = "restore-wallet-mnemonic"
  label = "Mnemonic phrase"
  input =
    { fieldState
    , format: MP.toString
    , validate: MP.fromString
    , render: \{ error, value } ->
        renderTextInput id label error (Input.setInputProps value []) case _ of
          MP.Empty -> "Required."
          MP.WrongWordCount -> "24 words required."
          MP.ContainsInvalidWords -> "Mnemonic phrase contains invalid words."
    }
