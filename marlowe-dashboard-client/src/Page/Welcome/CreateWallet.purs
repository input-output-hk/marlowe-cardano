module Page.Welcome.CreateWallet (component, _createWallet) where

import Prologue

import AppM (passphrase) as AppM
import Capability.Toast (class Toast, addToast)
import Capability.Wallet (class ManageWallet, createWallet)
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.AddressBook (AddressBook)
import Data.Lens (is, (^?))
import Data.Maybe (fromMaybe)
import Data.Wallet (mkWalletDetails)
import Data.WalletNickname (WalletNickname)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.Injective (blank, project)
import Halogen.Form.Input (FieldState)
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types (CreateResponse(..))
import Network.RemoteData (RemoteData(..), _Failure, _Loading)
import Page.Welcome.CreateWallet.Types
  ( Component
  , Input
  , Msg(..)
  , NewWalletDetails
  , _nickname
  )
import Page.Welcome.Forms.Render (mkNicknameInput, renderForm)
import Store as Store
import Toast.Types (ajaxErrorToast)
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

data Action
  = OnInit
  | OnReceive (Connected AddressBook Input)
  | OnNicknameMsg (Input.Msg Action WalletNickname)
  | OnFormSubmit Event
  | OnCancel
  | OnCreate WalletNickname

type State =
  { addressBook :: AddressBook
  , fieldState :: FieldState WalletNickname
  , newWalletDetails :: RemoteData String NewWalletDetails
  }

type ChildSlots =
  ( nickname :: Input.Slot Action WalletNickname Unit
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

_createWallet = Proxy :: Proxy "createWallet"

component
  :: forall m
   . MonadAff m
  => ManageWallet m
  => Toast m
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
initialState { context } =
  { addressBook: context
  , fieldState: blank
  , newWalletDetails: NotAsked
  }

handleAction
  :: forall m
   . MonadEffect m
  => Toast m
  => ManageWallet m
  => Action
  -> DSL m Unit
handleAction = case _ of
  OnInit -> do
    H.tell _nickname unit $ Input.Focus
  OnReceive input -> H.modify_ _ { addressBook = input.context }
  OnNicknameMsg msg -> case msg of
    Input.Updated fieldState -> H.modify_ _ { fieldState = fieldState }
    Input.Blurred -> pure unit
    Input.Focused -> pure unit
    Input.Emit action -> handleAction action
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnCancel -> H.raise CancelClicked
  OnCreate nickname -> do
    H.modify_ _ { newWalletDetails = Loading }
    response <- lift $ createWallet nickname AppM.passphrase
    case response of
      Left err -> do
        addToast $ ajaxErrorToast "Failed to create wallet" err
        H.modify_ _
          { newWalletDetails = Failure "Failed to create wallet"
          }
      Right (CreateResponse { walletInfo, mnemonic }) -> do
        let
          newWalletDetails =
            { walletDetails: mkWalletDetails nickname walletInfo, mnemonic }
        H.modify_ _ { newWalletDetails = Success newWalletDetails }
        H.raise $ WalletCreated newWalletDetails

render
  :: forall m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML m
render state = do
  let
    { addressBook
    , fieldState
    , newWalletDetails
    } = state
  let serverError = fromMaybe "" $ newWalletDetails ^? _Failure
  let inProgress = is _Loading newWalletDetails
  renderForm
    { body:
        [ HH.form
            [ HE.onSubmit OnFormSubmit
            , classNames [ "relative", "space-y-4" ]
            ]
            [ nicknameInput addressBook fieldState
            ]
        -- TODO replace with progress buttons when refactored.
        , HH.p [ classNames $ Css.inputError false ] [ HH.text serverError ]
        ]
    , inProgress
    , onCancel:
        { action: Just OnCancel
        , label: "Cancel"
        }
    , onSkip: Nothing
    , onSubmit:
        { action: OnCreate <$> project fieldState
        , label: "Create wallet"
        }
    , title: "Create testnet wallet"
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
    (mkNicknameInput false addressBook fieldState)
    OnNicknameMsg
