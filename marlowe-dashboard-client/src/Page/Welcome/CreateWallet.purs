module Page.Welcome.CreateWallet (component, _createWallet) where

import Prologue

import AppM (passphrase) as AppM
import Capability.Marlowe (class ManageMarlowe, NewWalletDetails, createWallet)
import Control.Monad.Trans.Class (lift)
import Css as Css
import Data.AddressBook (AddressBook)
import Data.Lens (is, set, (^?))
import Data.Lens.Record (prop)
import Data.Maybe (fromMaybe)
import Data.Variant (match) as Variant
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
import Page.Welcome.CreateWallet.Types
  ( Component
  , CreateWalletFields
  , CreateWalletParams
  , Input
  , Msg(..)
  , _nickname
  )
import Page.Welcome.Forms.Render (mkNicknameInput, renderForm)
import Store as Store
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
  , fields :: CreateWalletFields
  , result :: Maybe CreateWalletParams
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
  , newWalletDetails: NotAsked
  }

handleFieldMsg
  :: forall a m
   . MonadEffect m
  => ManageMarlowe m
  => Eq a
  => (FieldState a -> CreateWalletFields -> CreateWalletFields)
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
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnCancel -> H.raise CancelClicked
  OnCreate nickname -> do
    H.modify_ _ { newWalletDetails = Loading }
    response <- lift $ createWallet nickname AppM.passphrase
    case response of
      Left err -> do
        H.modify_ _
          { newWalletDetails = err
              #
                ( Variant.match
                    { serverError: const
                        "We have encountered some serious problem. Please try again later."
                    , clientServerError: const
                        "Unable to connect to server. Please check your internet connection."
                    }
                )
              # Failure
          }
      Right newWalletDetails -> do
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
    , result
    , fields
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
            [ nicknameInput addressBook fields.nickname
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
        { action: OnCreate <<< _.nickname <$> result
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
    (mkNicknameInput addressBook fieldState)
    OnNicknameMsg
