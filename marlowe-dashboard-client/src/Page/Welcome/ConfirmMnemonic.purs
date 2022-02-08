module Page.Welcome.ConfirmMnemonic (component, _confirmMnemonic) where

import Prologue

import Capability.Marlowe (class ManageMarlowe, NewWalletDetails)
import Component.Form (renderTextInput)
import Data.AddressBook (AddressBook)
import Data.Bifunctor (lmap)
import Data.Lens (set)
import Data.Lens.Record (prop)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
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
import Page.Welcome.ConfirmMnemonic.Types
  ( Component
  , ConfirmMnemonicFields
  , ConfirmMnemonicParams
  , Input
  , Msg(..)
  , _mnemonic
  )
import Page.Welcome.Forms.Render (renderForm)
import Store as Store
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

data Action
  = OnInit
  | OnReceive (Connected AddressBook Input)
  | OnMnemonicMsg (Input.Msg Action MnemonicPhrase)
  | OnFormSubmit Event
  | OnCancel
  | OnCreate NewWalletDetails

type State =
  { addressBook :: AddressBook
  , fields :: ConfirmMnemonicFields
  , result :: Maybe ConfirmMnemonicParams
  , newWalletDetails :: NewWalletDetails
  }

type ChildSlots =
  ( mnemonic :: Input.Slot Action MnemonicPhrase Unit
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

_confirmMnemonic = Proxy :: Proxy "confirmMnemonic"

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
initialState { context, input: { fields, newWalletDetails } } =
  { addressBook: context
  , fields
  , result: project fields
  , newWalletDetails
  }

handleFieldMsg
  :: forall a m
   . MonadEffect m
  => ManageMarlowe m
  => Eq a
  => (FieldState a -> ConfirmMnemonicFields -> ConfirmMnemonicFields)
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
    H.tell _mnemonic unit $ Input.Focus
  OnReceive input -> do
    oldState <- H.get
    let newState = initialState input
    when (oldState /= newState) $ H.put newState
  OnMnemonicMsg msg -> handleFieldMsg (set (prop _mnemonic)) msg
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnCancel -> H.raise <<< BackClicked =<< H.gets _.newWalletDetails
  OnCreate newWalletDetails -> H.raise $ MnemonicConfirmed newWalletDetails

render
  :: forall m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML m
render { result, fields, newWalletDetails } = do
  renderForm
    { body:
        [ HH.form
            [ HE.onSubmit OnFormSubmit
            , classNames [ "relative", "space-y-4" ]
            ]
            [ mnemonicInput newWalletDetails.mnemonic fields.mnemonic
            ]
        , HH.p
            [ classNames [ "pb-4" ] ]
            [ HH.text
                "You can confirm your new wallet mnemonic now."
            ]
        ]
    , inProgress: false
    , onCancel:
        { action: Just OnCancel
        , label: "Cancel"
        }
    , onSkip: Nothing
    , onSubmit:
        { action: OnCreate newWalletDetails <$ result
        , label: "Ok"
        }
    , title: "Confirm mnemonic"
    }

mnemonicInput
  :: forall m
   . MonadEffect m
  => MnemonicPhrase
  -> FieldState MnemonicPhrase
  -> ComponentHTML m
mnemonicInput mnemonic fieldState =
  HH.slot _mnemonic unit Input.component input OnMnemonicMsg
  where
  id = "restore-wallet-mnemonic"
  label = "Mnemonic phrase"
  input =
    { fieldState
    , format: MP.toString
    , validate: matches <=< lmap Right <<< MP.fromString
    , render: \{ error, value } ->
        renderTextInput id label error (Input.setInputProps value []) case _ of
          (Right MP.Empty) -> "Required."
          (Right MP.WrongWordCount) -> "24 words required."
          (Right MP.ContainsInvalidWords) ->
            "Mnemonic phrase contains invalid words."
          (Left _) -> "Given mnemonic differs from your new wallet mnemonic."
    }
  matches m
    | m == mnemonic = Left $ Left unit
    | otherwise = Right m
