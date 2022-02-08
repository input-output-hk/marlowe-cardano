module Component.ContractSetup (component, _contractSetup) where

import Prologue

import Component.Autocomplete as Autocomplete
import Component.ContractSetup.Types
  ( Component
  , ContractFields
  , ContractParams
  , Input
  , Msg(..)
  , _nickname
  , _roles
  , _timeouts
  , _values
  )
import Component.Form (renderNumberInput, renderTextInput)
import Component.Icons (Icon, icon)
import Component.Icons as Icon
import Css as Css
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.Bifunctor (lmap)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Compactable (compact)
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as CN
import Data.ContractTimeout (ContractTimeout)
import Data.ContractTimeout as CT
import Data.ContractValue (ContractValue)
import Data.ContractValue as CV
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (over, set)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Set as Set
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.FieldState as FS
import Halogen.Form.Injective (project)
import Halogen.Form.Input (FieldState)
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Marlowe.Extended.Metadata (NumberFormat(..))
import Marlowe.Semantics (TokenName)
import Store as Store
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

data Action
  = OnInit
  | OnReceive (Connected AddressBook Input)
  | OnNicknameMsg (Input.Msg Action ContractNickname)
  | OnRoleMsg TokenName (Autocomplete.Msg Address)
  | OnTimeoutMsg TokenName (Input.Msg Action ContractTimeout)
  | OnValueMsg TokenName (Input.Msg Action ContractValue)
  | OnFormSubmit Event
  | OnBack
  | OnReview ContractParams

type State =
  { addressBook :: Bimap String Address
  , templateValues :: Map String NumberFormat
  , templateName :: String
  , fields :: ContractFields
  , result :: Maybe ContractParams
  }

type ChildSlots =
  ( nickname :: Input.Slot Action ContractNickname Unit
  , roles :: Autocomplete.Slot Address TokenName
  , timeouts :: Input.Slot Action ContractTimeout String
  , values :: Input.Slot Action ContractValue String
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

_contractSetup :: Proxy "contractSetup"
_contractSetup = Proxy

component
  :: forall m
   . MonadAff m
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
initialState
  { context
  , input:
      { templateRoles
      , templateTimeouts
      , templateValues
      , templateName
      , fields
      }
  } =
  { addressBook: Bimap.fromFoldable $ map (lmap WN.toString) $
      ( Bimap.toUnfoldable
          $ unwrap context :: Array (Tuple WalletNickname Address)
      )
  , result: project fields
  , templateValues
  , templateName
  , fields: fields
      { roles = mapWithIndex mkRoleField $ Set.toMap templateRoles
      , timeouts = mapWithIndex mkTimeoutField templateTimeouts
      , values = mapWithIndex mkValueField templateValues
      }
  }
  where
  mkRoleField name _ = fromMaybe FS.Blank $ Map.lookup name fields.roles
  mkTimeoutField _ value = FS.Complete value
  mkValueField name _ = fromMaybe FS.Blank $ Map.lookup name fields.values

render
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => State
  -> ComponentHTML m
render state = do
  let
    { templateName
    , result
    , fields
    } = state
  HH.div [ classNames [ "h-full", "grid", "grid-rows-1fr-auto" ] ]
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
        [ HH.h2
            [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
            [ HH.text $ templateName <> " setup" ]
        , nicknameInput fields.nickname
        , templateInputsSection Icon.Roles "Roles"
            $ List.toUnfoldable
            $ Map.values
            $ mapWithIndex (roleInput state)
            $ fields.roles
        , templateInputsSection Icon.Terms "Terms" $
            ( List.toUnfoldable
                $ Map.values
                $ mapWithIndex timeoutInput fields.timeouts
            )
              <>
                ( List.toUnfoldable
                    $ Map.values
                    $ mapWithIndex (valueInput state) fields.values
                )
        ]
    , HH.div
        [ classNames
            [ "flex"
            , "items-baseline"
            , "p-4"
            , "border-gray"
            , "border-t"
            ]
        ]
        [ HH.a
            [ classNames [ "flex-1", "text-center" ]
            , onClick_ OnBack
            ]
            [ HH.text "Back" ]
        , HH.button
            ( compact
                [ pure $ classNames $
                    Css.primaryButton
                      <> [ "flex-1", "text-left" ]
                      <> Css.withIcon Icon.ArrowRight
                , onClick_ <<< OnReview <$> result
                , pure $ HP.enabled $ isJust result
                ]
            )
            [ HH.text "Review" ]
        ]
    ]

nicknameInput
  :: forall m. MonadEffect m => FieldState ContractNickname -> ComponentHTML m
nicknameInput fieldState =
  HH.slot _nickname unit Input.component input OnNicknameMsg
  where
  id = "contract-nickname"
  label = "Contract title"
  input =
    { fieldState
    , format: CN.toString
    , validate: CN.fromString
    , render: \{ error, value } ->
        renderTextInput id label error (Input.setInputProps value []) case _ of
          CN.Empty -> "Required."
    }

roleInput
  :: forall m
   . MonadAff m
  => MonadStore Store.Action Store.Store m
  => State
  -> TokenName
  -> FieldState Address
  -> ComponentHTML m
roleInput state name fieldState =
  HH.slot _roles name Autocomplete.component input $ OnRoleMsg name
  where
  { addressBook } = state
  input =
    { id: "role-" <> name
    , label: name
    , fieldState
    , options: addressBook
    }

timeoutInput
  :: forall m
   . MonadEffect m
  => String
  -> FieldState ContractTimeout
  -> ComponentHTML m
timeoutInput name fieldState =
  HH.slot _timeouts name Input.component input $ OnTimeoutMsg name
  where
  id = "timeout-" <> name
  input =
    { fieldState
    , format: CT.toString
    , validate: CT.fromString
    , render: \{ error, value } ->
        renderNumberInput
          TimeFormat
          id
          name
          error
          (Input.setInputProps value [])
          case _ of
            CT.Empty -> "Required."
            CT.Past -> "Must be in the future."
            CT.Invalid -> "Must be a number of slots from contract start."
    }

valueInput
  :: forall m
   . MonadEffect m
  => State
  -> String
  -> FieldState ContractValue
  -> ComponentHTML m
valueInput state name fieldState =
  HH.slot _values name Input.component input $ OnValueMsg name
  where
  { templateValues } = state
  format = fromMaybe DefaultFormat $ Map.lookup name templateValues
  id = "value-" <> name
  input =
    { fieldState
    , format: CV.toString
    , validate: case format of
        DecimalFormat d cs -> CV.currencyFromString cs d
        _ -> CV.fromString
    , render: \{ error, value } ->
        renderNumberInput format id name error (Input.setInputProps value [])
          case _ of
            CV.Empty -> "Required."
            CV.Invalid -> "Must by a number."
    }

templateInputsSection
  :: forall w i. Icon -> String -> Array (HH.HTML w i) -> HH.HTML w i
templateInputsSection icon' heading children =
  HH.fieldset [ classNames [ "space-y-2" ] ] $
    [ HH.h3
        [ classNames
            [ "flex"
            , "gap-1"
            , "items-center"
            , "leading-none"
            , "text-sm"
            , "font-semibold"
            , "pb-2"
            , "mb-4"
            , "border-gray"
            , "border-b"
            ]
        ]
        [ icon icon' [ "text-purple" ]
        , HH.text heading
        ]
    ] <> children

handleFieldMsg
  :: forall a m
   . MonadEffect m
  => Eq a
  => (FieldState a -> ContractFields -> ContractFields)
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

handleAction :: forall m. MonadEffect m => Action -> DSL m Unit
handleAction = case _ of
  OnInit -> do
    H.tell _nickname unit $ Input.Focus
  OnReceive input -> do
    oldState <- H.get
    let newState = initialState input
    when (oldState /= newState) $ H.put newState
  OnNicknameMsg msg -> handleFieldMsg (set (prop _nickname)) msg
  OnRoleMsg name (Autocomplete.Updated field) -> handleFieldMsg
    (over (prop _roles) <<< Map.insert name)
    (Input.Updated field)
  OnTimeoutMsg name msg -> handleFieldMsg
    (over (prop _timeouts) <<< Map.insert name)
    msg
  OnValueMsg name msg -> handleFieldMsg
    (over (prop _values) <<< Map.insert name)
    msg
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnBack -> H.raise BackClicked
  OnReview params -> H.raise $ ReviewClicked params
