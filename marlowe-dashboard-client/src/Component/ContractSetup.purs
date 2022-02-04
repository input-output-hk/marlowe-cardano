module Component.ContractSetup where

import Prologue

import Component.ContractSetup.Types
  ( Component
  , ContractFields
  , ContractParams
  , InitializeContractFields
  , Input
  , Msg(..)
  , Query(..)
  , _nickname
  , _roles
  , _timeouts
  , _values
  )
import Component.Icons (Icon, icon)
import Component.Icons as Icon
import Css as Css
import DOM.HTML.Indexed (HTMLinput)
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Compactable (compact)
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as CN
import Data.ContractTimeout (ContractTimeout)
import Data.ContractTimeout as CT
import Data.ContractValue (ContractValue)
import Data.ContractValue as CV
import Data.Either (note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (over, set)
import Data.Lens.Record (prop)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.WalletNickname as WN
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Css (classNames)
import Halogen.Form.Input (FieldState)
import Halogen.Form.Input as Input
import Halogen.Form.Projective (blank, project)
import Halogen.Form.Types (InitializeField(..))
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
  = OnNicknameMsg (Input.Msg Action ContractNickname)
  | OnRoleMsg TokenName (Input.Msg Action Address)
  | OnTimeoutMsg TokenName (Input.Msg Action ContractTimeout)
  | OnValueMsg TokenName (Input.Msg Action ContractValue)
  | OnFormSubmit Event
  | OnBack
  | OnReview ContractParams

type State =
  { addressBook :: AddressBook
  , templateRoles :: Set TokenName
  , templateTimeouts :: Map String ContractTimeout
  , templateValues :: Map String NumberFormat
  , templateName :: String
  , fields :: ContractFields
  , initialize :: InitializeContractFields
  , result :: Maybe ContractParams
  }

_input = Proxy :: Proxy "input"

_fields = Proxy :: Proxy "fields"

_addressBook = Proxy :: Proxy "addressBook"

_result = Proxy :: Proxy "result"

type ChildSlots =
  ( nickname :: Input.Slot Action ContractNickname Unit
  , roles :: Input.Slot Action Address TokenName
  , timeouts :: Input.Slot Action ContractTimeout String
  , values :: Input.Slot Action ContractValue String
  )

type ComponentHTML m =
  H.ComponentHTML Action ChildSlots m

type DSL m a =
  H.HalogenM State Action ChildSlots Msg m a

renderLabel :: forall w i. String -> String -> HH.HTML w i
renderLabel id label =
  HH.label [ classNames $ Css.labelBox <> Css.labelText, HP.for id ]
    [ HH.text label ]

renderErrorLabel :: forall w i. String -> Maybe String -> HH.HTML w i
renderErrorLabel id error = HH.label
  [ classNames $ Css.inputError <> maybe [ "invisible" ] (const []) error
  , HP.for id
  ]
  [ HH.text $ fromMaybe "Valid" error ]

renderInputBox
  :: forall w error i. Maybe error -> Array (HH.HTML w i) -> HH.HTML w i
renderInputBox error =
  HH.div [ classNames $ Css.inputBox error ]

renderInput
  :: forall pa slots error output m
   . String
  -> String
  -> Array (HP.IProp HTMLinput (Input.Action pa slots error output m))
  -> Input.ComponentHTML pa slots error output m
renderInput id value props = HH.input
  ( Input.setInputProps value $ props <> [ HP.id id, classNames Css.inputText ]
  )

_contractSetup :: Proxy "contractSetup"
_contractSetup = Proxy

component
  :: forall m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => Component m
component = connect (selectEq _.addressBook) $ H.mkComponent
  { initialState
  , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
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
      , initialize
      }
  } =
  { addressBook: context
  , result: project initialize
  , templateRoles
  , templateTimeouts
  , templateValues
  , templateName
  , initialize
  , fields: blank
  }

render :: forall m. MonadEffect m => State -> ComponentHTML m
render state = do
  let
    { templateName
    , templateRoles
    , templateTimeouts
    , templateValues
    , result
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
        , nicknameInput state
        , templateInputsSection Icon.Roles "Roles"
        , HH.fieldset [ classNames [ "space-y-2" ] ]
            $ map (roleInput state)
            $ Set.toUnfoldable templateRoles
        , templateInputsSection Icon.Terms "Terms"
        , HH.fieldset [ classNames [ "space-y-2" ] ] $
            ( List.toUnfoldable
                $ Map.values
                $ mapWithIndex timeoutInput templateTimeouts
            )
              <>
                ( List.toUnfoldable
                    $ Map.values
                    $ mapWithIndex (valueInput state) templateValues
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

nicknameInput :: forall m. MonadEffect m => State -> ComponentHTML m
nicknameInput state =
  HH.slot _nickname unit Input.component input OnNicknameMsg
  where
  { initialize: { nickname } } = state
  id = "contract-nickname"
  input =
    { initialize: nickname
    , format: CN.toString
    , validate: CN.fromString
    , render: \s ->
        let
          mkError = case _ of
            CN.Empty -> "Required."
          error = mkError <$> s.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id "Contract title"
            , renderInputBox error [ renderInput id s.value [] ]
            , renderErrorLabel id error
            ]
    }

roleInput
  :: forall m. MonadEffect m => State -> TokenName -> ComponentHTML m
roleInput state name =
  HH.slot _roles name Input.component input $ OnRoleMsg name
  where
  { initialize: { roles }, addressBook } = state
  id = "role-" <> name
  input =
    { initialize: fromMaybe FromBlank $ Map.lookup name roles
    , format: \addr ->
        maybe "" WN.toString $ AddressBook.lookupNickname addr state.addressBook
    , validate:
        note WN.DoesNotExist
          <<< flip AddressBook.lookupAddress addressBook
          <=< WN.fromString
    , render: \s ->
        let
          mkError = case _ of
            WN.Empty -> "Required."
            WN.Exists -> "Already exists."
            WN.DoesNotExist -> "Not found."
            WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."
          error = mkError <$> s.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id name
            , renderInputBox error
                [ renderInput id s.value [] ]
            , renderErrorLabel id error
            ]
    }

timeoutInput
  :: forall m. MonadEffect m => String -> ContractTimeout -> ComponentHTML m
timeoutInput name timeout =
  HH.slot _timeouts name Input.component input $ OnTimeoutMsg name
  where
  id = "timeout-" <> name
  input =
    { initialize: FromOutput timeout
    , format: CT.toString
    , validate: CT.fromString
    , render: \s ->
        let
          mkError = case _ of
            CT.Empty -> "Required."
            CT.Past -> "Must be in the future."
            CT.Invalid -> "Must be a number of slots from contract start."
          error = mkError <$> s.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id name
            , renderInputBox error
                [ renderInput id s.value [ HP.type_ HP.InputNumber ]
                , HH.span_ [ HH.text "minutes" ]
                ]
            , renderErrorLabel id error
            ]
    }

valueInput
  :: forall m
   . MonadEffect m
  => State
  -> String
  -> NumberFormat
  -> ComponentHTML m
valueInput state name format =
  HH.slot _values name Input.component input $ OnValueMsg name
  where
  { initialize: { values } } = state
  id = "value-" <> name
  input =
    { initialize: fromMaybe FromBlank $ Map.lookup name values
    , format: CV.toString format
    , validate: CV.fromString format
    , render: \s ->
        let
          mkError = case _ of
            CV.Empty -> "Required."
            CV.Negative -> "Must by positive."
            CV.Invalid -> "Must by a number."
          error = mkError <$> s.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id name
            , renderInputBox error $ join
                [ case format of
                    DecimalFormat _ symbol -> [ HH.span_ [ HH.text symbol ] ]
                    _ -> []
                , [ renderInput id s.value [ HP.type_ HP.InputNumber ] ]
                ]
            , renderErrorLabel id error
            ]
    }

templateInputsSection
  :: forall w i. Icon -> String -> HH.HTML w i
templateInputsSection icon' heading = HH.h3
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
  OnNicknameMsg msg -> handleFieldMsg (set (prop _nickname)) msg
  OnRoleMsg name msg -> handleFieldMsg
    (over (prop _roles) <<< Map.insert name)
    msg
  OnTimeoutMsg name msg -> handleFieldMsg
    (over (prop _timeouts) <<< Map.insert name)
    msg
  OnValueMsg name msg -> handleFieldMsg
    (over (prop _values) <<< Map.insert name)
    msg
  OnFormSubmit event -> H.liftEffect $ preventDefault event
  OnBack -> H.raise BackClicked
  OnReview params -> H.raise $ ReviewClicked params

handleQuery :: forall m a. MonadEffect m => Query a -> DSL m (Maybe a)
handleQuery = case _ of
  InitializeNickname init a -> do
    H.tell _nickname unit $ Input.Initialize init
    pure $ Just a
  InitializeRole name init a -> do
    H.tell _roles name $ Input.Initialize init
    pure $ Just a
  InitializeTimeout name init a -> do
    H.tell _timeouts name $ Input.Initialize init
    pure $ Just a
  InitializeValue name init a -> do
    H.tell _values name $ Input.Initialize init
    pure $ Just a
