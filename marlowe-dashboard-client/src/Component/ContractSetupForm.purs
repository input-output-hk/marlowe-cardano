module Component.ContractSetupForm where

import Prologue

import Component.Form as HF
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
import Data.Either (either, note)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens (_Just, (^?))
import Data.Lens.AffineTraversal (AffineTraversal')
import Data.Lens.Index (ix)
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
import Halogen.Form.Input as Input
import Halogen.HTML as HH
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Extra (mapComponent)
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Marlowe.Extended.Metadata (NumberFormat(..))
import Marlowe.Semantics (TokenName)
import Store as Store
import Type.Proxy (Proxy(..))

type ContractParams =
  { nickname :: ContractNickname
  , roles :: Map TokenName Address
  , timeouts :: Map String ContractTimeout
  , values :: Map String ContractValue
  }

type Input =
  { roles :: Set TokenName
  , timeouts :: Map String ContractTimeout
  , values :: Map String NumberFormat
  , contractName :: String
  , params :: Maybe ContractParams
  }

data Msg
  = Review ContractParams
  | Back

type Slot = HF.Slot Msg Unit

type ChildSlots pa =
  ( nickname :: Input.Slot pa ContractNickname Unit
  , roles :: Input.Slot pa Address TokenName
  , timeouts :: Input.Slot pa ContractTimeout String
  , values :: Input.Slot pa ContractValue String
  )

type ComponentHTML pa output m =
  HF.ComponentHTML pa output (ChildSlots pa) m

_nickname :: Proxy "nickname"
_nickname = Proxy

_roles :: Proxy "roles"
_roles = Proxy

_timeouts :: Proxy "timeouts"
_timeouts = Proxy

_values :: Proxy "values"
_values = Proxy

timeoutAt :: String -> AffineTraversal' ContractParams ContractTimeout
timeoutAt name = prop _timeouts <<< ix name

valueAt :: String -> AffineTraversal' ContractParams ContractValue
valueAt name = prop _values <<< ix name

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

contractNicknameForm
  :: forall pa m
   . MonadEffect m
  => Maybe ContractParams
  -> ComponentHTML pa ContractNickname m
contractNicknameForm params =
  Input.renderInSlot _nickname unit
    { options:
        { load: _.nickname <$> params
        , format: CN.toString
        , validate: CN.fromString
        }
    , handlers: Input.defaultHandlers
    , render: \state ->
        let
          id = "contract-nickname"
          mkError = case _ of
            CN.Empty -> "Required."
          error = mkError <$> state.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id "Contract title"
            , renderInputBox error [ renderInput id state.value [] ]
            , renderErrorLabel id error
            ]
    }

roleAssignmentForm
  :: forall pa m
   . MonadEffect m
  => AddressBook
  -> Maybe ContractParams
  -> TokenName
  -> ComponentHTML pa (Map TokenName Address) m
roleAssignmentForm addressBook params name =
  HF.zoom (ix name) $ Input.renderInSlot _roles name
    { options:
        { load: params ^? _Just <<< prop _roles <<< ix name
        , format: \addr ->
            maybe "" WN.toString $ AddressBook.lookupNickname addr addressBook
        , validate:
            note WN.DoesNotExist
              <<< flip AddressBook.lookupAddress addressBook
              <=< WN.fromString
        }
    , handlers: Input.defaultHandlers
    , render: \state ->
        let
          id = "role-" <> name
          mkError = case _ of
            WN.Empty -> "Required."
            WN.Exists -> "Already exists."
            WN.DoesNotExist -> "Not found."
            WN.ContainsNonAlphaNumeric -> "Can only contain letters and digits."
          error = mkError <$> state.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id name
            , renderInputBox error
                [ renderInput id state.value [] ]
            , renderErrorLabel id error
            ]
    }

timeoutForm
  :: forall pa m
   . MonadEffect m
  => String
  -> ContractTimeout
  -> ComponentHTML pa (Map String ContractTimeout) m
timeoutForm name timeout =
  HF.zoom (ix name) $ Input.renderInSlot _timeouts name
    { options:
        { load: Just timeout
        , format: CT.toString
        , validate: CT.fromString
        }
    , handlers: Input.defaultHandlers
    , render: \state ->
        let
          id = "timeout-" <> name
          mkError = case _ of
            CT.Empty -> "Required."
            CT.Past -> "Must be in the future."
            CT.Invalid -> "Must be a number of slots from contract start."
          error = mkError <$> state.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id name
            , renderInputBox error
                [ renderInput id state.value [ HP.type_ HP.InputNumber ]
                , HH.span_ [ HH.text "minutes" ]
                ]
            , renderErrorLabel id error
            ]
    }

valueForm
  :: forall pa m
   . MonadEffect m
  => Maybe ContractParams
  -> String
  -> NumberFormat
  -> ComponentHTML pa (Map String ContractValue) m
valueForm params name format =
  HF.zoom (ix name) $ Input.renderInSlot _values name
    { options:
        { load: params ^? _Just <<< valueAt name
        , format: CV.toString format
        , validate: CV.fromString format
        }
    , handlers: Input.defaultHandlers
    , render: \state ->
        let
          id = "value-" <> name
          mkError = case _ of
            CV.Empty -> "Required."
            CV.Negative -> "Must by positive."
            CV.Invalid -> "Must by a number."
          error = mkError <$> state.error
        in
          HH.div [ classNames [ "relative" ] ]
            [ renderLabel id name
            , renderInputBox error $ join
                [ case format of
                    DecimalFormat _ symbol -> [ HH.span_ [ HH.text symbol ] ]
                    _ -> []
                , [ renderInput id state.value [ HP.type_ HP.InputNumber ] ]
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

_contractSetup :: Proxy "contractSetup"
_contractSetup = Proxy

component
  :: forall m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => H.Component HF.Query Input Msg m

component = connect (selectEq _.addressBook) $ H.mkComponent
  { initialState: identity
  , eval: H.mkEval $ H.defaultEval
      { handleAction = either H.put H.raise
      , receive = Just <<< Left
      }
  , render: \({ context: addressBook, input }) -> do
      let { contractName, params, roles, timeouts, values } = input
      mapComponent Right $
        HF.renderInSlot _contractSetup unit params \result ->
          HH.div [ classNames [ "h-full", "grid", "grid-rows-1fr-auto" ] ]
            [ HH.form
                ( HF.setFormProps
                    [ classNames
                        [ "overflow-y-auto"
                        , "p-4"
                        , "flex"
                        , "flex-col"
                        , "gap-2"
                        ]
                    ]
                )
                [ HH.h2
                    [ classNames [ "text-lg", "font-semibold", "mb-2" ] ]
                    [ HH.text $ contractName <> " setup" ]
                , HF.zoom (prop _nickname) $ contractNicknameForm result
                , templateInputsSection Icon.Roles "Roles"
                , HF.zoom (prop _roles)
                    $ HH.fieldset [ classNames [ "space-y-2" ] ]
                    $ map (roleAssignmentForm addressBook result)
                    $ Set.toUnfoldable roles
                , templateInputsSection Icon.Terms "Terms"
                , HH.fieldset [ classNames [ "space-y-2" ] ] $
                    ( map (HF.zoom (prop _timeouts))
                        $ List.toUnfoldable
                        $ Map.values
                        $ mapWithIndex timeoutForm timeouts
                    )
                      <>
                        ( map (HF.zoom (prop _values))
                            $ List.toUnfoldable
                            $ Map.values
                            $ mapWithIndex (valueForm result) values
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
                    , onClick_ $ HF.emit Back
                    ]
                    [ HH.text "Back" ]
                , HH.button
                    ( compact
                        [ pure $ classNames $
                            Css.primaryButton
                              <> [ "flex-1", "text-left" ]
                              <> Css.withIcon Icon.ArrowRight
                        , onClick_ <<< HF.emit <<< Review <$> result
                        , pure $ HP.enabled $ isJust result
                        ]
                    )
                    [ HH.text "Review" ]
                ]
            ]
  }

render
  :: forall slots m
   . MonadEffect m
  => MonadStore Store.Action Store.Store m
  => Input
  -> HH.ComponentHTML Msg (contractSetup :: Slot | slots) m
render input = HH.slot _contractSetup unit component input identity
