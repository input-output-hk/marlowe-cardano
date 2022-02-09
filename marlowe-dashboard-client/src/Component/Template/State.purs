module Component.Template.State
  ( dummyState
  , initialState
  , handleAction
  , instantiateExtendedContract
  , templateSetupIsValid
  ) where

import Prologue

import Component.InputField.Lenses (_value)
import Component.InputField.State (formatBigIntValue, getBigIntValue, validate)
import Component.InputField.State (handleAction, mkInitialState) as InputField
import Component.InputField.Types (class InputFieldError)
import Component.InputField.Types (Action(..), State) as InputField
import Component.Template.Lenses
  ( _contractNicknameInput
  , _contractSetupStage
  , _contractTemplate
  , _roleWalletInput
  , _roleWalletInputs
  , _slotContentInput
  , _slotContentInputs
  , _valueContentInput
  , _valueContentInputs
  )
import Component.Template.Types
  ( Action(..)
  , ContractNicknameError(..)
  , ContractSetupStage(..)
  , Input
  , RoleError(..)
  , SlotError(..)
  , State
  , ValueError(..)
  )
import Data.AddressBook (AddressBook)
import Data.AddressBook as AB
import Data.Array (mapMaybe) as Array
import Data.BigInt.Argonaut (BigInt)
import Data.Either (hush)
import Data.Foldable (for_)
import Data.Lens (Lens', assign, lens, preview, set, use, view)
import Data.Lens.Extra (peruse)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, isNothing)
import Data.Set (toUnfoldable) as Set
import Data.Traversable (for, traverse)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Examples.PureScript.ContractForDifferences (defaultSlotContent) as ContractForDifferences
import Examples.PureScript.Escrow (contractTemplate, defaultSlotContent) as Escrow
import Examples.PureScript.EscrowWithCollateral (defaultSlotContent) as EscrowWithCollateral
import Examples.PureScript.Swap (defaultSlotContent) as Swap
import Examples.PureScript.ZeroCouponBond (defaultSlotContent) as ZeroCouponBond
import Halogen (HalogenM, RefLabel(..), getHTMLElementRef, modify_)
import Halogen.Extra (imapState, mapSubmodule)
import Halogen.Query.HalogenM (mapAction)
import MainFrame.Types (ChildSlots, Msg)
import Marlowe.Extended (Contract) as Extended
import Marlowe.Extended (ContractType(..), resolveRelativeTimes, toCore)
import Marlowe.Extended.Metadata
  ( MetaData
  , NumberFormat(..)
  , _extendedContract
  , _metaData
  , _valueParameterFormat
  , _valueParameterInfo
  )
import Marlowe.HasParties (getParties)
import Marlowe.Semantics (Contract) as Semantic
import Marlowe.Semantics (Party(..), Slot, TokenName)
import Marlowe.Template
  ( TemplateContent(..)
  , _slotContent
  , _valueContent
  , fillTemplate
  , getPlaceholderIds
  , initializeTemplateContent
  )
import Web.HTML.HTMLElement (focus)

-- see note [dummyState] in MainFrame.State
dummyState :: State
dummyState = initialState

initialState :: State
initialState =
  { contractSetupStage: Start
  , contractTemplate: Escrow.contractTemplate
  , contractNicknameInput: InputField.mkInitialState Nothing
  , roleWalletInputs: Map.empty
  , slotContentInputs: Map.empty
  , valueContentInputs: Map.empty
  }

-- Some actions are handled in `Dashboard.State` because they involve
-- modifications of that state. See Note [State] in MainFrame.State.
handleAction
  :: forall m
   . MonadAff m
  => Input
  -> Action
  -> HalogenM State Action ChildSlots Msg m Unit
handleAction _ (SetContractSetupStage contractSetupStage) = do
  assign _contractSetupStage contractSetupStage
  when (contractSetupStage == Setup) do
    element <- getHTMLElementRef $ RefLabel "contractNickname"
    liftEffect $ void $ traverse focus element

handleAction input (SetTemplate contractTemplate) = do
  let
    templateContent = initializeTemplateContent $ getPlaceholderIds
      contractTemplate.extendedContract

    slotContent = view _slotContent templateContent

    valueContent = view _valueContent templateContent

    roleWalletInputs = mkRoleWalletInputs contractTemplate.extendedContract

    slotContentInputs = mkSlotContentInputs contractTemplate.metaData
      slotContent

    valueContentInputs = mkValueContentInputs contractTemplate.metaData
      valueContent
  modify_
    $ set _contractSetupStage Overview
        <<< set _contractTemplate contractTemplate
        <<< set _roleWalletInputs roleWalletInputs
        <<< set _slotContentInputs slotContentInputs
        <<< set _valueContentInputs valueContentInputs
  handleAction input $ ContractNicknameInputAction $ InputField.Reset
  handleAction input $ ContractNicknameInputAction $ InputField.SetValidator
    contractNicknameError
  handleAction input UpdateRoleWalletValidators
  setInputValidators input _valueContentInputs ValueContentInputAction
    valueError
  setInputValidators input _slotContentInputs SlotContentInputAction slotError

handleAction _ (OpenCreateWalletCard _) = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

handleAction _ (ContractNicknameInputAction inputFieldAction) =
  toContractNicknameInput $ InputField.handleAction inputFieldAction

handleAction input@{ addressBook } UpdateRoleWalletValidators =
  setInputValidators input _roleWalletInputs RoleWalletInputAction
    $ roleError addressBook <<< hush <<< WN.fromString

handleAction _ (RoleWalletInputAction tokenName inputFieldAction) = do
  mInput <- peruse $ _roleWalletInput tokenName
  for_ mInput \s ->
    toRoleWalletInput tokenName s $ InputField.handleAction inputFieldAction

handleAction _ (SlotContentInputAction key inputFieldAction) = do
  mInput <- peruse $ _slotContentInput key
  for_ mInput \s ->
    toSlotContentInput key s $ InputField.handleAction inputFieldAction

handleAction _ (ValueContentInputAction key inputFieldAction) = do
  mInput <- peruse $ _valueContentInput key
  for_ mInput \s ->
    toValueContentInput key s $ InputField.handleAction inputFieldAction

handleAction _ StartContract = pure unit -- handled in Dashboard.State (see note [State] in MainFrame.State)

setInputValidators
  :: forall e m
   . MonadAff m
  => InputFieldError e
  => Input
  -> Lens' State (Map String (InputField.State e))
  -> (String -> (InputField.Action e -> Action))
  -> (String -> Maybe e)
  -> HalogenM State Action ChildSlots Msg m Unit
setInputValidators input lens action validator = do
  inputFields <- use lens
  let
    (inputFieldKeys :: Array String) = Set.toUnfoldable $ Map.keys inputFields
  void
    $ for inputFieldKeys \key ->
        handleAction input $ action key $ InputField.SetValidator validator

------------------------------------------------------------
mkRoleWalletInputs
  :: Extended.Contract -> Map TokenName (InputField.State RoleError)
mkRoleWalletInputs contract = Map.fromFoldable $ Array.mapMaybe getRoleInput
  (Set.toUnfoldable $ getParties contract)
  where
  getRoleInput :: Party -> Maybe (Tuple TokenName (InputField.State RoleError))
  getRoleInput (PK _) = Nothing

  getRoleInput (Role tokenName) = Just
    (Tuple tokenName $ InputField.mkInitialState Nothing)

mkSlotContentInputs
  :: MetaData -> Map String BigInt -> Map String (InputField.State SlotError)
mkSlotContentInputs metaData slotContent =
  let
    defaultSlotContent = case metaData.contractType of
      Escrow -> Escrow.defaultSlotContent
      EscrowWithCollateral -> EscrowWithCollateral.defaultSlotContent
      Swap -> Swap.defaultSlotContent
      ZeroCouponBond -> ZeroCouponBond.defaultSlotContent
      ContractForDifferences -> ContractForDifferences.defaultSlotContent
      _ -> Map.empty

    mkSlotContentInput key _ =
      let
        inputFieldInitialState = InputField.mkInitialState $ Just DefaultFormat
      in
        case Map.lookup key defaultSlotContent of
          Just value -> Just $ set _value (formatBigIntValue TimeFormat value)
            inputFieldInitialState
          Nothing -> Just inputFieldInitialState
  in
    Map.mapMaybeWithKey mkSlotContentInput slotContent

mkValueContentInputs
  :: MetaData -> Map String BigInt -> Map String (InputField.State ValueError)
mkValueContentInputs metaData valueContent = Map.mapMaybeWithKey valueToInput
  valueContent
  where
  valueToInput key _ =
    case
      OMap.lookup key $ map (view _valueParameterFormat)
        (view _valueParameterInfo metaData)
      of
      Just numberFormat -> Just $ InputField.mkInitialState $ Just numberFormat
      _ -> Just $ InputField.mkInitialState Nothing

instantiateExtendedContract :: Slot -> State -> Maybe Semantic.Contract
instantiateExtendedContract currentSlot state =
  let
    extendedContract = view (_contractTemplate <<< _extendedContract) state

    slotContentInputs = view _slotContentInputs state

    valueContentInputs = view _valueContentInputs state

    slotContent = map (getBigIntValue TimeFormat <<< view _value)
      slotContentInputs

    valueParameterFormats = map (view _valueParameterFormat)
      (view (_contractTemplate <<< _metaData <<< _valueParameterInfo) state)

    getBigIntValueWithDecimals key valueContentInput =
      case OMap.lookup key valueParameterFormats of
        Just numberFormat -> Just $ getBigIntValue numberFormat $ view _value
          valueContentInput
        _ -> Just $ getBigIntValue DefaultFormat $ view _value valueContentInput

    valueContent = Map.mapMaybeWithKey getBigIntValueWithDecimals
      valueContentInputs

    templateContent = TemplateContent { slotContent, valueContent }

    filledContract = fillTemplate templateContent extendedContract

    absoluteFilledContract = resolveRelativeTimes currentSlot filledContract
  in
    toCore absoluteFilledContract

------------------------------------------------------------
toContractNicknameInput
  :: forall m msg slots
   . Functor m
  => HalogenM (InputField.State ContractNicknameError)
       (InputField.Action ContractNicknameError)
       slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toContractNicknameInput = mapSubmodule _contractNicknameInput
  ContractNicknameInputAction

toRoleWalletInput
  :: forall m msg slots
   . Functor m
  => TokenName
  -> InputField.State RoleError
  -> HalogenM (InputField.State RoleError) (InputField.Action RoleError) slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toRoleWalletInput tokenName s =
  mapAction (RoleWalletInputAction tokenName) <<< imapState (lens getter setter)
  where
  trav = _roleWalletInput tokenName
  getter = fromMaybe s <<< preview trav
  setter = flip $ set trav

toSlotContentInput
  :: forall m msg slots
   . Functor m
  => String
  -> InputField.State SlotError
  -> HalogenM (InputField.State SlotError) (InputField.Action SlotError) slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toSlotContentInput key s =
  mapAction (SlotContentInputAction key) <<< imapState (lens getter setter)
  where
  trav = _slotContentInput key
  getter = fromMaybe s <<< preview trav
  setter = flip $ set trav

toValueContentInput
  :: forall m msg slots
   . Functor m
  => String
  -> InputField.State ValueError
  -> HalogenM (InputField.State ValueError) (InputField.Action ValueError)
       slots
       msg
       m
       Unit
  -> HalogenM State Action slots msg m Unit
toValueContentInput key s =
  mapAction (ValueContentInputAction key) <<< imapState (lens getter setter)
  where
  trav = _valueContentInput key
  getter = fromMaybe s <<< preview trav
  setter = flip $ set trav

------------------------------------------------------------
contractNicknameError :: String -> Maybe ContractNicknameError
contractNicknameError "" = Just EmptyContractNickname

contractNicknameError _ = Nothing

roleError :: AddressBook -> Maybe WalletNickname -> Maybe RoleError
roleError _ Nothing = Just EmptyNickname

roleError addressBook (Just walletNickname) =
  if AB.containsNickname walletNickname addressBook then
    Nothing
  else
    Just NonExistentNickname

-- TODO: Add proper slot input validation. It's not necessary yet, because slot parameters are
-- readonly for now.
slotError :: String -> Maybe SlotError
slotError "" = Just EmptySlot

slotError _ = Nothing

valueError :: String -> Maybe ValueError
valueError "" = Just EmptyValue

valueError _ = Nothing

templateSetupIsValid :: State -> Boolean
templateSetupIsValid state =
  let
    contractNicknameInput = view _contractNicknameInput state

    roleWalletInputs = view _roleWalletInputs state

    slotContentInputs = view _slotContentInputs state

    valueContentInputs = view _valueContentInputs state
  in
    (isNothing $ validate contractNicknameInput)
      && (Map.isEmpty $ Map.mapMaybe validate roleWalletInputs)
      && (Map.isEmpty $ Map.mapMaybe validate slotContentInputs)
      && (Map.isEmpty $ Map.mapMaybe validate valueContentInputs)
