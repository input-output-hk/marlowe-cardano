module Page.Simulation.View where

import Prologue hiding (div)

import Component.BottomPanel.Types as BottomPanelTypes
import Component.BottomPanel.View as BottomPanel
import Component.CurrencyInput.State (component) as CurrencyInput
import Component.DateTimeLocalInput.State (component) as DateTimeLocalInput
import Component.DateTimeLocalInput.Types (Message(..)) as DateTimeLocalInput
import Component.Hint.State (hint)
import Component.Icons as Icon
import Component.Popper (Placement(..))
import Component.Tooltip.State (tooltip)
import Component.Tooltip.Types (ReferenceId(..))
import Data.Array (concatMap, intercalate, length, mapWithIndex, sortWith)
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.BigInt.Argonaut (BigInt)
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Enum (fromEnum)
import Data.Lens (has, only, preview, previewOn, to, view, (^.), (^?))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.NonEmptyList (_Head)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Numbers.Natural (Natural)
import Data.Numbers.Natural as N
import Data.Set.Ordered.OSet (OSet)
import Data.String (trim)
import Data.Time.Duration (Minutes)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Classes
  ( aHorizontal
  , bold
  , btn
  , flex
  , flexCol
  , flexGrow
  , fontBold
  , fullHeight
  , fullWidth
  , grid
  , gridColsDescriptionLocation
  , group
  , justifyBetween
  , justifyCenter
  , justifyEnd
  , maxH70p
  , minH0
  , noMargins
  , overflowHidden
  , paddingX
  , plusBtn
  , smallBtn
  , smallSpaceBottom
  , spaceBottom
  , spaceRight
  , spanText
  , textSecondaryColor
  , textXs
  , uppercase
  )
import Halogen.Css (classNames)
import Halogen.Extra (renderSubmodule)
import Halogen.HTML
  ( ClassName(..)
  , ComponentHTML
  , HTML
  , PlainHTML
  , aside
  , b_
  , button
  , div
  , div_
  , em
  , em_
  , h4
  , h6
  , h6_
  , li
  , p
  , p_
  , section
  , slot
  , span
  , span_
  , strong_
  , text
  , ul
  )
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Properties (class_, classes, disabled, enabled, id)
import Halogen.Monaco (monacoComponent)
import Humanize
  ( formatPOSIXTime
  , humanizeInterval
  , humanizeOffset
  , humanizeValue
  , localToUtc
  , utcToLocal
  )
import Language.Marlowe.Core.V1.Semantics (inBounds)
import Language.Marlowe.Core.V1.Semantics.Types
  ( AccountId
  , Assets(..)
  , Bound(..)
  , ChoiceId(..)
  , CurrencySymbol
  , Input(..)
  , Party(..)
  , Payee(..)
  , Payment(..)
  , TimeInterval(..)
  , Token(..)
  , TokenName
  , TransactionInput(..)
  , timeouts
  )
import Language.Marlowe.Extended.V1.Metadata
  ( MetaData
  , NumberFormat(..)
  , getChoiceFormat
  )
import MainFrame.Types
  ( ChildSlots
  , _currencyInputSlot
  , _dateTimeInputSlot
  , _simulatorEditorSlot
  )
import Marlowe.Holes as Holes
import Marlowe.Monaco as MM
import Marlowe.Template (TemplateContent(..), orderContentUsingMetadata)
import Marlowe.Time (unixEpoch)
import Monaco as Monaco
import Page.Simulation.BottomPanel (panelContents)
import Page.Simulation.Lenses (_bottomPanelState)
import Page.Simulation.Types (Action(..), BottomPanelView(..), State)
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Pretty (renderPrettyParty, renderPrettyPayee, showPrettyChoice)
import Simulator.Lenses
  ( _SimulationRunning
  , _currentContract
  , _currentMarloweState
  , _executionState
  , _log
  , _marloweState
  , _possibleActions
  , _time
  , _transactionError
  , _transactionWarnings
  )
import Simulator.State (hasHistory)
import Simulator.Types
  ( ActionInput(..)
  , ActionInputId
  , ExecutionState(..)
  , InitialConditionsRecord
  , LogEntry(..)
  , MoveToTimeType(..)
  , otherActionsParty
  )
import Text.Markdown.TrimmedInline (markdownToHTML)

render
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> ComponentHTML Action ChildSlots m
render metadata state =
  div [ classes [ fullHeight, paddingX, flex ] ]
    [ div [ classes [ flex, flexCol, fullHeight, flexGrow ] ]
        [ section [ classes [ minH0, flexGrow, overflowHidden ] ]
            [ marloweEditor ]
        , section [ classes [ maxH70p ] ]
            [ renderSubmodule
                _bottomPanelState
                BottomPanelAction
                (BottomPanel.render panelTitles wrapBottomPanelContents)
                state
            ]
        ]
    , aside
        [ classNames
            [ "flex-shrink-0"
            , "ml-medium"
            , "w-30p"
            , "flex"
            , "flex-col"
            ]
        ]
        (sidebar metadata state)
    ]
  where
  panelTitles =
    [ { title: "Current State", view: CurrentStateView, classes: [] }
    , { title: problemsTitle, view: WarningsAndErrorsView, classes: [] }
    ]

  runtimeWarnings = view
    ( _marloweState <<< _Head <<< _executionState <<< _SimulationRunning <<<
        _transactionWarnings
    )
    state

  hasRuntimeError :: Boolean
  hasRuntimeError = has
    ( _marloweState <<< _Head <<< _executionState <<< _SimulationRunning
        <<< _transactionError
        <<< to isJust
        <<< only true
    )
    state

  numberOfProblems = length runtimeWarnings + fromEnum hasRuntimeError

  problemsTitle = "Warnings and errors" <>
    if numberOfProblems == 0 then "" else " (" <> show numberOfProblems <> ")"

  -- TODO: improve this wrapper helper
  actionWrapper = BottomPanelTypes.PanelAction

  wrapBottomPanelContents panelView = bimap (map actionWrapper) actionWrapper $
    panelContents metadata state panelView

otherActions :: forall p. HTML p Action
otherActions =
  div [ classes [ group ] ]
    [ editSourceButton ]

{-
    FIXME(outdated): This code was disabled because we changed "source" to "workflow" and
           we move it to the MainFrame. This posses a challenge, were this subcomponent
           needs to see information from the parent state which is not available in the
           subcomponent state.
           There were four possible solutions to this problem:
             * the easy but error prone would be to duplicate state in the MainFrame and here
             * we could change the type of Simulation.State to be
                type State =
                  { ownState :: OwnState -- what we currently call State
                  , parentState :: ProjectedState -- what data from the parent we need in this view, namely workflow
                  }
                or
                type State =
                  { simulationState :: Simulation.OwnState
                  , workflow :: Maybe Workflow
                  }
               which is similar but more "direct", and use a custom lense to provide access to both
               parts of the state.
             * Add the notion of "input" to the subcomponents, similar to what Halogen components do
             * we can reduce functionality and just say "Edit source"
           We opted for the last one as it's the simplest and least conflicting. In January the frontend
           team should meet to discuss the alternatives.
    EDIT: We should just abandon submodules and use regular Components. That will
          allow us to inject the data we need via the input of the component.
    [ sendToBlocklyButton state
      ]
        <> ( if has (_source <<< only Haskell) state then
              [ haskellSourceButton state ]
            else
              []
          )
        <> ( if has (_source <<< only Javascript) state then
              [ javascriptSourceButton state ]
            else
              []
          )
        <> ( if has (_source <<< only Actus) state then
              [ actusSourceButton state ]
            else
              []
          )

sendToBlocklyButton :: forall p. State -> HTML p Action
sendToBlocklyButton state =
  button
    [ onClick $ const $ Just $ SetBlocklyCode
    , enabled isBlocklyEnabled
    , classes [ Classes.disabled (not isBlocklyEnabled) ]
    ]
    [ text "View in Blockly Editor" ]
  where
  isBlocklyEnabled = view (_marloweState <<< _Head <<< _editorErrors <<< to Array.null) state

haskellSourceButton :: forall p. State -> HTML p Action
haskellSourceButton state =
  button
    [ onClick $ const $ Just $ EditHaskell
    ]
    [ text "Edit Haskell Source" ]

javascriptSourceButton :: forall p. State -> HTML p Action
javascriptSourceButton state =
  button
    [ onClick $ const $ Just $ EditJavascript
    ]
    [ text "Edit Javascript Source" ]

actusSourceButton :: forall p. State -> HTML p Action
actusSourceButton state =
  button
    [ onClick $ const $ Just $ EditActus
    ]
    [ text "Edit Actus Source" ]
-}
editSourceButton :: forall p. HTML p Action
editSourceButton =
  button
    [ onClick $ const EditSource
    , classNames [ "btn" ]
    ]
    [ text "Edit source" ]

marloweEditor
  :: forall m
   . MonadAff m
  => ComponentHTML Action ChildSlots m
marloweEditor = slot _simulatorEditorSlot unit component unit
  HandleEditorMessage
  where
  setup editor = liftEffect $ Monaco.setReadOnly editor true

  component = monacoComponent $ MM.settings setup

------------------------------------------------------------
sidebar
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> Array (ComponentHTML Action ChildSlots m)
sidebar metadata state =
  case preview (_marloweState <<< _Head <<< _executionState) state of
    Just (SimulationNotStarted notStartedRecord) ->
      [ startSimulationWidget
          metadata
          notStartedRecord
          state.tzOffset
      ]
    Just (SimulationRunning _) ->
      [ div [ class_ smallSpaceBottom ] [ simulationStateWidget state ]
      , div [ class_ spaceBottom ] [ actionWidget metadata state ]
      , logWidget metadata state
      ]
    Nothing -> [ div_ [ text "Simulation not ready" ] ]

------------------------------------------------------------

type TemplateFormDisplayInfo =
  { lookupDefinition ::
      String -> Maybe String -- Gets the definition for a given key
  , title :: String -- Title of the section of the template type
  , orderedMetadataSet ::
      OSet String -- Ordered set of parameters with metadata (in custom metadata order)
  }

startSimulationWidget
  :: forall m
   . MonadAff m
  => MetaData
  -> InitialConditionsRecord
  -> Minutes
  -> ComponentHTML Action ChildSlots m
startSimulationWidget
  metadata
  { initialTime
  , templateContent
  }
  tzOffset =
  cardWidget "Simulation has not started yet"
    $ div_
        [ div
            [ classes [ ClassName "time-input", ClassName "initial-time-input" ]
            ]
            [ spanText "Initial time:"
            , marloweInstantInput "initial-time"
                [ "mx-2", "flex-grow", "flex-shrink-0", "flex", "gap-2" ]
                SetInitialTime
                initialTime
                tzOffset
            ]
        , templateParameters
            "simulator"
            metadata
            templateContent
            { valueAction: SetValueTemplateParam
            , timeAction: SetTimeTemplateParam
            }
            tzOffset
        , div [ classNames [ "transaction-btns", "flex", "justify-center" ] ]
            [ button
                [ classNames
                    [ "btn", "bold", "flex-1", "max-w-[15rem]", "mx-2" ]
                , onClick $ const DownloadAsJson
                ]
                [ text "Download as JSON" ]
            , button
                [ classNames
                    [ "btn", "bold", "flex-1", "max-w-[15rem]", "mx-2" ]
                , onClick $ const StartSimulation
                ]
                [ text "Start simulation" ]
            ]
        ]

type TemplateParameterActionsGen action =
  { valueAction :: String -> BigInt -> action
  , timeAction :: String -> Instant -> action
  }

templateParameters
  :: forall action m
   . MonadAff m
  => String
  -> MetaData
  -> TemplateContent
  -> TemplateParameterActionsGen action
  -> Minutes
  -> ComponentHTML action ChildSlots m
templateParameters
  refPrefix
  metadata
  (TemplateContent { timeContent, valueContent })
  { valueAction, timeAction }
  tzOffset =

  let
    inputCss = [ "mx-2", "flex-grow", "flex-shrink-0", "flex", "gap-2" ]
    timeoutParameters = templateParametersSection
      refPrefix
      ( \fieldName fieldValue ->
          marloweInstantInput
            (templateFieldRef refPrefix fieldName)
            inputCss
            (timeAction fieldName)
            fieldValue
            tzOffset
      )
      timeParameterDisplayInfo
      timeContent

    valueParameters = templateParametersSection
      refPrefix
      ( \fieldName fieldValue ->
          case extractValueParameterNumberFormat fieldName of
            Just (currencyLabel /\ numDecimals) ->
              marloweCurrencyInput (templateFieldRef refPrefix fieldName)
                inputCss
                (valueAction fieldName)
                (Just currencyLabel)
                numDecimals
                fieldValue
            Nothing -> marloweActionInput (templateFieldRef refPrefix fieldName)
              inputCss
              (valueAction fieldName)
              fieldValue
      )
      valueParameterDisplayInfo
      valueContent
    lookupDescription k m =
      ( case Map.lookup k m of
          Just { valueParameterDescription: description }
            | trim description /= "" -> Just description
          _ -> Nothing
      )

    timeParameterDisplayInfo =
      { lookupDefinition: (flip Map.lookup)
          (Map.fromFoldableWithIndex metadata.timeParameterDescriptions) -- Convert to normal Map for efficiency
      , title: "Timeout template parameters"
      , orderedMetadataSet: OMap.keys metadata.timeParameterDescriptions
      }

    valueParameterDisplayInfo =
      { lookupDefinition: (flip lookupDescription)
          (Map.fromFoldableWithIndex metadata.valueParameterInfo) -- Convert to normal Map for efficiency
      , title: "Value template parameters"
      , orderedMetadataSet: OMap.keys metadata.valueParameterInfo
      }
    extractValueParameterNumberFormat fieldName =
      case OMap.lookup fieldName metadata.valueParameterInfo of
        Just { valueParameterFormat: DecimalFormat numDecimals currencyLabel } ->
          Just (currencyLabel /\ N.fromInt numDecimals)
        _ -> Nothing
  in
    div_ (timeoutParameters <> valueParameters)

-- NOTE: The refPrefix parameter is used to avoid sharing the same component between
--       different views (like Simulator and Static analyisis bottom bar). The proper
--       solution would be to make each page a proper component.
templateFieldRef :: String -> String -> String
templateFieldRef refPrefix fieldName =
  "template-parameter-"
    <> refPrefix
    <> "-"
    <> fieldName

emptyDiv :: forall w i. HTML w i
emptyDiv = div_ []

templateParametersSection
  :: forall inputType action m
   . MonadAff m
  => String
  -> (String -> inputType -> ComponentHTML action ChildSlots m)
  -> TemplateFormDisplayInfo
  -> Map String inputType
  -> Array (ComponentHTML action ChildSlots m)
templateParametersSection
  refPrefix
  componentGen
  { lookupDefinition
  , title
  , orderedMetadataSet
  }
  content =
  let
    templateFieldTitle =
      h6 [ classNames [ "italic", "m-0", "mb-4" ] ]
        [ text title ]

    parameterHint fieldName =
      maybe emptyDiv
        ( \explanation ->
            hint
              [ "leading-none" ]
              (templateFieldRef refPrefix fieldName)
              Auto
              (markdownHintWithTitle fieldName explanation)

        )
        $ lookupDefinition fieldName

    templateParameter (fieldName /\ fieldValue) =
      div
        [ classNames [ "m-2", "ml-6", "flex", "flex-wrap" ] ]
        [ div_
            [ strong_ [ text fieldName ]
            , text ":"
            ]
        , parameterHint fieldName
        , componentGen fieldName fieldValue
        ]
    orderedContent = orderContentUsingMetadata content orderedMetadataSet
  in
    if Map.isEmpty content then
      []
    else
      join
        [ [ templateFieldTitle ]
        , OMap.toUnfoldable orderedContent <#> templateParameter
        ]

------------------------------------------------------------
simulationStateWidget :: forall p. State -> HTML p Action
simulationStateWidget state =
  let
    tzOffset = state.tzOffset
    offsetStr = humanizeOffset tzOffset
    currentTime = state ^.
      ( _currentMarloweState <<< _executionState <<< _SimulationRunning
          <<< _time
          -- TODO: SCP-3887 unify time construct
          <<< to POSIXTime
          -- TODO SCP-3833 Add type safety to timezone conversions
          <<< to (formatPOSIXTime tzOffset)
          <<< to \(dateStr /\ timeStr) ->
            intercalate " " [ dateStr, timeStr, offsetStr ]
      )

    expirationTime = contractMaxTime (previewOn state _currentContract)

    contractMaxTime = case _ of
      Nothing -> "Closed"
      Just (Holes.Term Holes.Close _) -> "Closed"
      Just contract ->
        let
          posixTime = (_.maxTime <<< unwrap <<< timeouts) contract
          -- TODO SCP-3833 Add type safety to timezone conversions
          dateStr /\ timeStr = formatPOSIXTime tzOffset posixTime
        in
          intercalate " " [ dateStr, timeStr, offsetStr ]

    indicator name value =
      div [ classNames [ "flex", "flex-col" ] ]
        [ span
            [ class_ bold ]
            [ text $ name <> ": " ]
        , span_ [ text value ]
        ]
  in
    div
      [ classes [ flex, justifyBetween ] ]
      [ indicator "current time" currentTime
      , indicator "expiration time" expirationTime
      ]

------------------------------------------------------------
actionWidget
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> ComponentHTML Action ChildSlots m
actionWidget metadata state =
  cardWidget "Actions"
    $ div [ classes [] ]
        [ ul [ class_ (ClassName "participants") ]
            if (Map.isEmpty possibleActions) then
              [ text "No valid inputs can be added to the transaction" ]
            else
              (actionsForParties possibleActions)
        , div [ classes [ ClassName "transaction-btns", flex, justifyCenter ] ]
            [ button
                [ classes [ btn, bold, spaceRight ]
                , disabled $ not $ hasHistory state
                , onClick $ const Undo
                ]
                [ text "Undo" ]
            , button
                [ classes [ btn, bold ]
                , onClick $ const ResetSimulator
                ]
                [ text "Reset" ]
            ]
        ]
  where
  possibleActions = fromMaybe Map.empty $ state ^? _marloweState <<< _Head
    <<< _executionState
    <<< _SimulationRunning
    <<< _possibleActions
    <<< _Newtype

  kvs :: forall k v. Map k v -> Array (Tuple k v)
  kvs = Map.toUnfoldable

  vs :: forall k v. Map k v -> Array v
  vs m = map snd (kvs m)

  sortParties :: forall v. Array (Tuple Party v) -> Array (Tuple Party v)
  sortParties = sortWith (\(Tuple party _) -> party == otherActionsParty)

  actionsForParties
    :: Map Party (Map ActionInputId ActionInput)
    -> Array (ComponentHTML Action ChildSlots m)
  actionsForParties m = map
    (\(Tuple k v) -> participant metadata state k (vs v))
    (sortParties (kvs m))

participant
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> Party
  -> Array ActionInput
  -> ComponentHTML Action ChildSlots m
participant metadata state party actionInputs =
  li [ classes [ noMargins ] ]
    ( [ title ]
        <> (map (inputItem metadata state) actionInputs)
    )
  where
  partyHint = case party of
    Role roleName ->
      maybe emptyDiv
        ( \explanation ->
            hint
              [ "relative", "-top-1" ]
              ("participant-hint-" <> roleName)
              Auto
              (markdownHintWithTitle roleName explanation)
        )
        $ Map.lookup roleName metadata.roleDescriptions
    _ -> emptyDiv

  title =
    div [ classes [ ClassName "action-group" ] ]
      if party == otherActionsParty then
        -- QUESTION: if we only have "move to time", could we rename this to "Time Actions"?
        [ h6_ [ em_ [ text "Other Actions" ] ] ]
      else
        [ h6_
            [ em [ classNames [ "mr-1" ] ]
                [ text "Participant "
                , strong_ [ text partyName ]
                ]
            , partyHint
            ]
        ]

  partyName = case party of
    (PK name) -> name
    (Role name) -> name

choiceRef :: String -> ChoiceId -> String
choiceRef prefix (ChoiceId choiceName choiceOwner) = intercalate "-"
  [ prefix, choiceName, choiceOwnerStr ]
  where
  choiceOwnerStr = case choiceOwner of
    PK pk -> pk
    Role name -> name

inputItem
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> ActionInput
  -> ComponentHTML Action ChildSlots m
inputItem metadata _ (DepositInput accountId party token value) =
  div [ classes [ ClassName "action", aHorizontal ] ]
    [ renderDeposit metadata accountId party token value
    , div [ class_ (ClassName "align-top") ]
        [ button
            [ classes [ plusBtn, smallBtn, btn ]
            , onClick $ const $ AddInput (IDeposit accountId party token value)
                []
            ]
            [ text "+" ]
        ]
    ]

inputItem
  metadata
  _
  (ChoiceInput choiceId@(ChoiceId choiceName choiceOwner) bounds chosenNum) =
  let
    ref = choiceRef "choice-hint" choiceId

    choiceHint =
      maybe (div_ [])
        ( \explanation ->
            hint
              [ "relative", "-top-1" ]
              ref
              Auto
              (markdownHintWithTitle choiceName explanation)
        )
        (mChoiceInfo >>= mExtractDescription)
  in
    div
      [ classes [ ClassName "action", aHorizontal, ClassName "flex-nowrap" ] ]
      ( [ div [ classes [ ClassName "action-label" ] ]
            [ div [ class_ (ClassName "choice-input") ]
                [ span [ class_ (ClassName "break-word-span") ]
                    [ text "Choice "
                    , b_ [ text (show choiceName <> ": ") ]
                    , choiceHint
                    ]
                , case mChoiceInfo of
                    Just
                      { choiceFormat: DecimalFormat numDecimals currencyLabel } ->
                      marloweCurrencyInput ref
                        [ "mx-2", "flex-grow", "flex-shrink-0" ]
                        (SetChoice choiceId)
                        (Just currencyLabel)
                        (N.fromInt numDecimals)
                        chosenNum
                    _ -> marloweActionInput ref
                      [ "mx-2", "flex-grow", "flex-shrink-0" ]
                      (SetChoice choiceId)
                      chosenNum
                ]
            , div [ class_ (ClassName "choice-error") ] error
            ]
        ]
          <> addButton
      )
  where
  mChoiceInfo = Map.lookup choiceName metadata.choiceInfo

  mExtractDescription { choiceDescription }
    | trim choiceDescription /= "" = Just choiceDescription

  mExtractDescription _ = Nothing

  addButton =
    [ button
        [ classes
            [ btn
            , plusBtn
            , smallBtn
            , ClassName "align-top"
            , ClassName "flex-noshrink"
            ]
        , onClick $ const $ AddInput
            (IChoice (ChoiceId choiceName choiceOwner) chosenNum)
            bounds
        , enabled $ inBounds chosenNum bounds
        ]
        [ text "+" ]
    ]

  error = if inBounds chosenNum bounds then [] else [ text boundsError ]

  boundsError =
    if Array.null bounds then
      "A choice must have set bounds, please fix the contract"
    else
      "Choice must be between " <> intercalate " or " (map boundError bounds)

  boundError (Bound from to) = showPretty from <> " and " <> showPretty to

  showPretty :: BigInt -> String
  showPretty = showPrettyChoice (getChoiceFormat metadata choiceName)

inputItem _ _ NotifyInput =
  li
    [ classes [ ClassName "action", ClassName "choice-a", aHorizontal ] ]
    [ p_ [ text "Notify Contract" ]
    , button
        [ classes [ btn, plusBtn, smallBtn, ClassName "align-top" ]
        , onClick $ const $ AddInput INotify []
        ]
        [ text "+" ]
    ]

inputItem _ state (MoveToTime moveType time) =
  div
    [ classes [ aHorizontal, ClassName "flex-nowrap" ] ]
    ( [ div [ classes [ ClassName "action" ] ]
          [ p_
              [ text "Move current time to "
              , span [ id ref, classNames [ "font-bold", "underline-dotted" ] ]
                  [ text $ case moveType of
                      NextTime -> "next minute"
                      NextTimeout -> "next timeout"
                      ExpirationTime -> "expiration time"
                  ]
              ]
          , p [ class_ (ClassName "choice-error") ] error
          , tooltip fullTime (RefId ref) Bottom
          ]
      ]
        <> addButton
    )
  where
  currentTime = fromMaybe unixEpoch $ state ^?
    _currentMarloweState <<< _executionState <<< _SimulationRunning <<< _time

  ref = "move-to-" <> show moveType
  fullTime =
    formatPOSIXTime
      state.tzOffset
      (POSIXTime time)
      # \(dateStr /\ timeStr) ->
          intercalate " " [ dateStr, timeStr, humanizeOffset state.tzOffset ]
  isForward = currentTime < time

  addButton =
    if isForward then
      [ button
          [ classes
              [ plusBtn
              , smallBtn
              , ClassName "align-top"
              , ClassName "flex-noshrink"
              , btn
              ]
          , onClick $ const $ MoveTime time
          ]
          [ text "+" ]
      ]
    else
      []

  error = if isForward then [] else [ text boundsError ]

  boundsError = "The new time must be more than the current time."

marloweCurrencyInput
  :: forall m action
   . MonadEffect m
  => String
  -> Array String
  -> (BigInt -> action)
  -> Maybe String
  -> Maybe Natural
  -> BigInt
  -> ComponentHTML action ChildSlots m
marloweCurrencyInput
  ref
  classList
  f
  currencySymbol
  majorCurrencyFactor
  amountInMinor =
  slot
    _currencyInputSlot
    ref
    CurrencyInput.component
    { classList, amountInMinor, currencySymbol, majorCurrencyFactor }
    f

-- This component builds on top of the DateTimeLocal component to work
-- with Instant and to do the UTC convertion. Value in and out are expressed
-- in UTC.
marloweInstantInput
  :: forall m action
   . String
  -> Array String
  -> (Instant -> action)
  -> Instant
  -> Minutes
  -> ComponentHTML action ChildSlots m
marloweInstantInput ref classList f current tzOffset =
  div [ classNames classList ]
    [ slot
        _dateTimeInputSlot
        ref
        DateTimeLocalInput.component
        { classList: [ "flex-grow" ]
        -- TODO: SCP-3833 Add type safety to timezone conversions
        , value: utcToLocal tzOffset $
            Instant.toDateTime current
        , trimSeconds: true
        }
        ( \(DateTimeLocalInput.ValueChanged dt) -> f $ Instant.fromDateTime $
            localToUtc
              tzOffset
              dt
        )
    , text $ humanizeOffset tzOffset
    ]

marloweActionInput
  :: forall m action
   . MonadEffect m
  => String
  -> Array String
  -> (BigInt -> action)
  -> BigInt
  -> ComponentHTML action ChildSlots m
marloweActionInput ref classes f current = marloweCurrencyInput ref classes f
  Nothing
  Nothing
  current

renderDeposit
  :: forall p
   . MetaData
  -> AccountId
  -> Party
  -> Token
  -> BigInt
  -> HTML p Action
renderDeposit metadata accountOwner party tok money =
  span [ classes [ ClassName "break-word-span" ] ]
    [ text "Deposit "
    , strong_ [ text (humanizeValue tok money) ]
    , text " from "
    , strong_ [ renderPrettyParty metadata party ]
    , text " wallet, into "
    , strong_ [ renderPrettyParty metadata accountOwner ]
    , text " account"
    ]

------------------------------------------------------------
logWidget
  :: forall m
   . MonadAff m
  => MetaData
  -> State
  -> ComponentHTML Action ChildSlots m
logWidget metadata state =
  cardWidget "Transaction log"
    $ div [ classes [ grid, gridColsDescriptionLocation, fullWidth ] ]
        ( [ div [ class_ fontBold ] [ text "Event" ]
          , div [ class_ fontBold ] [ text "Time" ]
          ]
            <> inputLines
        )
  where
  logEntries = state ^.
    ( _marloweState
        <<< _Head
        <<< _executionState
        <<< _SimulationRunning
        <<< _log
    )
  inputLines = join $ logToLines state.tzOffset metadata `mapWithIndex`
    logEntries

logTime
  :: forall m a
   . MonadAff m
  => Int /\ Int
  -> Minutes
  -> TimeInterval
  -> ComponentHTML a ChildSlots m
logTime (step /\ subStep) tzOffset time@(TimeInterval start _) = span
  [ class_ justifyEnd, id ref ]
  [ text startTime
  , tooltip fullTime (RefId ref) Auto
  ]
  where
  ref = intercalate "-" [ "log", "time", show step, show subStep ]
  _ /\ startTime = formatPOSIXTime tzOffset start
  fullTime = intercalate " "
    [ "Event executed"
    , humanizeInterval tzOffset time
    , humanizeOffset tzOffset
    ]

logToLines
  :: forall m a
   . MonadAff m
  => Minutes
  -> MetaData
  -> Int
  -> LogEntry
  -> Array (ComponentHTML a ChildSlots m)
logToLines tzOffset _ stepNumber (StartEvent time) =
  [ span_ [ text "Contract started" ]
  , logTime (stepNumber /\ 0) tzOffset interval
  ]
  where
  interval = TimeInterval time' time'
  time' = POSIXTime time
logToLines
  tzOffset
  metadata
  stepNumber
  (InputEvent (TransactionInput { interval, inputs })) =
  join $
    mapWithIndex
      ( \subStep input -> inputToLine
          tzOffset
          metadata
          interval
          (stepNumber /\ subStep)
          input
      )
      (Array.fromFoldable inputs)

logToLines tzOffset metadata stepNumber (OutputEvent interval payment) =
  paymentToLines
    tzOffset
    metadata
    interval
    stepNumber
    payment

logToLines tzOffset _ stepNumber (CloseEvent timeInterval) =
  [ span_ [ text $ "Contract ended" ]
  , logTime (stepNumber /\ 0) tzOffset timeInterval
  ]

inputToLine
  :: forall m a
   . MonadAff m
  => Minutes
  -> MetaData
  -> TimeInterval
  -> (Int /\ Int)
  -> Input
  -> Array (ComponentHTML a ChildSlots m)
inputToLine
  tzOffset
  metadata
  timeInterval
  stepNumber
  (IDeposit accountOwner party token money) =
  [ span_
      [ strong_ [ renderPrettyParty metadata party ]
      , text " deposited "
      , strong_ [ text (humanizeValue token money) ]
      , text " from his/her wallet into "
      , strong_ [ renderPrettyParty metadata accountOwner ]
      , text " account "
      ]
  , logTime stepNumber tzOffset timeInterval
  ]

inputToLine
  tzOffset
  metadata
  timeInterval
  stepNumber
  (IChoice (ChoiceId choiceName choiceOwner) chosenNum) =
  [ span_
      [ strong_ [ renderPrettyParty metadata choiceOwner ]
      , text " choosed the value "
      , strong_
          [ text
              (showPrettyChoice (getChoiceFormat metadata choiceName) chosenNum)
          ]
      , text " for choice with id "
      , strong_ [ text (show choiceName) ]
      ]
  , logTime stepNumber tzOffset timeInterval
  ]

inputToLine tzOffset _ timeInterval stepNumber INotify =
  [ text "Notify"
  , logTime stepNumber tzOffset timeInterval
  ]

paymentToLines
  :: forall m a
   . MonadAff m
  => Minutes
  -> MetaData
  -> TimeInterval
  -> Int
  -> Payment
  -> Array (ComponentHTML a ChildSlots m)
paymentToLines
  tzOffset
  metadata
  timeInterval
  stepNumber
  (Payment accountId payee money) =
  let
    Assets money' = money

    assets :: Array (CurrencySymbol /\ TokenName /\ BigInt)
    assets = do
      currencySymbol /\ asset <- Map.toUnfoldable money'
      tokenName /\ value <- Map.toUnfoldable asset
      pure $ currencySymbol /\ tokenName /\ value

  in
    join $
      mapWithIndex
        ( \subStep (currencySymbol /\ tokenName /\ value) ->
            paymentToLine
              tzOffset
              metadata
              timeInterval
              accountId
              payee
              (stepNumber /\ subStep)
              (Token currencySymbol tokenName)
              value
        )
        assets

paymentToLine
  :: forall m a
   . MonadAff m
  => Minutes
  -> MetaData
  -> TimeInterval
  -> AccountId
  -> Payee
  -> Int /\ Int
  -> Token
  -> BigInt
  -> Array (ComponentHTML a ChildSlots m)
paymentToLine
  tzOffset
  metadata
  timeInterval
  accountId
  payee
  stepNumber
  token
  money =
  [ span_
      [ text "The contract pays "
      , strong_ [ text (humanizeValue token money) ]
      , text " from "
      , strong_ $ renderPrettyPayee metadata (Account accountId)
      , text " to "
      , strong_ $ renderPrettyPayee metadata payee
      ]
  , logTime stepNumber tzOffset timeInterval
  ]

unfoldAssets :: forall a. Assets -> (Token -> BigInt -> a) -> Array a
unfoldAssets (Assets mon) f =
  concatMap
    ( \(Tuple currencySymbol tokenMap) ->
        ( map
            ( \(Tuple tokenName value) ->
                f (Token currencySymbol tokenName) value
            )
            (Map.toUnfoldable tokenMap)
        )
    )
    (Map.toUnfoldable mon)

------------------------------------------------------------
cardWidget :: forall p a. String -> HTML p a -> HTML p a
cardWidget name body =
  let
    title' = h6
      [ classes [ noMargins, textSecondaryColor, bold, uppercase, textXs ] ]
      [ text name ]
  in
    div
      [ classNames
          [ "simulation-card-widget", "overflow-hidden", "flex", "flex-col" ]
      ]
      [ div [ classNames [ "simulation-card-widget-header" ] ]
          [ title' ]
      , div [ classNames [ "simulation-card-widget-body", "overflow-scroll" ] ]
          [ body ]
      ]

markdownHintWithTitle :: String -> String -> PlainHTML
markdownHintWithTitle title markdown =
  div_
    $
      [ h4
          -- With min-w-max we define that the title should never break into
          -- a different line.
          [ classNames
              [ "no-margins"
              , "text-lg"
              , "font-semibold"
              , "flex"
              , "items-center"
              , "pb-2"
              , "min-w-max"
              ]
          ]
          [ Icon.icon Icon.HelpOutline [ "mr-1", "font-normal" ]
          , text title
          ]
      ]
        <> markdownToHTML markdown
