module Component.Contract.View
  ( currentStepActions
  , firstLetterInCircle
  , participantWithNickname
  , renderParty
  , startingStepActions
  , timeoutString
  ) where

import Prologue hiding (div)

import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon)
import Css as Css
import Data.Array (intercalate)
import Data.Array as Array
import Data.BigInt.Argonaut (fromString)
import Data.BigInt.Argonaut as BigInt
import Data.Lens ((^.))
import Data.Map (lookup) as Map
import Data.Maybe (isJust, maybe, maybe')
import Data.Set as Set
import Data.String (take, trim)
import Data.String.Extra (capitalize)
import Data.Tuple (uncurry)
import Data.WalletNickname as WN
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (applyWhen, classNames)
import Halogen.HTML (HTML, button, div, div_, h4, input, p_, span, span_, text)
import Halogen.HTML.Events.Extra (onClick_, onValueInput_)
import Halogen.HTML.Properties
  ( InputType(..)
  , enabled
  , placeholder
  , type_
  , value
  )
import Humanize (humanizeDuration, humanizeValue)
import MainFrame.Types (ChildSlots)
import Marlowe.Execution.Lenses (_mNextTimeout)
import Marlowe.Execution.State (isClosed)
import Marlowe.Execution.Types (NamedAction(..))
import Marlowe.Execution.Types as Execution
import Marlowe.Semantics
  ( Bound(..)
  , ChoiceId(..)
  , Party(..)
  , Slot
  , getEncompassBound
  )
import Marlowe.Slot (secondsDiff)
import Page.Contract.Lenses
  ( _executionState
  , _metadata
  , _participants
  , _userParties
  )
import Page.Contract.Types (Action(..), StartedState)
import Text.Markdown.TrimmedInline (markdownToHTML)

timeoutString :: Slot -> Execution.State -> String
timeoutString currentSlot executionState =
  let
    mNextTimeout = executionState ^. _mNextTimeout
  in
    maybe'
      ( \_ ->
          if isClosed executionState then "Contract closed"
          else "Timed out"
      )
      (\nextTimeout -> humanizeDuration $ secondsDiff nextTimeout currentSlot)
      mNextTimeout

currentStepActions
  :: forall m
   . MonadAff m
  => StartedState
  -> ComponentHTML Action ChildSlots m
currentStepActions state =
  let
    executionState = state ^. _executionState
  in
    case
      isClosed executionState,
      isJust executionState.mPendingTransaction,
      state.namedActions
      of
      true, _, _ ->
        div
          [ classNames
              [ "h-full", "flex", "flex-col", "justify-center", "items-center" ]
          ]
          [ icon Icon.TaskAlt [ "text-green", "text-big-icon" ]
          , div
              -- The bottom margin on this div means the whole thing isn't perfectly centered vertically.
              -- Because there's an image on top and text below, being perfectly centered vertically
              -- looks wrong.
              [ classNames [ "text-center", "text-sm", "mb-4" ] ]
              [ div [ classNames [ "font-semibold" ] ]
                  [ text "This contract is now closed" ]
              , div_ [ text "There are no tasks to complete" ]
              ]
          ]
      _, true, _ ->
        -- FIXME: when we have a design for this, we can include more information (probably making this look more like
        -- the past step card)
        div_
          [ text
              "Your transaction has been submitted. You will be notified when confirmation is received."
          ]
      _, _, [] ->
        let
          purpleDot extraCss = div
            [ classNames $
                [ "rounded-full"
                , "bg-lightpurple"
                , "w-3"
                , "h-3"
                , "animate-grow"
                ]
                  <> extraCss
            ]
            []
        in
          div
            [ classNames [ "text-xs", "flex", "flex-col", "h-full", "gap-2" ] ]
            [ h4 [ classNames [ "font-semibold" ] ] [ text "Please wait…" ]
            , p_
                [ text
                    "There are no tasks to complete at this step. The contract will progress automatically when the timeout passes."
                ]
            , div
                [ classNames
                    [ "flex-grow"
                    , "flex"
                    , "justify-center"
                    , "items-center"
                    , "gap-2"
                    ]
                ]
                [ purpleDot []
                , purpleDot [ "animate-delay-150" ]
                , purpleDot [ "animate-delay-300" ]
                ]
            ]
      _, _, namedActions ->
        div
          [ classNames [ "space-y-4" ] ]
          $ uncurry (renderPartyTasks state)
              <$> namedActions

-- TODO: In zeplin all participants have a different color. We need to decide how are we going to assing
--       colors to users. For now they all have purple
renderParty :: forall p a. StartedState -> Party -> HTML p a
renderParty state party =
  let
    participantName = participantWithNickname state party

    userParties = state ^. _userParties
  in
    div [ classNames $ [ "text-xs", "flex", "gap-1" ] ]
      [ firstLetterInCircle
          { styles:
              [ "bg-gradient-to-r"
              , "from-purple"
              , "to-lightpurple"
              , "text-white"
              ]
          , name: participantName
          }
      , div [ classNames [ "font-semibold" ] ]
          [ text $
              if Set.member party userParties then participantName <> " (you)"
              else participantName
          ]
      ]

renderPartyTasks
  :: forall p. StartedState -> Party -> Array NamedAction -> HTML p Action
renderPartyTasks state party actions =
  let
    actionsSeparatedByOr =
      intercalate
        [ div [ classNames [ "font-semibold", "text-center", "text-xs" ] ]
            [ text "OR" ]
        ]
        (Array.singleton <<< renderAction state party <$> actions)
  in
    div [ classNames [ "space-y-2" ] ]
      ([ renderParty state party ] <> actionsSeparatedByOr)

participantWithNickname :: StartedState -> Party -> String
participantWithNickname state party =
  let
    mNickname = join $ Map.lookup party (state ^. _participants)
  in
    capitalize case party, mNickname of
      -- TODO: For the demo we wont have PK, but eventually we probably want to limit the amount of characters
      PK publicKey, _ -> publicKey
      Role roleName, Just nickname -> roleName <> " (" <> WN.toString nickname
        <> ")"
      Role roleName, Nothing -> roleName

-- The Party parameter represents who is taking the action
renderAction :: forall p. StartedState -> Party -> NamedAction -> HTML p Action
renderAction state party namedAction@(MakeDeposit intoAccountOf by token value) =
  let
    userParties = state ^. _userParties

    isActiveParticipant = Set.member party userParties

    fromDescription =
      if isActiveParticipant then
        "You make"
      else case party of
        PK publicKey -> "Account " <> publicKey <> " makes"
        Role roleName -> capitalize roleName <> " makes"

    toDescription =
      if Set.member intoAccountOf userParties then
        "your"
      else if by == intoAccountOf then
        "their"
      else case intoAccountOf of
        PK publicKey -> publicKey <> " public key"
        Role roleName -> roleName <> "'s"

    description = fromDescription <> " a deposit into " <> toDescription <>
      " account"
  in
    div [ classNames [ "space-y-2" ] ]
      [ shortDescription isActiveParticipant description
      , button
          [ classNames $ Css.button <> Css.withAnimation
              <> [ "flex", "justify-between", "w-full" ]
              <>
                if isActiveParticipant then
                  Css.bgBlueGradient <> Css.withShadow
                else
                  [ "text-black", "cursor-default" ]
          , enabled $ isActiveParticipant
          , onClick_ $ AskConfirmation namedAction
          ]
          [ span_ [ text "Deposit:" ]
          , span_ [ text $ humanizeValue token value ]
          ]
      ]

renderAction state party namedAction@(MakeChoice choiceId bounds mChosenNum) =
  let
    userParties = state ^. _userParties

    isActiveParticipant = Set.member party userParties

    metadata = state ^. (_executionState <<< _metadata)

    -- NOTE': We could eventually add an heuristic that if the difference between min and max is less
    --        than 10 elements, we could show a `select` instead of a input[number]
    Bound minBound maxBound = getEncompassBound bounds

    ChoiceId choiceIdKey _ = choiceId

    choiceDescription = case Map.lookup choiceIdKey metadata.choiceInfo of
      Just { choiceDescription: description }
        | trim description /= "" -> shortDescription isActiveParticipant
            description
      _ -> div_ []

    isValid = maybe false (between minBound maxBound) mChosenNum

    multipleInput = \_ ->
      div
        [ classNames
            $
              [ "flex"
              , "w-full"
              , "rounded"
              , "overflow-hidden"
              , "focus-within:ring-1"
              , "ring-black"
              ]
                <> applyWhen isActiveParticipant Css.withShadow
        ]
        [ input
            [ classNames
                [ "border-0"
                , "py-4"
                , "pl-4"
                , "pr-1"
                , "flex-grow"
                , "focus:ring-0"
                , "min-w-0"
                , "text-sm"
                , "disabled:bg-lightgray"
                ]
            , type_ InputNumber
            , enabled isActiveParticipant
            , maybe'
                ( \_ -> placeholder $ "Choose between "
                    <> BigInt.toString minBound
                    <> " and "
                    <> BigInt.toString maxBound
                )
                (value <<< BigInt.toString)
                mChosenNum
            , onValueInput_ $ ChangeChoice choiceId <<< fromString
            ]
        , button
            [ classNames
                ( [ "px-5", "font-bold" ]
                    <>
                      if isValid then
                        Css.bgBlueGradient
                      else
                        [ "bg-darkgray"
                        , "text-white"
                        , "opacity-50"
                        , "cursor-default"
                        ]
                )
            , onClick_ $ AskConfirmation namedAction
            , enabled $ isValid && isActiveParticipant
            ]
            [ text "..." ]
        ]

    singleInput = \_ ->
      button
        [ classNames $ Css.button <> Css.withAnimation <> [ "w-full" ]
            <>
              if isActiveParticipant then
                Css.bgBlueGradient <> Css.withShadow
              else
                [ "text-black", "cursor-default" ]
        , enabled isActiveParticipant
        , onClick_ $ AskConfirmation $ MakeChoice choiceId bounds $ Just
            minBound
        ]
        [ span_ [ text choiceIdKey ]
        ]
  in
    div [ classNames [ "space-y-2" ] ]
      [ choiceDescription
      , if minBound == maxBound then
          singleInput unit
        else
          multipleInput unit
      ]

renderAction _ _ (MakeNotify _) = div [] [ text "FIXME: awaiting observation?" ]

renderAction _ _ (Evaluate _) = div []
  [ text "FIXME: what should we put here? Evaluate" ]

renderAction state party CloseContract =
  let
    userParties = state ^. _userParties

    isActiveParticipant = Set.member party userParties
  in
    div [ classNames [ "space-y-2" ] ]
      -- FIXME: revisit the text
      [ shortDescription isActiveParticipant
          "The contract is still open and needs to be manually closed by any participant for the remainder of the balances to be distributed (charges may apply)"
      , button
          [ classNames $ Css.button <> Css.withAnimation <> [ "w-full" ]
              <>
                if isActiveParticipant then
                  Css.bgBlueGradient <> Css.withShadow
                else
                  [ "text-black", "cursor-default" ]
          , enabled $ isActiveParticipant
          , onClick_ $ AskConfirmation CloseContract
          ]
          [ text "Close contract" ]
      ]

shortDescription :: forall p a. Boolean -> String -> HTML p a
shortDescription isActiveParticipant description =
  div
    [ classNames
        ([ "text-xs" ] <> applyWhen (not isActiveParticipant) [ "opacity-50" ])
    ]
    [ span [ classNames [ "font-semibold" ] ] [ text "Short description: " ]
    , span_ $ markdownToHTML description
    ]

firstLetterInCircle
  :: forall p a. { styles :: Array String, name :: String } -> HTML p a
firstLetterInCircle { styles, name } =
  div
    [ classNames
        $
          [ "rounded-full"
          , "w-5"
          , "h-5"
          , "text-center"
          , "font-semibold"
          ]
            <> styles
    ]
    [ text $ take 1 name ]

startingStepActions :: forall p a. HTML p a
startingStepActions =
  div [ classNames [ "space-y-6" ] ]
    [ placeholderAccount
    , placeholderContent
    ]
  where
  placeholderAccount =
    div [ classNames [ "flex", "items-center", "gap-1" ] ]
      [ placeholderAvatar
      , placeholderName
      ]

  placeholderAvatar = div
    [ classNames [ "bg-gray", "rounded-full", "w-5", "h-5" ] ]
    []

  placeholderName = div
    [ classNames [ "bg-gray", "rounded-sm", "w-1/2", "h-6" ] ]
    []

  placeholderContent = div
    [ classNames [ "bg-gray", "rounded-full", "w-full", "h-12" ] ]
    []
