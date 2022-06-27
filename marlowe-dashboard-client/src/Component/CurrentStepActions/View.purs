module Component.CurrentStepActions.View
  ( currentStepActions
  ) where

import Prologue hiding (div)

import Component.Contract.View (renderParty)
import Component.CurrentStepActions.Types
  ( Action(..)
  , ComponentHTML
  , State
  , _executionState
  , _namedActions
  , _roleTokens
  , _rolesCurrency
  )
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon)
import Css as Css
import Data.Array (intercalate)
import Data.Array as Array
import Data.BigInt.Argonaut (fromString)
import Data.BigInt.Argonaut as BigInt
import Data.Compactable (compact)
import Data.Filterable (filter)
import Data.Lens ((^.))
import Data.Map (lookup) as Map
import Data.Maybe (isJust)
import Data.String (trim)
import Data.String.Extra (capitalize)
import Data.UserNamedActions (haveActions, mapActions)
import Effect.Aff.Class (class MonadAff)
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
import Humanize (humanizeValue)
import Language.Marlowe.Core.V1.Semantics (getEncompassBound)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Bound(..)
  , ChoiceId(..)
  , Party(..)
  , Token(..)
  )
import Marlowe.Execution.State (isClosed)
import Marlowe.Execution.Types (NamedAction(..))
import Page.Contract.Lenses (_metadata)
import Store.RoleTokens (isMyRoleToken)
import Text.Markdown.TrimmedInline (markdownToHTML)

currentStepActions
  :: forall m
   . MonadAff m
  => State
  -> ComponentHTML m
currentStepActions state =
  let
    executionState = state ^. _executionState
  in
    case
      isClosed executionState,
      isJust executionState.mPendingTransaction,
      state ^. _namedActions
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
      _, _, namedActions | haveActions namedActions == false ->
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
          $ renderPartyTasks state `mapActions` namedActions

renderPartyTasks
  :: forall m
   . MonadAff m
  => State
  -> Party
  -> Array NamedAction
  -> ComponentHTML m
renderPartyTasks state party actions =
  let
    roleTokens = state ^. _roleTokens

    currencySymbol = state ^. _rolesCurrency

    actionsSeparatedByOr =
      intercalate
        [ div [ classNames [ "font-semibold", "text-center", "text-xs" ] ]
            [ text "OR" ]
        ]
        (Array.singleton <<< renderAction state party <$> actions)
  in
    div [ classNames [ "space-y-2" ] ]
      ([ renderParty currencySymbol roleTokens party ] <> actionsSeparatedByOr)

-- The Party parameter represents who is taking the action
renderAction :: forall p. State -> Party -> NamedAction -> HTML p Action
renderAction state party namedAction@(MakeDeposit intoAccountOf by token value) =
  let
    roleTokens = state ^. _roleTokens

    currencySymbol = state ^. _rolesCurrency

    isCurrentUser = case _ of
      Role tokenName ->
        isMyRoleToken (Token currencySymbol tokenName) roleTokens
      -- FIXME check against current user's addres when public key hashes are
      -- replaced with addresses in the semantics.
      _ -> false

    isActiveParticipant = isCurrentUser party

    fromDescription =
      if isActiveParticipant then
        "You make"
      else case party of
        PK publicKey -> "Account " <> publicKey <> " makes"
        Role roleName -> capitalize roleName <> " makes"

    toDescription =
      if isCurrentUser intoAccountOf then
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
          , onClick_ $ SelectAction namedAction Nothing
          ]
          [ span_ [ text "Deposit:" ]
          , span_ [ text $ humanizeValue token value ]
          ]
      ]

renderAction state party namedAction@(MakeChoice choiceId bounds) =
  let
    roleTokens = state ^. _roleTokens

    currencySymbol = state ^. _rolesCurrency

    isCurrentUser = case _ of
      Role tokenName ->
        isMyRoleToken (Token currencySymbol tokenName) roleTokens
      -- FIXME check against current user's addres when public key hashes are
      -- replaced with addresses in the semantics.
      _ -> false

    mChosenNum = Map.lookup choiceId $ state.transient.choiceValues

    isActiveParticipant = isCurrentUser party

    metadata = state ^. _executionState <<< _metadata

    -- NOTE': We could eventually add an heuristic that if the difference between min and max is less
    --        than 10 elements, we could show a `select` instead of a input[number]
    Bound minBound maxBound = getEncompassBound bounds

    ChoiceId choiceIdKey _ = choiceId

    choiceDescription = case Map.lookup choiceIdKey metadata.choiceInfo of
      Just { choiceDescription: description }
        | trim description /= "" -> shortDescription isActiveParticipant
            description
      _ -> div_ []

    mValidChosen = filter (between minBound maxBound) mChosenNum
    isValid = isJust mValidChosen

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
        [ input $ compact
            [ Just $ classNames
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
            , Just $ type_ InputNumber
            , Just $ enabled isActiveParticipant
            , Just $ placeholder $ "Choose between "
                <> BigInt.toString minBound
                <> " and "
                <> BigInt.toString maxBound
            , value <<< BigInt.toString <$> mChosenNum
            , Just $ onValueInput_ $ ChangeChoice choiceId <<< fromString
            ]
        , button
            ( compact
                [ Just $ classNames
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
                -- TODO add note
                , mValidChosen <#> \validChosen ->
                    onClick_ $ SelectAction namedAction $ Just validChosen
                , Just $ enabled $ isValid && isActiveParticipant
                ]
            )
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
        , onClick_ $ SelectAction (MakeChoice choiceId bounds) $ Just
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
    roleTokens = state ^. _roleTokens

    currencySymbol = state ^. _rolesCurrency

    isCurrentUser = case _ of
      Role tokenName ->
        isMyRoleToken (Token currencySymbol tokenName) roleTokens
      -- FIXME check against current user's addres when public key hashes are
      -- replaced with addresses in the semantics.
      _ -> false

    isActiveParticipant = isCurrentUser party
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
          , onClick_ $ SelectAction CloseContract Nothing
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
