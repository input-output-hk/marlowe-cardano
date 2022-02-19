module Page.Contract.View
  ( contractScreen
  ) where

import Prologue hiding (div)

import Component.Contacts.State (adaToken)
import Component.Contract.View
  ( currentStep
  , currentStepActions
  , firstLetterInCircle
  , participantWithNickname
  , partyToParticipant
  , paymentToTransfer
  , renderParty
  , startingStepActions
  , timeoutString
  )
import Component.Hint.State (hint)
import Component.Icons (Icon(..)) as Icon
import Component.Icons (icon, icon_)
import Component.Popper (Placement(..))
import Component.Progress.Circular as Progress
import Component.Tooltip.State (tooltip)
import Component.Tooltip.Types (ReferenceId(..))
import Component.Transfer.Types (Termini(..))
import Component.Transfer.View (transfer)
import Data.Array (fromFoldable, intercalate, length)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Compactable (compact)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Lens ((^.))
import Data.List.NonEmpty (foldr)
import Data.Map (intersectionWith, keys, toUnfoldable) as Map
import Data.Maybe (isJust, maybe, maybe')
import Data.Set as Set
import Data.String (take)
import Data.Time.Duration (Minutes)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Halogen (ComponentHTML)
import Halogen.Css (applyWhen, classNames)
import Halogen.Extra (LifecycleEvent(..), lifeCycleSlot)
import Halogen.HTML (HTML, a, button, div, div_, h4_, span, span_, text)
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events.Extra (onClick_)
import Halogen.HTML.Properties (IProp, enabled, id, ref)
import Humanize (formatDate, formatTime, humanizeOffset, humanizeValue)
import MainFrame.Types (ChildSlots)
import Marlowe.Execution.Lenses (_semanticState)
import Marlowe.Execution.State (expandBalances, isClosed)
import Marlowe.Execution.Types (NamedAction(..))
import Marlowe.PAB (transactionFee)
import Marlowe.Semantics
  ( ChoiceId(..)
  , Party(..)
  , Slot
  , SlotInterval(..)
  , Token
  , TransactionInput(..)
  , _accounts
  )
import Marlowe.Semantics (Input(..)) as S
import Marlowe.Slot (posixTimeToDateTime, slotToDateTime)
import Page.Contract.Lenses
  ( _executionState
  , _expandPayments
  , _namedActions
  , _participants
  , _previousSteps
  , _resultingPayments
  , _selectedStep
  , _userParties
  )
import Page.Contract.Types
  ( Action(..)
  , ContractState(..)
  , PreviousStep
  , PreviousStepState(..)
  , StartedState
  , State
  , StepBalance
  , Tab(..)
  , TimeoutInfo
  , scrollContainerRef
  )

-------------------------------------------------------------------------------
-- Top-level views
-------------------------------------------------------------------------------
contractScreen
  :: forall m. MonadAff m => State -> ComponentHTML Action ChildSlots m
contractScreen state =
  let
    cards = case state.contract of
      Started started ->
        let
          pastStepsCards =
            mapWithIndex
              (renderPastStep state.tzOffset started)
              (started ^. _previousSteps)

          currentStepCard = [ renderCurrentStep state.currentSlot started ]

          cardForStep stepNumber
            | stepNumber == started.selectedStep = card
                [ "mx-2", "shadow-sm", "md:shadow-lg" ]
            | otherwise = card [ "-mx-4", "shadow-sm", "md:shadow", "scale-77" ]
        in
          mapWithIndex cardForStep $ pastStepsCards <> currentStepCard
      Starting _ ->
        [ card [ "shadow-sm", "md:shadow-lg" ]
            [ tabBar Tasks Nothing
            , cardBody []
                [ titleBar []
                    [ stepStatusText "Starting contract… " [ "flex-grow" ]
                    , Progress.view Progress.defaultSpec
                    ]
                , cardContent [] [ startingStepActions ]
                ]
            ]
        ]

    -- NOTE: Because the cards container is a flex element with a max width property, when there are more cards than
    --       can fit the view, the browser will try shrink them and remove any extra right margin/padding (not sure
    --       why this is only done to the right side). To avoid our cards getting shrinked, we add the flex-shrink-0
    --       property, and we add an empty `div` that occupies space (aka also cant be shrinked) to allow that the
    --       first and last card can be scrolled to the center
    paddingElement =
      [ div
          [ classNames
              [ "flex-shrink-0"
              , "-ml-3"
              , "w-carousel-padding-element"
              , "h-full"
              ]
          ]
          []
      ]
  in
    div
      [ classNames
          [ "flex"
          , "flex-col"
          , "items-center"
          , "pt-3"
          , "h-full"
          , "w-screen"
          , "relative"
          ]
      ]
      [ lifeCycleSlot "carousel-lifecycle" case _ of
          OnInit -> CarouselOpened
          OnFinalize -> CarouselClosed
      -- NOTE: The card is allowed to grow in an h-full container and the navigation buttons are absolute positioned
      --       because the cards x-scrolling can't coexist with a visible y-overflow. To avoid clipping the cards shadow
      --       we need the cards container to grow (hence the flex-grow).
      , div [ classNames [ "flex-grow", "w-full" ] ]
          [ div
              [ classNames
                  [ "flex"
                  , "items-center"
                  , "overflow-x-scroll"
                  , "h-full"
                  , "scrollbar-width-none"
                  , "relative"
                  ]
              , ref scrollContainerRef
              ]
              $ paddingElement
                  <> cards
                  <> paddingElement
          ]
      , cardNavigationButtons state.contract
      , div [ classNames [ "self-end", "pb-4", "pr-4", "font-bold" ] ]
          [ text $ statusIndicatorMessage state.contract ]
      ]

-------------------------------------------------------------------------------
-- UI components
-------------------------------------------------------------------------------

statusIndicatorMessage :: ContractState -> String
statusIndicatorMessage (Starting _) = "Starting contract…"

statusIndicatorMessage (Started state) =
  let
    userParties = state ^. _userParties

    participantsWithAction = Set.fromFoldable $ map fst $ state ^. _namedActions

    executionState = state ^. _executionState
  in
    if isClosed executionState then
      "Contract completed"
    else if Set.isEmpty (Set.intersection userParties participantsWithAction) then
      "Waiting for "
        <>
          if Set.size participantsWithAction > 1 then
            "multiple participants"
          else case Set.findMin participantsWithAction of
            Just (Role roleName) -> roleName
            Just (PK pubKey) -> take 4 pubKey
            Nothing -> " a timeout"
    else
      "Your turn…"

cardNavigationButtons
  :: forall m. MonadAff m => ContractState -> ComponentHTML Action ChildSlots m
cardNavigationButtons (Starting _) = div [] []

cardNavigationButtons (Started state) =
  let
    lastStep = currentStep state

    selectedStep = state ^. _selectedStep

    hasLeftStep = selectedStep > 0

    hasRightStep = selectedStep < lastStep

    buttonClasses isActive = [ "leading-none", "text-xl" ] <> applyWhen
      (not isActive)
      [ "text-darkgray", "cursor-none" ]

    leftButton =
      [ button
          ( compact
              [ Just $ classNames $ buttonClasses hasLeftStep
              , if hasLeftStep then Just $ onClick $ const $ MoveToStep $
                  selectedStep - 1
                else Nothing
              , Just $ enabled hasLeftStep
              , Just $ id "previousStepButton"
              ]
          )
          [ icon_ Icon.ArrowLeft ]
      , tooltip "Previous step" (RefId "previousStepButton") Bottom
      ]

    rightButton =
      [ button
          ( compact
              [ Just $ classNames $ buttonClasses hasRightStep
              , if hasRightStep then Just $ onClick $ const $ MoveToStep $
                  selectedStep + 1
                else Nothing
              , Just $ enabled hasRightStep
              , Just $ id "nextStepButton"
              ]
          )
          [ icon_ Icon.ArrowRight ]
      , tooltip "Next step" (RefId "nextStepButton") Bottom
      ]
  in
    div [ classNames [ "flex-grow" ] ]
      [ div
          [ classNames
              [ "flex"
              , "items-center"
              , "px-6"
              , "py-2"
              , "bg-white"
              , "rounded"
              , "shadow"
              ]
          ]
          $ leftButton
              <> [ icon Icon.Info [ "px-4", "invisible" ] ]
              <> rightButton
      ]

renderPastStep
  :: forall m
   . MonadAff m
  => Minutes
  -> StartedState
  -> Int
  -> PreviousStep
  -> Array (ComponentHTML Action ChildSlots m)
renderPastStep tzOffset state stepNumber step =
  [ tabBar step.tab $ Just (SelectTab stepNumber)
  , cardBody []
      $
        [ titleBar []
            [ numberedStepTitle stepNumber
            , case step.state of
                TimeoutStep _ ->
                  stepStatus []
                    [ stepStatusText "Timed out" []
                    , icon Icon.Timer []
                    ]
                TransactionStep _ ->
                  stepStatus []
                    [ icon Icon.Done [ "text-green" ]
                    , stepStatusText "Completed" [ "text-green" ]
                    ]
            ]
        , case step.tab, step of
            Tasks, { state: TransactionStep txInput } -> cardContent [ "p-4" ]
              [ renderPastStepTasksTab tzOffset stepNumber state txInput step ]
            Tasks, { state: TimeoutStep timeoutInfo } -> cardContent [ "p-4" ]
              [ renderTimeout tzOffset state stepNumber timeoutInfo ]
            Balances, { balances } -> cardContent []
              [ renderBalances stepNumber state balances ]
        ]
  ]

type InputsByParty
  = { inputs :: Array S.Input, interval :: SlotInterval, party :: Party }

-- Normally we would expect that a TransactionInput has either no inputs or a single one
-- but the types allows for them to be a list of different inputs, possibly made by different
-- parties. If there are multiple inputs we group them by participant.
groupTransactionInputByParticipant :: TransactionInput -> Array InputsByParty
groupTransactionInputByParticipant (TransactionInput { inputs, interval }) =
  Array.fromFoldable inputs
    # Array.mapMaybe
        ( \input -> getParty input <#> (\party -> { inputs: [ input ], party })
        )
    # Array.groupBy sameParty
    # map mergeInputsFromSameParty
  where
  sameParty a b = a.party == b.party

  mergeInputsFromSameParty
    :: NonEmptyArray { inputs :: Array S.Input, party :: Party }
    -> InputsByParty
  mergeInputsFromSameParty elements =
    foldr
      (\elem accu -> accu { inputs = elem.inputs <> accu.inputs })
      ( NonEmptyArray.head elements # \{ party } ->
          { inputs: [], party, interval }
      )
      elements

renderPastStepTasksTab
  :: forall m
   . MonadAff m
  => Minutes
  -> Int
  -> StartedState
  -> TransactionInput
  -> PreviousStep
  -> ComponentHTML Action ChildSlots m
renderPastStepTasksTab tzOffset stepNumber state txInput step =
  let
    actionsByParticipant = groupTransactionInputByParticipant txInput

    resultingPayments = step ^. _resultingPayments
  in
    div [ classNames [ "space-y-4" ] ]
      if Array.length actionsByParticipant == 0 then
        -- TODO: See if we can reach this state and what text describes it better.
        [ div [ classNames [ "bg-white", "rounded", "shadow-sm" ] ]
            [ text "An empty transaction was made to advance this step" ]
        ]
      else
        append
          (renderPartyPastActions tzOffset state <$> actionsByParticipant)
          if length resultingPayments == 0 then
            []
          else
            [ renderPaymentSummary stepNumber state step ]

renderPaymentSummary
  :: forall p. Int -> StartedState -> PreviousStep -> HTML p Action
renderPaymentSummary stepNumber state step =
  let
    expanded = step ^. _expandPayments

    transfers = paymentToTransfer state <$> step ^. _resultingPayments

    expandIcon = if expanded then Icon.ExpandLess else Icon.ExpandMore
  in
    div
      [ classNames
          [ "bg-white", "rounded", "shadow-sm", "divide-y", "divide-gray" ]
      ]
      ( append
          [ a
              [ classNames
                  [ "px-4"
                  , "py-2"
                  , "flex"
                  , "justify-between"
                  , "items-center"
                  , "text-xs"
                  , "font-semibold"
                  , "select-none"
                  ]
              , onClick_ $ ToggleExpandPayment stepNumber
              ]
              [ text "Payment summary"
              , icon_ expandIcon
              ]
          ]
          if not expanded then
            []
          else
            [ div [ classNames [ "px-4" ] ]
                $
                  div [ classNames [ "py-4" ] ]
                    <<< pure
                    <<< transfer
                    <$> transfers
            ]
      )

renderPartyPastActions
  :: forall m action
   . MonadAff m
  => Minutes
  -> StartedState
  -> InputsByParty
  -> ComponentHTML action ChildSlots m
renderPartyPastActions tzOffset state { inputs, interval, party } =
  let
    -- We don't know exactly when a transaction was executed, we have an interval. But
    -- the design asks for an exact date so we use the lower end of the interval so that
    -- we don't show a value in the future
    (SlotInterval intervalFrom _) = interval

    mTransactionDateTime = posixTimeToDateTime intervalFrom

    transactionDate = formatDate tzOffset mTransactionDateTime

    transactionTime = formatTime tzOffset mTransactionDateTime

    renderPartyHeader =
      div [ classNames [ "flex", "justify-between", "items-center", "p-4" ] ]
        [ renderParty state party
        , div
            [ classNames
                [ "flex", "flex-col", "items-end", "text-xxs", "font-semibold" ]
            ]
            [ span_ [ text transactionDate ]
            , span_
                [ text $ transactionTime <> " (" <> humanizeOffset tzOffset <>
                    ")"
                ]
            ]
        ]

    renderFeesSummary =
      div [ classNames [ "p-4", "flex", "items-center", "text-xs", "gap-1" ] ]
        [ icon Icon.Language [ "text-lightpurple", "text-lg" ]
        , span [ classNames [ "font-semibold" ] ]
            [ text "Total fees:"
            ]
        , span [ classNames [ "flex-grow", "text-right" ] ]
            [ text $ humanizeValue adaToken transactionFee ]
        ]

    renderActionsBody =
      div [ classNames [ "p-4", "space-y-4" ] ]
        (map renderPastAction inputs)

    renderPastAction = case _ of
      S.IDeposit recipient sender token quantity ->
        transfer
          { sender: partyToParticipant state sender
          , recipient: partyToParticipant state recipient
          , token
          , quantity
          , termini: WalletToAccount sender recipient
          }
      S.IChoice (ChoiceId choiceIdKey _) chosenNum ->
        div []
          [ h4_ [ text "Chose:" ]
          -- NOTE: It would be neat if we could use the same trick that we use in renderAction.MakeChoice
          --       to detect if this is a single option or not, but we don't have the bounds of the choice
          --       available here. We could later add a way to get that from the contract or rethink the
          --       different constructs of When to properly include a single option and an enum.
          , span_ [ text $ BigInt.toString chosenNum <> " for " ]
          , span [ classNames [ "font-semibold" ] ] [ text choiceIdKey ]
          ]
      _ -> div_ []
  in
    div
      [ classNames
          [ "bg-white", "rounded", "shadow-sm", "divide-y", "divide-gray" ]
      ]
      [ renderPartyHeader
      , renderActionsBody
      , renderFeesSummary
      ]

renderTimeout
  :: forall p a. Minutes -> StartedState -> Int -> TimeoutInfo -> HTML p a
renderTimeout tzOffset state _ timeoutInfo =
  let
    mTimeoutDateTime = slotToDateTime timeoutInfo.slot

    timeoutDate = maybe "-" (formatDate tzOffset) mTimeoutDateTime

    timeoutTime = maybe "-" (formatTime tzOffset) mTimeoutDateTime

    header =
      div
        [ classNames [ "p-2", "gap-2", "w-full", "flex", "justify-between" ] ]
        [ div [ classNames [ "flex", "items-center", "text-xs", "gap-1" ] ]
            [ icon_ Icon.Timer
            , span [ classNames [ "font-semibold" ] ] [ text "Timed out" ]
            ]
        , div
            [ classNames
                [ "flex", "flex-col", "items-end", "text-xxs", "font-semibold" ]
            ]
            [ span_ [ text timeoutDate ]
            , span_
                [ text $ timeoutTime <> " (" <> humanizeOffset tzOffset <> ")" ]
            ]
        ]

    body =
      div [ classNames [ "p-2", "space-y-2" ] ]
        $ renderMissingActions state timeoutInfo
  in
    div
      [ classNames
          [ "divide-y"
          , "divide-gray"
          , "bg-white"
          , "rounded"
          , "shadow-sm"
          , "flex"
          , "flex-col"
          , "items-center"
          ]
      ]
      [ header
      , body
      ]

renderMissingActions
  :: forall p a. StartedState -> TimeoutInfo -> Array (HTML p a)
renderMissingActions _ { missedActions: [] } =
  [ div [ classNames [ "font-semibold", "text-xs", "leading-none" ] ]
      [ text
          "There were no tasks to complete at this step and the contract has timeouted as expected."
      ]
  ]

renderMissingActions state { missedActions } =
  append
    [ div [ classNames [ "font-semibold", "text-xs", "leading-none" ] ]
        [ text "The step timed out before the following actions could be made."
        ]
    ]
    (missedActions <#> uncurry (renderPartyMissingActions state))

renderPartyMissingActions
  :: forall p a. StartedState -> Party -> Array NamedAction -> HTML p a
renderPartyMissingActions state party actions =
  let
    renderMissingAction (MakeChoice (ChoiceId name _) _ _) = span_
      [ text "Make a choice for "
      , span [ classNames [ "font-semibold" ] ] [ text name ]
      ]

    renderMissingAction (MakeDeposit _ _ token value) = span []
      [ text $ "Make a deposit of " <> humanizeValue token value ]

    renderMissingAction (MakeNotify _) = span [] [ text "awaiting observation" ]

    renderMissingAction _ = span [] [ text "invalid action" ]

    actionsSeparatedByOr =
      intercalate
        [ div [ classNames [ "font-semibold", "text-xs" ] ] [ text "or" ]
        ]
        (Array.singleton <<< renderMissingAction <$> actions)
  in
    div [ classNames [ "border-l-2", "border-black", "pl-2", "space-y-2" ] ]
      $ Array.cons (renderParty state party) actionsSeparatedByOr

renderCurrentStep
  :: forall m
   . MonadAff m
  => Slot
  -> StartedState
  -> Array (ComponentHTML Action ChildSlots m)
renderCurrentStep currentSlot state =
  let
    stepNumber = currentStep state

    executionState = state ^. _executionState
    isPendingTransaction = isJust executionState.mPendingTransaction
    contractIsClosed = isClosed executionState
  in
    [ tabBar state.tab $ Just (SelectTab stepNumber)
    , cardBody []
        $
          [ titleBar []
              [ numberedStepTitle stepNumber
              , stepStatus []
                  $ case contractIsClosed, isPendingTransaction of
                      true, _ -> [ stepStatusText "Contract closed" [] ]
                      _, true -> [ stepStatusText "Awaiting confirmation" [] ]
                      _, _ ->
                        [ stepStatusText
                            (timeoutString currentSlot executionState)
                            []
                        , icon Icon.Timer []
                        ]
              ]
          , case state.tab of
              Tasks -> cardContent [ "bg-wite", "p-4" ]
                [ currentStepActions state ]
              Balances ->
                let
                  participants = state ^. _participants

                  balancesAtStart = state ^.
                    (_executionState <<< _semanticState <<< _accounts)

                  atStart = expandBalances
                    (Set.toUnfoldable $ Map.keys participants)
                    [ adaToken ]
                    balancesAtStart

                  atEnd =
                    if isPendingTransaction then Just atStart
                    else Nothing
                in
                  cardContent [ "bg-gray" ]
                    [ renderBalances stepNumber state { atStart, atEnd } ]
          ]
    ]

accountIndicator
  :: forall p a
   . { colorStyles :: Array String
     , otherStyles :: Array String
     , name :: String
     }
  -> HTML p a
accountIndicator { colorStyles, otherStyles, name } =
  div [ classNames $ [ "relative" ] <> otherStyles ]
    [ firstLetterInCircle { styles: colorStyles, name }
    , icon Icon.ReadMore
        [ "absolute"
        , "text-xs"
        , "-top-1"
        , "right-0"
        , "bg-white"
        , "rounded-full"
        ]
    ]

renderBalances
  :: forall m a
   . MonadAff m
  => Int
  -> StartedState
  -> StepBalance
  -> ComponentHTML a ChildSlots m
renderBalances stepNumber state stepBalance =
  let
    -- TODO: Right now we only have one type of Token (ada), but when we support multiple tokens we may want to group by
    --       participant and show the different tokens for each participant.
    -- NOTE: This transformation assumes that the balances at start and at end (if available) are
    --       expanded to all participants, because intersectionWith only produces values for duplicated
    --       keys. If a key is in one of the map but not the other, it won't be shown.
    accounts' :: Array
      (Tuple (Tuple Party Token) { atStart :: BigInt, atEnd :: Maybe BigInt })
    accounts' =
      Map.toUnfoldable
        $ maybe'
            ( \_ -> stepBalance.atStart <#> \balance ->
                { atStart: balance, atEnd: Nothing }
            )
            ( \balancesAtEnd ->
                Map.intersectionWith
                  (\atStart atEnd -> { atStart, atEnd: Just atEnd })
                  stepBalance.atStart
                  balancesAtEnd
            )
            stepBalance.atEnd
  in
    div [ classNames [ "text-xs", "space-y-4" ] ]
      [ div
          [ classNames
              [ "bg-white", "py-1", "px-4", "gap-1", "flex", "items-center" ]
          ]
          [ icon_ Icon.ReadMore
          , span [ classNames [ "font-semibold" ] ]
              [ text "Balances of contract accounts" ]
          , hint [ "relative", "-top-1" ] ("balances-" <> show stepNumber) Auto
              -- FIXME: Revisit copy

              $ div_
                  [ text
                      "This tab shows the amount of tokens that each role has in the contract account. A deposit is needed to get tokens from a wallet into the contract account, and a payment is needed to redeem tokens from the contract account into a user's wallet"
                  ]
          ]
      , div [ classNames [ "px-4", "space-y-3" ] ]
          ( accounts'
              <#>
                ( \((party /\ token) /\ balance) ->
                    let
                      participantName = participantWithNickname state party
                    in
                      div
                        [ classNames
                            [ "grid"
                            , "grid-cols-2"
                            , "gap-y-1"
                            , "py-3"
                            , "px-4"
                            , "shadow-sm"
                            , "bg-white"
                            , "rounded"
                            ]
                        ]
                        ( append
                            [ div [ classNames [ "flex", "gap-1" ] ]
                                [ accountIndicator
                                    { colorStyles:
                                        [ "bg-gradient-to-r"
                                        , "from-purple"
                                        , "to-lightpurple"
                                        , "text-white"
                                        ]
                                    , otherStyles:
                                        []
                                    , name: participantName
                                    }
                                , span [ classNames [ "font-semibold" ] ]
                                    [ text participantName ]
                                ]
                            , div
                                [ classNames
                                    [ "font-semibold", "justify-self-end" ]
                                ]
                                [ text "Balance:" ]
                            , div [ classNames [] ] [ text "Step start:" ]
                            , div [ classNames [ "justify-self-end" ] ]
                                [ text $ humanizeValue token balance.atStart ]
                            ]
                            $ maybe []
                                ( \balanceAtEnd ->
                                    [ div [ classNames [] ] [ text "Step end:" ]
                                    , div [ classNames [ "justify-self-end" ] ]
                                        [ text $ humanizeValue token
                                            balanceAtEnd
                                        ]
                                    ]
                                )
                                balance.atEnd
                        )
                )
          )
      ]

getParty :: S.Input -> Maybe Party
getParty (S.IDeposit _ p _ _) = Just p

getParty (S.IChoice (ChoiceId _ p) _) = Just p

getParty _ = Nothing

-------------------------------------------------------------------------------
-- UI Layout helpers
-------------------------------------------------------------------------------
withBaseCss
  :: forall i r p action
   . ( Array (IProp (class :: String | r) i)
       -> Array (HTML p action)
       -> HTML p action
     )
  -> Array String
  -> Array String
  -> Array (HTML p action)
  -> HTML p action
withBaseCss el baseCss css children = el [ classNames $ baseCss <> css ]
  children

card :: forall p action. Array String -> Array (HTML p action) -> HTML p action
card =
  div
    `withBaseCss`
      [ "grid"
      , "grid-rows-auto-1fr"
      , "rounded"
      , "overflow-hidden"
      , "flex-shrink-0"
      , "w-contract-card"
      , "h-contract-card"
      , "transform"
      , "transition-transform"
      , "duration-100"
      , "ease-out"
      ]

tabBar :: forall p action. Tab -> Maybe (Tab -> action) -> HTML p action
tabBar current onClick =
  let
    css = [ "flex", "select-none", "rounded-t", "overflow-hidden" ]

    tab tab' =
      let
        bgColor
          | current == tab' = "bg-white"
          | otherwise = "bg-gray"

        title = case tab' of
          Tasks -> "Tasks"
          Balances -> "Balances"
      in
        a
          ( [ classNames
                [ "flex-grow"
                , "text-center"
                , "py-2"
                , "text-sm"
                , "font-semibold"
                , bgColor
                ]
            ]
              <> fromFoldable (onClick_ <$> (onClick <*> pure tab'))
          )
          [ span_ $ [ text title ] ]
  in
    div [ classNames css ] [ tab Tasks, tab Balances ]

cardBody
  :: forall p action. Array String -> Array (HTML p action) -> HTML p action
cardBody = div `withBaseCss`
  [ "bg-white", "grid", "grid-rows-auto-1fr", "divide-y", "divide-lightgray" ]

titleBar
  :: forall p action. Array String -> Array (HTML p action) -> HTML p action
titleBar = div `withBaseCss` [ "py-2.5", "px-4", "flex", "items-center" ]

cardContent
  :: forall p action. Array String -> Array (HTML p action) -> HTML p action
cardContent = div `withBaseCss` [ "overflow-y-auto" ]

numberedStepTitle :: forall p action. Int -> HTML p action
numberedStepTitle stepNumber = stepTitle $ "Step " <> show (stepNumber + 1)

stepTitle :: forall p action. String -> HTML p action
stepTitle s =
  span
    [ classNames [ "text-xl", "font-semibold", "flex-grow" ] ]
    [ text s ]

stepStatus
  :: forall p action. Array String -> Array (HTML p action) -> HTML p action
stepStatus = div `withBaseCss` [ "h-10", "flex", "items-center", "gap-2" ]

stepStatusText :: forall p action. String -> Array String -> HTML p action
stepStatusText message css =
  span
    [ classNames $ [ "select-none", "font-semibold" ] <> css ]
    [ text message ]

