module Page.Contract.State (component) where

import Prologue

import Capability.Marlowe (class ManageMarlowe)
import Capability.Toast (class Toast)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Now (class MonadTime, timezoneOffset)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.State.Class (modify_)
import Data.Array (index, length, mapMaybe)
import Data.ContractNickname as ContractNickname
import Data.ContractStatus (ContractStatus(..))
import Data.DateTime.Instant (Instant)
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Lens (assign, modifying, set, to, toArrayOf, traversed, use, (^.))
import Data.Lens.Extra (peruse)
import Data.List (toUnfoldable)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.NewContract (NewContract(..))
import Data.Ord (abs)
import Data.Set as Set
import Data.Time.Duration (Minutes(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut (emptyUUID)
import Data.UserNamedActions (userNamedActions)
import Data.UserNamedActions as UserNamedActions
import Effect (Effect)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Env (Env(..))
import Halogen (raise)
import Halogen as H
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as HS
import Language.Marlowe.Core.V1.Semantics.Types
  ( Contract(..)
  , _accounts
  , adaToken
  )
import Language.Marlowe.Extended.V1.Metadata (emptyContractMetadata)
import Marlowe.Execution.Lenses (_history, _pendingTimeouts, _semanticState)
import Marlowe.Execution.State (expandBalances, extractNamedActions)
import Marlowe.Execution.Types (PastAction(..))
import Marlowe.Execution.Types (PastState, State, TimeoutInfo) as Execution
import Marlowe.HasParties (getParties)
import Page.Contract.Lenses
  ( _Started
  , _contract
  , _executionState
  , _expandPayments
  , _selectedStep
  , _tab
  )
import Page.Contract.Types
  ( Action(..)
  , ContractState
  , DSL
  , Input
  , Msg(..)
  , PreviousStep
  , PreviousStepState(..)
  , Slice
  , StartedState
  , State
  , scrollContainerRef
  )
import Page.Contract.View (contractScreen)
import Store as Store
import Store.Contracts (getContract, getNewContract)
import Store.RoleTokens (RoleTokenStore)
import Web.DOM.Element (getElementsByClassName)
import Web.DOM.HTMLCollection as HTMLCollection
import Web.Dom.ElementExtra
  ( Alignment(..)
  , ScrollBehavior(..)
  , debouncedOnScroll
  , scrollIntoView
  , throttledOnScroll
  )
import Web.HTML (HTMLElement)
import Web.HTML.HTMLElement (getBoundingClientRect, offsetLeft)
import Web.HTML.HTMLElement as HTMLElement

component
  :: forall query m
   . MonadAff m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Msg m
component =
  connect
    ( selectEq \{ roleTokens, contracts, currentTime } ->
        { roleTokens, contracts, currentTime }
    ) $
    H.mkComponent
      { initialState: deriveState
      , render: contractScreen
      , eval: H.mkEval H.defaultEval
          { handleAction = handleAction
          , receive = Just <<< Receive
          , initialize = Just Init
          , finalize = Just Finalize
          }
      }

dummyState :: ContractState
dummyState = Starting $ NewContract
  emptyUUID
  ContractNickname.unknown
  emptyContractMetadata
  Nothing
  Close

deriveState :: Connected Slice Input -> State
deriveState
  { context
  , input: { contractIndex }
  } =
  let
    mContract = case contractIndex of
      Starting reqId -> Starting <$> getNewContract reqId context.contracts
      Started marloweParams ->
        mkInitialState context.currentTime context.roleTokens
          <$> getContract marloweParams context.contracts

    -- TODO: We might want to represent an error state instead of dummyState
    contract = fromMaybe dummyState $ mContract
  in
    { contract
    , currentTime: context.currentTime
    , tzOffset: Minutes 0.0 -- This will be set in Init
    , roleTokens: context.roleTokens
    }

mkInitialState
  :: Instant
  -> RoleTokenStore
  -> Execution.State
  -> ContractState
mkInitialState currentTime roleTokens executionState =
  let
    { initialContract } = executionState
    initialState =
      { tabs: Map.empty
      , expandPayments: Map.empty
      , executionState
      , previousSteps: mempty
      , selectedStep: 0
      , namedActions: UserNamedActions.empty
      , participants: getParties initialContract
      }
  in
    initialState
      # regenerateStepCards roleTokens currentTime
      # selectLastStep
      # Started

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => MonadTime m
  => ManageMarlowe m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => Action
  -> DSL m Unit
handleAction Init = do
  tzOffset <- timezoneOffset
  modify_ _ { tzOffset = tzOffset }
  selectedStep <- peruse $ _contract <<< _Started <<< _selectedStep
  mElement <- H.getHTMLElementRef scrollContainerRef
  for_ (Tuple <$> mElement <*> selectedStep) \(elm /\ step) -> do
    -- When the carousel is opened we want to assure that the selected step is
    -- in the center without any animation
    liftEffect $ scrollStepToCenter Auto step elm
    subscribeToSelectCenteredStep

handleAction Finalize = unsubscribeFromSelectCenteredStep

handleAction (Receive { input, context }) = do
  modify_ _ { currentTime = context.currentTime }
  case input.contractIndex of
    Started marloweParams -> void $ runMaybeT do
      executionState <- hoistMaybe $ getContract marloweParams context.contracts
      currentContractState <- use _contract
      case currentContractState of
        -- If we are transitioning from a Starting to a Started contract we recreate the Contract initial
        -- state
        Starting _ -> assign _contract $ mkInitialState
          context.currentTime
          context.roleTokens
          executionState
        -- If we are just receiving a new input but we are already in a Started state, then we just
        -- update the execution state
        Started _ -> modifying (_contract <<< _Started) $
          regenerateStepCards context.roleTokens context.currentTime
            <<< set _executionState executionState
    _ -> pure unit

handleAction (SelectTab stepNumber tab) =
  assign (_contract <<< _Started <<< _tab stepNumber) tab

handleAction (ToggleExpandPayment stepNumber) = modifying
  (_contract <<< _Started <<< _expandPayments stepNumber)
  not

handleAction (OnActionSelected action num) = raise $ AskConfirmation action num

handleAction (SelectStep stepNumber) = assign
  (_contract <<< _Started <<< _selectedStep)
  stepNumber

handleAction (MoveToStep stepNumber) = do
  -- The MoveToStep action is called when a new step is added (either via an apply transaction or
  -- a timeout). We unsubscribe and resubscribe to update the tracked elements.
  unsubscribeFromSelectCenteredStep
  subscribeToSelectCenteredStep
  mElement <- H.getHTMLElementRef scrollContainerRef
  for_ mElement $ liftEffect <<< scrollStepToCenter Smooth stepNumber

transactionsToStep
  :: RoleTokenStore
  -> StartedState
  -> Execution.PastState
  -> PreviousStep
transactionsToStep
  roleTokens
  state
  { balancesAtStart, balancesAtEnd, txInput, resultingPayments, action } =
  let
    participants = state.participants

    -- TODO: When we add support for multiple tokens we should extract the possible tokens from the
    --       contract, store it in ContractState and pass them here.
    expandedBalancesAtStart = expandBalances
      (Set.toUnfoldable participants)
      [ adaToken ]
      balancesAtStart

    expandedBalancesAtEnd = expandBalances
      (Set.toUnfoldable participants)
      [ adaToken ]
      balancesAtEnd

    stepState = case action of
      TimeoutAction act ->
        let
          missedActions = userNamedActions
            roleTokens
            state.executionState
            act.missedActions
        in
          TimeoutStep { time: act.time, missedActions }
      InputAction -> TransactionStep txInput
  in
    { resultingPayments: toUnfoldable resultingPayments
    , balances:
        { atStart:
            expandedBalancesAtStart
        , atEnd: Just expandedBalancesAtEnd
        }
    , state: stepState
    }

timeoutToStep
  :: RoleTokenStore
  -> StartedState
  -> Execution.TimeoutInfo
  -> PreviousStep
timeoutToStep roleTokens state { time, missedActions } =
  let
    balances = state ^. (_executionState <<< _semanticState <<< _accounts)

    participants = state.participants

    expandedBalances = expandBalances (Set.toUnfoldable participants)
      [ adaToken ]
      balances
  in
    { -- FIXME: Revisit how should we treat payments from timeout steps, for now they are not displayed
      resultingPayments: []
    , balances:
        { atStart: expandedBalances
        , atEnd: Nothing
        }
    , state:
        TimeoutStep
          { time
          , missedActions:
              userNamedActions roleTokens state.executionState missedActions
          }
    }

regenerateStepCards :: RoleTokenStore -> Instant -> StartedState -> StartedState
regenerateStepCards roleTokens currentTime state =
  -- TODO: This regenerates all the previous step cards, resetting them to their default state (showing
  -- the Tasks tab). If any of them are showing the Balances tab, it would be nice to keep them that way.
  -- TODO: Performance optimization
  --  This function is being called for all cotracts whenever any contract change. We should be able to call it
  --  only for the changed contract. Moreover, we regenerate previous steps for the selected contract card and the
  --  summary cards in the dashboard, but only the selected contract cares about the previous steps, the dashboard
  --  only needs the current step and the step number.
  let
    confirmedSteps :: Array PreviousStep
    confirmedSteps = toArrayOf
      ( _executionState <<< _history <<< traversed <<< to
          (transactionsToStep roleTokens state)
      )
      state

    pendingTimeoutSteps :: Array PreviousStep
    pendingTimeoutSteps = toArrayOf
      ( _executionState <<< _pendingTimeouts <<< traversed <<< to
          (timeoutToStep roleTokens state)
      )
      state

    previousSteps = confirmedSteps <> pendingTimeoutSteps

    executionState = state ^. _executionState

    namedActions = userNamedActions roleTokens executionState
      $ extractNamedActions currentTime executionState
  in
    state { previousSteps = previousSteps, namedActions = namedActions }

selectLastStep :: StartedState -> StartedState
selectLastStep state@{ previousSteps } = state
  { selectedStep = length previousSteps }

------------------------------------------------------------------
-- NOTE: In the first version of the selectCenteredStep feature the subscriptionId was stored in the
--       Contract.State as a Maybe SubscriptionId. But when calling subscribe/unsubscribe multiple
--       times in a small period of time there was a concurrency issue and multiple subscriptions
--       were active at the same time, which caused scroll issues. We use an AVar to control the
--       concurrency and assure that only one subscription is active at a time.
unsubscribeFromSelectCenteredStep
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => DSL m Unit
unsubscribeFromSelectCenteredStep = do
  mutex <- asks \(Env e) -> e.contractStepCarouselSubscription
  mSubscription <- liftAff $ AVar.tryTake mutex
  for_ mSubscription H.unsubscribe

subscribeToSelectCenteredStep
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => DSL m Unit
subscribeToSelectCenteredStep = do
  mElement <- H.getHTMLElementRef scrollContainerRef
  for_ mElement \elm -> do
    subscription <- H.subscribe $ selectCenteredStepEventSource elm
    -- We try to update the subscription without blocking, and if we cant (because another
    -- subscription is already present, then we clean this one, so only one subscription can
    -- be active at a time)
    mutex <- asks \(Env e) -> e.contractStepCarouselSubscription
    mutexUpdated <- liftAff $ AVar.tryPut subscription mutex
    when (not mutexUpdated) $ H.unsubscribe subscription

scrollStepToCenter
  :: ScrollBehavior
  -> Int
  -> HTMLElement
  -> Effect Unit
scrollStepToCenter behavior stepNumber parentElement = do
  let
    getStepElemets = HTMLCollection.toArray =<< getElementsByClassName
      "w-contract-card"
      (HTMLElement.toElement parentElement)
  mStepElement <- flip index stepNumber <$> getStepElemets
  for_ mStepElement $ scrollIntoView { block: Center, inline: Center, behavior }

-- This EventSource is responsible for selecting the step closest to the center of the scroll container
-- when scrolling
selectCenteredStepEventSource :: HTMLElement -> HS.Emitter Action
selectCenteredStepEventSource scrollContainer =
  HS.makeEmitter \push -> do
    -- Calculate where the left coordinate of the center step should be
    -- (relative to the visible part of the scroll container)
    parentWidth <- _.width <$> getBoundingClientRect scrollContainer
    let
      stepCardWidth = 264.0

      intendedLeft = parentWidth / 2.0 - stepCardWidth / 2.0
    -- Calculate the left coordinate of all cards relative to the scroll container (which needs to have a
    -- display: relative property)
    stepElements <- HTMLCollection.toArray =<< getElementsByClassName
      "w-contract-card"
      (HTMLElement.toElement scrollContainer)
    stepLeftOffsets <- traverse offsetLeft $ mapMaybe HTMLElement.fromElement
      stepElements
    let
      calculateClosestStep scrollPos =
        _.index
          $ foldlWithIndex
              ( \index accu stepLeftOffset ->
                  let
                    diff = abs $ stepLeftOffset -
                      (scrollPos.left + intendedLeft)
                  in
                    if diff < accu.diff then { diff, index } else accu
              )
              { index: 0, diff: top }
              stepLeftOffsets
    -- We use two different scroll listeners:
    -- * The first one is responsible for actually selecting the step closest to the center. It is throttled,
    --   which means that it will be called at most once in every `window of time`. We do this because the
    --   scroll event dispatch several events per scroll action and the callback is handled in the main thread
    --   so if we do a heavy computation, the browser can lag.
    unsubscribeSelectEventListener <-
      throttledOnScroll
        50.0
        (HTMLElement.toElement scrollContainer)
        (push <<< SelectStep <<< calculateClosestStep)
    -- * The second one is responsible for snapping the card to the center position. Initially this was
    --   handled by CSS using the `scroll-snap-type` and `scroll-snap-align` properties. But I found a bug
    --   in chrome when those properties were used at the same time of a `smooth` scrollTo, so I ended up
    --   doing manual snapping. The event is debounced, which means that it will be called just once after
    --   X time with no scroll events.
    -- https://bugs.chromium.org/p/chromium/issues/detail?id=1195682
    unsubscribeSnapEventListener <-
      debouncedOnScroll
        150.0
        (HTMLElement.toElement scrollContainer)
        $ \scrollPos -> do
            let
              index = calculateClosestStep scrollPos
            scrollStepToCenter Smooth index scrollContainer
            push $ SelectStep index
    pure do
      unsubscribeSelectEventListener
      unsubscribeSnapEventListener
