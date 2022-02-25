module Page.Contract.State
  ( component
  , handleAction
  , mkPlaceholderState
  ) where

import Prologue

import Capability.Marlowe (class ManageMarlowe)
import Capability.MarloweStorage
  ( class ManageMarloweStorage
  , modifyContractNicknames
  )
import Capability.Toast (class Toast)
import Component.Contacts.State (adaToken)
import Control.Monad.Reader (class MonadAsk, asks)
import Data.Array (index, length, mapMaybe, modifyAt)
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as ContractNickname
import Data.ContractUserParties (contractUserParties, getParticipants)
import Data.Foldable (for_)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Lens (assign, modifying, set, to, toArrayOf, traversed, (^.))
import Data.Lens.Extra (peruse)
import Data.Lens.Index (ix)
import Data.List (toUnfoldable)
import Data.LocalContractNicknames (insertContractNickname)
import Data.Maybe (fromMaybe)
import Data.Ord (abs)
import Data.PABConnectedWallet (PABConnectedWallet)
import Data.Set as Set
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\))
import Data.UserNamedActions (userNamedActions)
import Data.UserNamedActions as UserNamedActions
import Effect (Effect)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Env (Env(..))
import Halogen (HalogenM, raise)
import Halogen as H
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Halogen.Store.Select (selectEq)
import Halogen.Subscription as HS
import Marlowe.Execution.Lenses (_history, _pendingTimeouts, _semanticState)
import Marlowe.Execution.State (expandBalances, extractNamedActions)
import Marlowe.Execution.Types (PastAction(..))
import Marlowe.Execution.Types (PastState, State, TimeoutInfo) as Execution
import Marlowe.Extended.Metadata (MetaData, emptyContractMetadata)
import Marlowe.Semantics (Slot, _accounts)
import Page.Contract.Lenses
  ( _Started
  , _contract
  , _contractUserParties
  , _executionState
  , _expandPayments
  , _previousSteps
  , _selectedStep
  )
import Page.Contract.Types
  ( Action(..)
  , Context
  , ContractState(..)
  , DSL
  , Input
  , Msg(..)
  , PreviousStep
  , PreviousStepState(..)
  , StartedState
  , State
  , Tab(..)
  , scrollContainerRef
  )
import Page.Contract.View (contractScreen)
import Store as Store
import Store.Contracts (getContract)
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
  => ManageMarlowe m
  => ManageMarloweStorage m
  => Toast m
  => MonadStore Store.Action Store.Store m
  => H.Component query Input Msg m
component =
  connect (selectEq \{ contracts, currentSlot } -> { contracts, currentSlot }) $
    H.mkComponent
      { initialState: deriveState
      , render: contractScreen
      , eval: H.mkEval H.defaultEval
          { handleAction = handleAction
          , receive = Just <<< Receive
          , initialize = Just Init
          }
      }

dummyState :: ContractState
dummyState =
  Starting
    { nickname: ContractNickname.unknown
    , metadata: emptyContractMetadata
    }

deriveState :: Connected Context Input -> State
deriveState
  { context
  , input:
      { tzOffset
      , wallet
      , marloweParams
      }
  } =
  let
    mExecutionState = getContract marloweParams context.contracts
    -- FIXME-3487 we might want to represent an error state instead of dummyState
    contract = fromMaybe dummyState $ mkInitialState wallet context.currentSlot
      <$> mExecutionState
  in
    { contract
    , currentSlot: context.currentSlot
    , tzOffset
    , wallet
    }

-- TODO: SCP-3487 Fix the flow that creates a contract
-- this is for making a placeholder state for the user who created the contract, used for displaying
-- something before we get the MarloweParams back from the WalletCompanion app
mkPlaceholderState :: ContractNickname -> MetaData -> ContractState
mkPlaceholderState nickname metaData =
  Starting
    { nickname
    , metadata: metaData
    }

mkInitialState
  :: PABConnectedWallet
  -> Slot
  -> Execution.State
  -> ContractState
mkInitialState wallet currentSlot executionState =
  let
    { marloweParams, contract } = executionState
    initialState =
      { tab: Tasks
      , executionState
      , previousSteps: mempty
      , selectedStep: 0
      -- FIXME-3208: Check, because I think the contract in the executionState is the current
      --             continuation and not the initial contract, so getParticipants might be wrong
      , contractUserParties: contractUserParties wallet marloweParams contract
      , namedActions: UserNamedActions.empty
      }
  in
    initialState
      # regenerateStepCards currentSlot
      # selectLastStep
      # Started

withStarted
  :: forall action slots msg m
   . Monad m
  => (StartedState -> HalogenM State action slots msg m Unit)
  -> HalogenM State action slots msg m Unit
withStarted f = peruse (_contract <<< _Started) >>= traverse_ f

handleAction
  :: forall m
   . MonadAff m
  => MonadAsk Env m
  => ManageMarlowe m
  => ManageMarloweStorage m
  => MonadStore Store.Action Store.Store m
  => Toast m
  => Action
  -> DSL m Unit
handleAction Init = do
  selectedStep <- peruse $ _contract <<< _Started <<< _selectedStep
  mElement <- H.getHTMLElementRef scrollContainerRef
  for_ (Tuple <$> mElement <*> selectedStep) \(elm /\ step) -> do
    -- When the carousel is opened we want to assure that the selected step is
    -- in the center without any animation
    liftEffect $ scrollStepToCenter Auto step elm
    subscribeToSelectCenteredStep
handleAction (Receive { input, context }) = do
  let
    mExecutionState = getContract input.marloweParams context.contracts
  mStartedContract <- peruse (_contract <<< _Started)
  case mExecutionState, mStartedContract of
    Just executionState, Just startedContract
      | executionState /= startedContract.executionState ->
          let
            newContractState =
              startedContract
                # set _executionState executionState
                # regenerateStepCards context.currentSlot
                # Started
          in
            assign _contract newContractState
    _, _ -> pure unit
handleAction (SetNickname nickname) =
  withStarted \{ executionState: { marloweParams } } -> do
    void $ modifyContractNicknames $ insertContractNickname marloweParams
      nickname

handleAction (SelectTab stepNumber tab) =
  modifying (_contract <<< _Started) \started@{ previousSteps } ->
    case
      modifyAt stepNumber (\previousStep -> previousStep { tab = tab })
        previousSteps
      of
      -- if the stepNumber is in the range of the previousSteps, we update that step
      Just modifiedPreviousSteps -> started
        { previousSteps = modifiedPreviousSteps }
      -- otherwise we update the tab of the current step
      Nothing -> started { tab = tab }

handleAction (ToggleExpandPayment stepNumber) = modifying
  ( _contract <<< _Started <<< _previousSteps <<< ix stepNumber <<<
      _expandPayments
  )
  not

handleAction (OnActionSelected action) = raise $ AskConfirmation action

handleAction CancelConfirmation = pure unit -- Managed by Dashboard.State

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

transactionsToStep :: StartedState -> Execution.PastState -> PreviousStep
transactionsToStep
  state
  { balancesAtStart, balancesAtEnd, txInput, resultingPayments, action } =
  let
    participants = getParticipants $ state ^. _contractUserParties

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
          contractUserParties = state ^. _contractUserParties

          missedActions =
            userNamedActions
              contractUserParties
              act.missedActions
        in
          TimeoutStep { slot: act.slot, missedActions }
      InputAction -> TransactionStep txInput
  in
    { tab: Tasks
    , expandPayments: false
    , resultingPayments: toUnfoldable resultingPayments
    , balances:
        { atStart:
            expandedBalancesAtStart
        , atEnd: Just expandedBalancesAtEnd
        }
    , state: stepState
    }

timeoutToStep :: StartedState -> Execution.TimeoutInfo -> PreviousStep
timeoutToStep state { slot, missedActions } =
  let
    balances = state ^. (_executionState <<< _semanticState <<< _accounts)

    contractUserParties = state ^. _contractUserParties

    participants = getParticipants contractUserParties

    expandedBalances = expandBalances (Set.toUnfoldable participants)
      [ adaToken ]
      balances
  in
    { tab: Tasks
    , expandPayments: false
    -- FIXME: Revisit how should we treat payments from timeout steps, for now they are not displayed
    , resultingPayments: []
    , balances:
        { atStart: expandedBalances
        , atEnd: Nothing
        }
    , state:
        TimeoutStep
          { slot
          , missedActions:
              userNamedActions
                contractUserParties
                missedActions
          }
    }

regenerateStepCards :: Slot -> StartedState -> StartedState
regenerateStepCards currentSlot state =
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
          (transactionsToStep state)
      )
      state

    pendingTimeoutSteps :: Array PreviousStep
    pendingTimeoutSteps = toArrayOf
      ( _executionState <<< _pendingTimeouts <<< traversed <<< to
          (timeoutToStep state)
      )
      state

    previousSteps = confirmedSteps <> pendingTimeoutSteps

    executionState = state ^. _executionState

    contractUserParties = state ^. _contractUserParties

    namedActions =
      userNamedActions contractUserParties $ extractNamedActions currentSlot
        executionState
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
