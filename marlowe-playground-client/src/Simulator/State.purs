module Simulator.State
  ( advanceTime
  , applyInput
  , emptyExecutionStateWithTime
  , emptyMarloweState
  , getAllActions
  , hasHistory
  , initialMarloweState
  , startSimulation
  , updateChoice
  ) where

import Prologue

import Control.Alternative (guard)
import Control.Bind (bindFlipped)
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.State (class MonadState)
import Data.Array
  ( catMaybes
  , fold
  , fromFoldable
  , mapMaybe
  , snoc
  , sort
  , toUnfoldable
  , uncons
  )
import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Function (on)
import Data.Lens (modifying, over, previewOn, set, to, use, (^.))
import Data.Lens.Extra (peruse)
import Data.Lens.NonEmptyList (_Head)
import Data.List (List(..))
import Data.List as List
import Data.List.Types (NonEmptyList)
import Data.Map (Map)
import Data.Map as Map
import Data.Map.Ordered.OMap as OMap
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty ((:|))
import Data.NonEmptyList.Extra (extendWith)
import Data.NonEmptyList.Lens (_Tail)
import Data.Semigroup.Foldable (foldl1)
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\))
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Holes
  ( Contract(..)
  , Term(..)
  , TransactionOutput(..)
  , computeTransaction
  , fromTerm
  , reduceContractUntilQuiescent
  )
import Marlowe.Holes as T
import Marlowe.Semantics
  ( Action(..)
  , Bound(..)
  , ChoiceId(..)
  , ChosenNum
  , Environment(..)
  , Input
  , IntervalResult(..)
  , Observation
  , Party
  , State
  , TimeInterval(..)
  , Timeouts(..)
  , TransactionError(..)
  , TransactionInput(..)
  , _minTime
  , boundFrom
  , emptyState
  , evalValue
  , fixInterval
  , moneyInContract
  , timeouts
  )
import Marlowe.Semantics as S
import Marlowe.Template
  ( getPlaceholderIds
  , initializeTemplateContentWithIncreasingTime
  )
import Marlowe.Time (unixEpoch)
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Simulator.Lenses
  ( _SimulationRunning
  , _contract
  , _currentMarloweState
  , _executionState
  , _log
  , _marloweState
  , _moneyInContract
  , _pendingInputs
  , _possibleActions
  , _state
  , _time
  , _transactionError
  , _transactionWarnings
  )
import Simulator.Types
  ( ActionInput(..)
  , ActionInputId(..)
  , ExecutionState(..)
  , ExecutionStateRecord
  , LogEntry(..)
  , MarloweState
  , MoveToTimeType(..)
  , PartiesAction(..)
  , moveToTimePartyAction
  , otherActionsParty
  )

emptyExecutionStateWithTime :: Instant -> Term T.Contract -> ExecutionState
emptyExecutionStateWithTime time cont =
  SimulationRunning
    { possibleActions: mempty
    , pendingInputs: mempty
    , transactionError: Nothing
    , transactionWarnings: mempty
    , log: mempty
    , state: emptyState
    , time
    , moneyInContract: mempty
    , contract: cont
    }

simulationNotStarted
  :: Instant -> Term T.Contract -> MetaData -> ExecutionState
simulationNotStarted initialTime termContract metadata =
  let
    templateContent =
      initializeTemplateContentWithIncreasingTime
        initialTime
        (Minutes 5.0)
        (OMap.keys metadata.timeParameterDescriptions)
        (getPlaceholderIds termContract)

  in
    SimulationNotStarted
      { initialTime
      , termContract
      , templateContent
      }

emptyMarloweState :: MarloweState
emptyMarloweState =
  { editorErrors: mempty
  , editorWarnings: mempty
  , holes: mempty
  , executionState: Nothing
  }

initialMarloweState :: Instant -> Term T.Contract -> MetaData -> MarloweState
initialMarloweState initialTime contract metadata =
  { editorErrors: mempty
  , editorWarnings: mempty
  , holes: mempty
  , executionState: Just $ simulationNotStarted initialTime contract metadata
  }

minimumBound :: Array Bound -> ChosenNum
minimumBound bnds = case uncons (map boundFrom bnds) of
  Just { head, tail } -> foldl1 min (head :| tail)
  Nothing -> zero

actionToActionInput :: State -> Action -> Tuple ActionInputId ActionInput
actionToActionInput state (Deposit accountId party token value) =
  let
    minTime = state ^. _minTime

    evalResult = evalValue env state value

    env = Environment { timeInterval: (TimeInterval minTime minTime) }
  in
    Tuple (DepositInputId accountId party token evalResult)
      (DepositInput accountId party token evalResult)

actionToActionInput _ (Choice choiceId bounds) = Tuple (ChoiceInputId choiceId)
  (ChoiceInput choiceId bounds (minimumBound bounds))

actionToActionInput _ (Notify _) = Tuple NotifyInputId NotifyInput

combineChoices :: ActionInput -> ActionInput -> ActionInput
combineChoices
  (ChoiceInput choiceId1 bounds1 _)
  (ChoiceInput choiceId2 bounds2 _)
  | choiceId1 == choiceId2 =
      (ChoiceInput choiceId2 combinedBounds (minimumBound combinedBounds))
      where
      combinedBounds = bounds1 <> bounds2

combineChoices _ a2 = a2

simplifyActionInput :: ActionInput -> ActionInput
simplifyActionInput (ChoiceInput choiceId bounds minBound) = ChoiceInput
  choiceId
  (simplifyBounds bounds)
  minBound

simplifyActionInput other = other

simplifyBounds :: Array Bound -> Array Bound
simplifyBounds bounds = fromFoldable
  (simplifyBoundList (toUnfoldable (sort bounds)))

simplifyBoundList :: List Bound -> List Bound
simplifyBoundList (Cons (Bound low1 high1) (Cons b2@(Bound low2 high2) rest))
  | high1 >= low2 = simplifyBoundList
      (Cons (Bound (min low1 low2) (max high1 high2)) rest)
  | otherwise = (Cons (Bound low1 high1) (simplifyBoundList (Cons b2 rest)))

simplifyBoundList l = l

updatePossibleActions :: MarloweState -> MarloweState
updatePossibleActions
  oldState@{ executionState: Just (SimulationRunning executionState) } =
  let
    contract = executionState ^. _contract

    state = executionState ^. _state

    txInput = pendingTransactionInputs executionState

    (Tuple nextState actions) = extractRequiredActionsWithTxs txInput state
      contract

    usefulActions = mapMaybe removeUseless actions

    currentTime = executionState.time

    rawActionInputs = Map.fromFoldableWith combineChoices $ map
      (actionToActionInput nextState)
      usefulActions

    actionInputs = map simplifyActionInput rawActionInputs

    mNextTimeout = nextTimeout oldState
    moveTo = fold case contract of
      Term Close _ -> []
      _ ->
        catMaybes
          [ moveToTimePartyAction NextTimeout <$> mNextTimeout
          , moveToTimePartyAction <$> Just NextTime <*>
              (fromDateTime <$> (adjust (Minutes one) $ toDateTime currentTime))
          , let
              expirationTime = (unwrap <<< _.maxTime <<< unwrap <<< timeouts)
                contract
            in
              Just $ moveToTimePartyAction ExpirationTime expirationTime
          ]

    newExecutionState =
      executionState
        # over _possibleActions (updateActions actionInputs >>> append moveTo)
  in
    set _executionState (SimulationRunning newExecutionState) oldState
  where
  removeUseless :: Action -> Maybe Action
  removeUseless action@(Notify observation) =
    if evalObservation oldState observation then Just action else Nothing

  removeUseless action = Just action

  updateActions
    :: Map ActionInputId ActionInput -> PartiesAction -> PartiesAction
  updateActions actionInputs oldInputs = foldlWithIndex
    (addButPreserveActionInputs oldInputs)
    mempty
    actionInputs

  addButPreserveActionInputs
    :: PartiesAction
    -> ActionInputId
    -> PartiesAction
    -> ActionInput
    -> PartiesAction
  addButPreserveActionInputs oldInputs actionInputIdx m actionInput =
    let
      party = actionPerson actionInput
    in
      wrap $ appendValue (unwrap m) (unwrap oldInputs) party actionInputIdx
        actionInput

  actionPerson :: ActionInput -> Party
  actionPerson (DepositInput _ party _ _) = party

  actionPerson (ChoiceInput (ChoiceId _ party) _ _) = party

  -- We have a special person for notifications
  actionPerson _ = otherActionsParty

  appendValue
    :: forall k k2 v2
     . Ord k
    => Ord k2
    => Map k (Map k2 v2)
    -> Map k (Map k2 v2)
    -> k
    -> k2
    -> v2
    -> Map k (Map k2 v2)
  appendValue m oldMap k k2 v2 = Map.alter
    (alterMap k2 (findWithDefault2 v2 k k2 oldMap))
    k
    m

  alterMap :: forall k v. Ord k => k -> v -> Maybe (Map k v) -> Maybe (Map k v)
  alterMap k v Nothing = Just $ Map.singleton k v

  alterMap k v (Just vs) = Just $ Map.insert k v vs

  findWithDefault2
    :: forall k k2 v2
     . Ord k
    => Ord k2
    => v2
    -> k
    -> k2
    -> Map k (Map k2 v2)
    -> v2
  findWithDefault2 def k k2 m = case Map.lookup k m of
    Just m2 -> case Map.lookup k2 m2 of
      Just v -> v
      Nothing -> def
    Nothing -> def

updatePossibleActions oldState = oldState

extractRequiredActionsWithTxs
  :: TransactionInput -> State -> Term T.Contract -> Tuple State (Array Action)
extractRequiredActionsWithTxs txInput state contract
  | TransactionOutput { txOutContract, txOutState } <-
      computeTransaction txInput state contract = Tuple txOutState
      (extractRequiredActions txOutContract)
  | TransactionInput { inputs: Nil } <- txInput
  , IntervalTrimmed env fixState <- fixInterval (unwrap txInput).interval state
  , Just (_ /\ reducedContract) <-
      reduceContractUntilQuiescent env fixState contract = Tuple fixState
      (extractRequiredActions reducedContract)
  -- the actions remain unchanged in error cases, cases where the contract is not reduced or cases where inputs remain
  | otherwise = Tuple state (extractRequiredActions contract)

extractRequiredActions :: Term T.Contract -> Array Action
extractRequiredActions contract = case contract of
  Term (When cases _ _) _ -> map (\(S.Case action _) -> action) $ mapMaybe
    fromTerm
    cases
  _ -> mempty

applyPendingInputs :: MarloweState -> MarloweState
applyPendingInputs
  oldState@{ executionState: Just (SimulationRunning executionState) } =
  newState
  where
  txInput@(TransactionInput txIn) = pendingTransactionInputs executionState

  newState =
    case
      computeTransaction txInput (executionState ^. _state)
        (executionState ^. _contract)
      of
      TransactionOutput
        { txOutWarnings, txOutPayments, txOutState, txOutContract } ->
        let
          mContractCloseLog = case txOutContract of
            Term Close _ -> over _log
              (\logs -> logs <> [ CloseEvent txIn.interval ])
            _ -> identity

          newExecutionState =
            ( set _transactionError Nothing
                <<< over _transactionWarnings
                  (flip append $ fromFoldable txOutWarnings)
                <<< set _pendingInputs mempty
                <<< set _state txOutState
                <<< set _moneyInContract (moneyInContract txOutState)
                <<< mContractCloseLog
                <<< over _log
                  ( \logs -> logs <>
                      ( fromFoldable
                          (map (OutputEvent txIn.interval) txOutPayments)
                      )
                  )
                <<< over _log (\logs -> logs <> [ InputEvent txInput ])
            )
              executionState
        in
          set _executionState
            (SimulationRunning (set _contract txOutContract newExecutionState))
            oldState
      InvalidContract -> oldState
      SemanticError TEUselessTransaction -> oldState
      SemanticError txError ->
        let
          newExecutionState =
            ( set _transactionError (Just txError)
                -- apart from setting the error, we also removing the pending inputs

                -- otherwise there can be hidden pending inputs in the simulation

                <<< set _pendingInputs mempty
            )
              executionState
        in
          set _executionState (SimulationRunning newExecutionState) oldState

applyPendingInputs oldState = oldState

updateTime :: Instant -> MarloweState -> MarloweState
updateTime = set (_executionState <<< _SimulationRunning <<< _time)

pendingTransactionInputs :: ExecutionStateRecord -> TransactionInput
pendingTransactionInputs executionState =
  let
    time = executionState ^. _time

    interval = on TimeInterval POSIXTime time time

    inputs = executionState ^. _pendingInputs
  in
    TransactionInput { interval: interval, inputs: (List.fromFoldable inputs) }

updateMarloweState
  :: forall s m
   . MonadState { marloweState :: NonEmptyList MarloweState | s } m
  => (MarloweState -> MarloweState)
  -> m Unit
updateMarloweState f = modifying _marloweState
  (extendWith (updatePossibleActions <<< f))

applyInputTransformation
  :: forall s m
   . MonadState { marloweState :: NonEmptyList MarloweState | s } m
  => (Array Input -> Array Input)
  -> m Unit
applyInputTransformation inputTransformation =
  updateMarloweState
    ( applyPendingInputs
        <<<
          ( over (_executionState <<< _SimulationRunning <<< _pendingInputs)
              inputTransformation
          )
    )

applyInput
  :: forall s m
   . MonadState { marloweState :: NonEmptyList MarloweState | s } m
  => Input
  -> m Unit
applyInput input = applyInputTransformation $ flip snoc $ input

updateChoice
  :: forall s m
   . MonadState { marloweState :: NonEmptyList MarloweState | s } m
  => ChoiceId
  -> ChosenNum
  -> m Unit
updateChoice choiceId chosenNum = updateMarloweState
  ( over (_executionState <<< _SimulationRunning <<< _possibleActions)
      (mapPartiesActionInput (doUpdate choiceId))
  )
  where
  doUpdate :: ChoiceId -> ActionInput -> ActionInput
  doUpdate wantedChoiceId (ChoiceInput currentChoiceId bounds _)
    | wantedChoiceId == currentChoiceId = ChoiceInput choiceId bounds chosenNum

  doUpdate _ input = input

startSimulation
  :: forall s m
   . MonadState { marloweState :: NonEmptyList MarloweState | s } m
  => Instant
  -> Term Contract
  -> m Unit
startSimulation initialTime contract =
  let
    initialExecutionState =
      emptyExecutionStateWithTime initialTime contract
        # set (_SimulationRunning <<< _log)
            ( catMaybes
                [ Just $ StartEvent initialTime
                , case contract of
                    Term Close _ -> Just $ CloseEvent
                      -- TODO: SCP-3887 unify time construct
                      ( TimeInterval
                          (POSIXTime initialTime)
                          (POSIXTime initialTime)
                      )
                    _ -> Nothing
                ]
            )

  in
    updateMarloweState
      ( {- This code was taken/adapted from the SimulationPage, we should revisit if applyPendingInputs is necesary
      when we are starting a simulation, as there should not be any prior pending input. The only reason
      that I think it might be useful is if the contract starts with something other than a When clause.
      TODO: revisit this
      -} applyPendingInputs
          <<< (set _executionState initialExecutionState)
      )

advanceTime
  :: forall s m
   . MonadState { marloweState :: NonEmptyList MarloweState | s } m
  => Instant
  -> MaybeT m Unit
advanceTime newTime = do
  time <- MaybeT $ peruse
    ( _currentMarloweState
        <<< _executionState
        <<< _SimulationRunning
        <<< _time
    )
  guard $ newTime > time
  mSignificantTime <- use (_marloweState <<< _Head <<< to nextTimeout)
  let
    -- We only apply pending inputs if the new slot is "significant", in other
    -- words, if it would trigger a timeout
    mApplyPendingInputs =
      if newTime >= (fromMaybe unixEpoch mSignificantTime) then
        applyPendingInputs
      else
        identity
  updateMarloweState (mApplyPendingInputs <<< updateTime newTime)

hasHistory
  :: forall s. { marloweState :: NonEmptyList MarloweState | s } -> Boolean
hasHistory state = case state ^. (_marloweState <<< _Tail) of
  Nil -> false
  Cons _ _ -> true

evalObservation :: MarloweState -> Observation -> Boolean
evalObservation
  { executionState: Just (SimulationRunning executionState) }
  observation =
  let
    txInput = pendingTransactionInputs executionState
  in
    case fixInterval (unwrap txInput).interval (executionState ^. _state) of
      IntervalTrimmed env state' -> S.evalObservation env state' observation
      -- if there is an error in the state we will say that the observation is false.
      -- Nothing should happen anyway because applying the input will fail later
      IntervalError _ -> false

evalObservation _ _ = false

nextTimeout :: MarloweState -> Maybe Instant
nextTimeout state = do
  contract <- previewOn state
    (_executionState <<< _SimulationRunning <<< _contract)
  let
    Timeouts { minTime } = timeouts contract
  map unwrap minTime

mapPartiesActionInput
  :: (ActionInput -> ActionInput) -> PartiesAction -> PartiesAction
mapPartiesActionInput f (PartiesAction m) = PartiesAction $ (map <<< map) f m

getAllActions :: PartiesAction -> Array ActionInput
getAllActions (PartiesAction p) =
  Map.toUnfoldable p
    # map snd
    # bindFlipped (map snd <<< Map.toUnfoldable)
