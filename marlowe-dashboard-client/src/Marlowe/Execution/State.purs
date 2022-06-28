module Marlowe.Execution.State
  ( currentStep
  , expandBalances
  , extractNamedActions
  , getActionParticipant
  , getAllPayments
  , isClosed
  , mkInitialState
  , mkTx
  , nextState
  , nextTimeout
  , pendingTimeouts
  , removePendingTransaction
  , restoreState
  , setPendingTransaction
  , timeoutState
  ) where

import Prologue

import Data.Array (foldl, length)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.Function (on)
import Data.Lens (view, (^.))
import Data.List (List(..), concat, fromFoldable)
import Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, fromMaybe', maybe)
import Data.Newtype (unwrap)
import Data.Time.Duration (Days(..), Seconds(..))
import Data.Tuple.Nested ((/\))
import Language.Marlowe.Client (ContractHistory)
import Language.Marlowe.Core.V1.Semantics
  ( computeTransaction
  , emptyState
  , evalValue
  , makeEnvironment
  , reduceContractUntilQuiescent
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( Accounts
  , Action(..)
  , Case(..)
  , ChoiceId(..)
  , Contract(..)
  , Environment(..)
  , Input
  , MarloweData(..)
  , MarloweParams
  , Party
  , Payment
  , ReduceResult(..)
  , TimeInterval(..)
  , Timeouts(..)
  , Token
  , TransactionError(..)
  , TransactionInput(..)
  , TransactionOutput(..)
  , _accounts
  , timeouts
  )
import Language.Marlowe.Core.V1.Semantics.Types (State) as Semantic
import Language.Marlowe.Extended.V1.Metadata (MetaData)
import Marlowe.Client (getInitialData, getMarloweParams, getTransactionInputs)
import Marlowe.Execution.Lenses (_resultingPayments)
import Marlowe.Execution.Types
  ( NamedAction(..)
  , PastAction(..)
  , PendingTimeouts
  , State
  , TimeoutInfo
  )
import Marlowe.PAB (PlutusAppId)
import Partial.Unsafe (unsafePartial)
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Plutus.V1.Ledger.Time as POSIXTime
import Safe.Coerce (coerce)

mkInitialState
  :: PlutusAppId
  -> MarloweParams
  -> MetaData
  -> Contract
  -> State
mkInitialState followerAppId marloweParams metadata contract =
  { semanticState: emptyState
  , contract
  , contractHistory: Nothing
  , initialContract: contract
  , metadata
  , marloweParams
  , history: mempty
  , mPendingTransaction: Nothing
  , mPendingTimeouts: Nothing
  , mNextTimeout: nextTimeout contract
  , followerAppId
  }

restoreState
  :: PlutusAppId
  -> Instant
  -> MetaData
  -> ContractHistory
  -> Either TransactionError State
restoreState followerAppId currentTime metadata history = do
  let
    MarloweData { marloweContract, marloweState } = getInitialData history
    marloweParams = getMarloweParams history
    inputs = getTransactionInputs history
    -- Derive the initial params from the Follower Contract params
    initialState =
      { semanticState: marloweState
      , contract: marloweContract
      , initialContract: marloweContract
      , metadata
      , marloweParams
      , history: mempty
      , contractHistory: Just history
      , mPendingTransaction: Nothing
      , mPendingTimeouts: Nothing
      , mNextTimeout: nextTimeout marloweContract
      , followerAppId
      }
  -- Apply all the transaction inputs
  appliedInputState <- foldl
    (\mState txInput -> nextState txInput =<< mState)
    (Right initialState)
    inputs

  -- See if any step has timeouted
  timeoutState currentTime appliedInputState

setPendingTransaction :: TransactionInput -> State -> State
setPendingTransaction txInput state = state
  { mPendingTransaction = Just txInput }

removePendingTransaction :: State -> State
removePendingTransaction state = state
  { mPendingTransaction = Nothing }

nextState :: TransactionInput -> State -> Either TransactionError State
nextState txInput state =
  let
    { semanticState, contract, history } =
      state
    TransactionInput { interval: TimeInterval (POSIXTime minTime) _, inputs } =
      txInput

  in
    case computeTransaction txInput semanticState contract of
      (Error err) -> Left err
      (TransactionOutput { txOutState, txOutContract, txOutPayments }) ->
        let
          mPendingTransaction =
            if state.mPendingTransaction == Just txInput then
              Nothing
            else
              state.mPendingTransaction

          -- For the moment the only way to get an empty transaction is if there was a timeout,
          -- but later on there could be other reasons to move a contract forward, and we should
          -- compare with the contract to see the reason.
          action = case inputs of
            Nil ->
              TimeoutAction
                { time: minTime
                , missedActions:
                    extractActionsFromContract minTime semanticState contract
                }
            _ -> InputAction

          pastState =
            { balancesAtStart: semanticState ^. _accounts
            , action
            , txInput
            , balancesAtEnd: txOutState ^. _accounts
            , resultingPayments: txOutPayments
            }
        in
          pure $ state
            { semanticState = txOutState
            , contract = txOutContract
            , history = Array.snoc history pastState
            , mPendingTransaction = mPendingTransaction
            , mPendingTimeouts = Nothing
            , mNextTimeout = nextTimeout txOutContract
            }

nextTimeout :: Contract -> Maybe Instant
nextTimeout = timeouts >>> \(Timeouts { minTime }) -> coerce minTime

mkTx :: Instant -> Maybe Instant -> List Input -> TransactionInput
mkTx currentTime mNextTimeout inputs =
  TransactionInput { interval: mkInterval currentTime mNextTimeout, inputs }

-- This function checks if the are any new timeouts in the current execution state
timeoutState :: Instant -> State -> Either TransactionError State
timeoutState currentTime state =
  let
    { semanticState
    , contract
    , mPendingTimeouts
    , mNextTimeout
    } = state
    -- We start of by getting a PendingTimeout structure from the execution state (because the
    -- contract could already have some timeouts that were "advanced")
    { continuation, timeouts } =
      fromMaybe'
        ( \_ ->
            { continuation: { state: semanticState, contract }, timeouts: [] }
        )
        mPendingTimeouts

    -- This helper function does all the leg work.
    -- A contract step can be stale/timeouted but it does not advance on its own, it needs
    -- an empty transaction or the next meaningfull transaction. With this function we check if
    -- the contract has timeouted and calculate what would be the resulting continuation contract
    -- and resulting state if we'd apply an empty transaction.
    advanceAllTimeouts
      :: Maybe Instant
      -> Array TimeoutInfo
      -> Semantic.State
      -> Contract
      -> Either
           TransactionError
           { mNextTimeout :: Maybe Instant
           , mPendingTimeouts :: Maybe PendingTimeouts
           }
    advanceAllTimeouts (Just timeoutTime) newTimeouts state' contract'
      | timeoutTime <= currentTime = do
          let env = on makeEnvironment POSIXTime currentTime currentTime

          { txOutState, txOutContract } <-
            case reduceContractUntilQuiescent env state' contract' of
              -- TODO: SCP-2088 We need to discuss how to display the warnings that computeTransaction may give
              ContractQuiescent _ _ _ txOutState txOutContract ->
                Right { txOutState, txOutContract }
              RRAmbiguousTimeIntervalError ->
                Left TEAmbiguousTimeIntervalError

          let newNextTimeout = nextTimeout txOutContract
          let
            timeoutInfo =
              { time: timeoutTime
              , missedActions: extractActionsFromContract timeoutTime state'
                  contract'
              }
          advanceAllTimeouts newNextTimeout
            (Array.snoc newTimeouts timeoutInfo)
            txOutState
            txOutContract

    advanceAllTimeouts mNextTimeout newTimeouts state' contract' = pure
      { mNextTimeout
      , mPendingTimeouts:
          if newTimeouts == mempty then
            Nothing
          else
            Just
              { continuation: { state: state', contract: contract' }
              , timeouts: newTimeouts
              }
      }

    advancedTimeouts = advanceAllTimeouts
      mNextTimeout
      timeouts
      continuation.state
      continuation.contract
  in
    state { mPendingTimeouts = _, mNextTimeout = _ }
      <$> (_.mPendingTimeouts <$> advancedTimeouts)
      <*> (_.mNextTimeout <$> advancedTimeouts)

------------------------------------------------------------
isClosed :: State -> Boolean
isClosed { contract: Close } = true

isClosed _ = false

pendingTimeouts :: State -> Maybe PendingTimeouts
pendingTimeouts { mPendingTimeouts } = mPendingTimeouts

getActionParticipant :: NamedAction -> Maybe Party
getActionParticipant (MakeDeposit _ party _ _) = Just party

getActionParticipant (MakeChoice (ChoiceId _ party) _) = Just party

getActionParticipant _ = Nothing

extractNamedActions :: Instant -> State -> Array NamedAction
extractNamedActions
  _
  { mPendingTimeouts: Just { continuation: { contract: Close } } } =
  [ CloseContract ]

extractNamedActions currentTime { mPendingTimeouts: Just { continuation } } =
  extractActionsFromContract currentTime continuation.state
    continuation.contract

extractNamedActions currentTime { semanticState, contract } =
  extractActionsFromContract currentTime semanticState contract

-- a When can only progress if it has timed out or has Cases
extractActionsFromContract
  :: Instant -> Semantic.State -> Contract -> Array NamedAction
extractActionsFromContract _ _ Close = []

extractActionsFromContract currentTime semanticState contract@(When cases _ _) =
  cases <#> \(Case action _) -> toNamedAction action
  where
  toNamedAction (Deposit a p t v) =
    let
      timeInterval = mkInterval currentTime $ nextTimeout contract
      env = Environment { timeInterval }
      amount = evalValue env semanticState v
    in
      MakeDeposit a p t amount

  toNamedAction (Choice cid bounds) = MakeChoice cid bounds

  toNamedAction (Notify obs) = MakeNotify obs

-- In reality other situations should never occur as contracts always reduce to When or Close
-- however someone could in theory publish a contract that starts with another Contract constructor
-- and we would want to enable moving forward with Evaluate
extractActionsFromContract _ _ _ =
  [ Evaluate { bindings: Map.empty, payments: [] } ]

-- This function expands the balances inside the Semantic.State to all participants and tokens,
-- using zero if the participant does not have balance for that token.
expandBalances :: Array Party -> Array Token -> Accounts -> Accounts
expandBalances participants tokens stateAccounts =
  Map.fromFoldable do
    party <- participants
    tokens
      <#> \token ->
        let
          key = party /\ token
        in
          key /\ (fromMaybe zero $ Map.lookup key stateAccounts)

mkInterval :: Instant -> Maybe Instant -> TimeInterval
mkInterval currentTime mNextTimeout =
  case mNextTimeout of
    -- There are no timeouts in the remaining contract. i.e. the contract is
    -- not an executable contract.
    Nothing -> TimeInterval
      (POSIXTime currentTime)
      (POSIXTime currentTimeAdjustedForBlockConfirmation)
    Just nextTO
      -- FIXME SCP-2875 Change hardcoded time in mkInterval
      -- The next timeout in the contract has already passed - i.e. the
      -- branch is timed out.
      | nextTO < currentTime ->
          TimeInterval
            ( fromMaybe (POSIXTime nextTO)
                $ POSIXTime.adjust (Seconds 1.0)
                $ POSIXTime nextTO
            )
            top
      -- There is a timeout approaching. The valid interval for the transaction
      -- is any time between now and the next timeout.
      | otherwise ->
          TimeInterval (POSIXTime currentTime)
            $ fromMaybe (POSIXTime nextTO)
            $ POSIXTime.adjust (Seconds (-1.0)) (POSIXTime nextTO)
  where
  -- TODO move this to configuration and possibly process server-side instead.
  -- If the block is confirmed after the upper bound of the interval, the
  -- transaction will fail in the validator. Since it takes some time to
  -- confirm a block, we have to add a buffer to the current time to allow the
  -- transaction to complete.
  currentTimeAdjustedForBlockConfirmation = unwrap
    $ unsafePartial
    $ fromJust
    $ POSIXTime.adjust (Days one) (POSIXTime currentTime)

getAllPayments :: State -> List Payment
getAllPayments { history } = concat $ fromFoldable $ map
  (view _resultingPayments)
  history

-- Zero-indexed step
currentStep :: State -> Int
currentStep { history, mPendingTimeouts } = pastSteps + pendingSteps
  where
  pastSteps = length history
  pendingSteps = maybe 0 (\{ timeouts } -> length timeouts) mPendingTimeouts
