module Marlowe.Execution.State
  ( contractName
  , currentStep
  , expandBalances
  , extractNamedActions
  , getActionParticipant
  , getAllPayments
  , isClosed
  , mkInitialState
  , mkTx
  , nextState
  , nextTimeout
  , restoreState
  , setPendingTransaction
  , timeoutState
  ) where

import Prologue

import Control.Bind (bindFlipped)
import Data.Array (foldl, length)
import Data.Array as Array
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as ContractNickname
import Data.DateTime.Instant (Instant)
import Data.Either (note)
import Data.Lens (view, (^.), (^?))
import Data.List (List(..), concat, fromFoldable)
import Data.Map as Map
import Data.Maybe (fromMaybe, fromMaybe', maybe, maybe')
import Data.String (joinWith)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Traversable (for)
import Data.Tuple.Nested ((/\))
import Marlowe.Client (ContractHistory, _chHistory, _chParams)
import Marlowe.Execution.Lenses (_resultingPayments)
import Marlowe.Execution.Types
  ( NamedAction(..)
  , PastAction(..)
  , PendingTimeouts
  , State
  , TimeoutInfo
  )
import Marlowe.Extended.Metadata (MetaData)
import Marlowe.Semantics
  ( Accounts
  , Action(..)
  , Case(..)
  , ChoiceId(..)
  , Contract(..)
  , Environment(..)
  , Input
  , MarloweParams
  , Party
  , Payment
  , ReduceResult(..)
  , TimeInterval(..)
  , Timeouts(..)
  , Token
  , TransactionInput(..)
  , TransactionOutput(..)
  , _accounts
  , _marloweContract
  , _marloweState
  , _rolesCurrency
  , computeTransaction
  , emptyState
  , evalValue
  , makeEnvironment
  , reduceContractUntilQuiescent
  , timeouts
  )
import Marlowe.Semantics (State) as Semantic
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Plutus.V1.Ledger.Time as POSIXTime
import Safe.Coerce (coerce)

mkInitialState
  :: Maybe ContractNickname
  -> MarloweParams
  -> MetaData
  -> Contract
  -> State
mkInitialState contractNickname marloweParams metadata contract =
  { semanticState: emptyState
  , contractNickname
  , contract
  , metadata
  , marloweParams
  , history: mempty
  , mPendingTransaction: Nothing
  , mPendingTimeouts: Nothing
  , mNextTimeout: nextTimeout contract
  }

restoreState
  :: Instant
  -> Maybe ContractNickname
  -> MetaData
  -> ContractHistory
  -> Either String State

restoreState currentTime contractNickname metadata history = do
  Tuple marloweParams marloweData <-
    note "params not available" $ history ^? _chParams
  let
    contract = view _marloweContract marloweData
    initialSemanticState = view _marloweState marloweData
    inputs = view _chHistory history
    -- Derive the initial params from the Follower Contract params
    initialState =
      { semanticState: initialSemanticState
      , contractNickname
      , contract
      , metadata
      , marloweParams
      , history: mempty
      , mPendingTransaction: Nothing
      , mPendingTimeouts: Nothing
      , mNextTimeout: nextTimeout contract
      }
  -- Apply all the transaction inputs
  foldl (flip (bindFlipped <<< nextState)) (pure initialState) inputs
    -- See if any step has timeouted
    >>= timeoutState currentTime

-- Each contract should always have a name, if we
-- have given a Local nickname, we use that, if not we
-- show the currency symbol
contractName :: State -> String
contractName { contractNickname, marloweParams } = maybe'
  (\_ -> view _rolesCurrency marloweParams)
  ContractNickname.toString
  contractNickname

setPendingTransaction :: TransactionInput -> State -> State
setPendingTransaction txInput state = state
  { mPendingTransaction = Just txInput }

nextState :: TransactionInput -> State -> Either String State
nextState txInput state = do
  let
    { semanticState, contract, history } =
      state
    TransactionInput { interval: TimeInterval (POSIXTime minTime) _, inputs } =
      txInput

    { txOutState, txOutContract, txOutPayments } =
      case computeTransaction txInput semanticState contract of
        (TransactionOutput { txOutState, txOutContract, txOutPayments }) ->
          { txOutState, txOutContract, txOutPayments }
        -- We should not have contracts which cause errors in the dashboard so we will just ignore error cases for now
        -- FIXME: Change nextState to return an Either
        -- TODO: SCP-2088 We need to discuss how to display the warnings that computeTransaction may give
        (Error _) ->
          { txOutState: semanticState
          , txOutContract: contract
          , txOutPayments: mempty
          }

    mPendingTransaction =
      if state.mPendingTransaction == Just txInput then
        Nothing
      else
        state.mPendingTransaction

  -- For the moment the only way to get an empty transaction is if there was a timeout,
  -- but later on there could be other reasons to move a contract forward, and we should
  -- compare with the contract to see the reason.
  action <- case inputs of
    Nil ->
      TimeoutAction <<< { time: minTime, missedActions: _ }
        <$> extractActionsFromContract minTime semanticState contract
    _ -> pure InputAction

  let
    pastState =
      { balancesAtStart: semanticState ^. _accounts
      , action
      , txInput
      , balancesAtEnd: txOutState ^. _accounts
      , resultingPayments: txOutPayments
      }

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

mkTx :: Instant -> Contract -> List Input -> Either String TransactionInput
mkTx currentTime contract inputs =
  TransactionInput <<< { interval: _, inputs }
    <$> mkInterval currentTime contract

-- This function checks if the are any new timeouts in the current execution state
timeoutState :: Instant -> State -> Either String State
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
           String
           { mNextTimeout :: Maybe Instant
           , mPendingTimeouts :: Maybe PendingTimeouts
           }
    advanceAllTimeouts (Just timeoutTime) newTimeouts state' contract'
      | timeoutTime <= currentTime = do
          let
            env = makeEnvironment
              (POSIXTime currentTime)
              (POSIXTime currentTime)

            { txOutState, txOutContract } =
              case reduceContractUntilQuiescent env state' contract' of
                -- TODO: SCP-2088 We need to discuss how to display the warnings that computeTransaction may give
                ContractQuiescent _ _ _ txOutState txOutContract ->
                  { txOutState, txOutContract }
                -- FIXME: Change timeoutState to return an Either
                RRAmbiguousTimeIntervalError ->
                  { txOutState: state', txOutContract: contract' }

            newNextTimeout = nextTimeout txOutContract
          timeoutInfo <- { time: timeoutTime, missedActions: _ }
            <$> extractActionsFromContract timeoutTime state' contract'
          advanceAllTimeouts newNextTimeout
            (Array.snoc newTimeouts timeoutInfo)
            txOutState
            txOutContract

    advanceAllTimeouts mNextTimeout' newTimeouts state' contract' = pure
      { mNextTimeout: mNextTimeout'
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
    state
      { mPendingTransaction = Nothing
      , mPendingTimeouts = _
      , mNextTimeout = _
      }
      <$> (_.mPendingTimeouts <$> advancedTimeouts)
      <*> (_.mNextTimeout <$> advancedTimeouts)

------------------------------------------------------------
isClosed :: State -> Boolean
isClosed { contract: Close } = true

isClosed _ = false

getActionParticipant :: NamedAction -> Maybe Party
getActionParticipant (MakeDeposit _ party _ _) = Just party

getActionParticipant (MakeChoice (ChoiceId _ party) _ _) = Just party

getActionParticipant _ = Nothing

extractNamedActions :: Instant -> State -> Either String (Array NamedAction)
extractNamedActions
  _
  { mPendingTimeouts: Just { continuation: { contract: Close } } } =
  pure [ CloseContract ]

extractNamedActions currentTime { mPendingTimeouts: Just { continuation } } =
  extractActionsFromContract currentTime continuation.state
    continuation.contract

extractNamedActions currentTime { semanticState, contract } =
  extractActionsFromContract currentTime semanticState contract

-- a When can only progress if it has timed out or has Cases
extractActionsFromContract
  :: Instant -> Semantic.State -> Contract -> Either String (Array NamedAction)
extractActionsFromContract _ _ Close = pure []

extractActionsFromContract currentTime semanticState contract@(When cases _ _) =
  for cases \(Case action _) -> toNamedAction action
  where
  toNamedAction (Deposit a p t v) = do
    timeInterval <- mkInterval currentTime contract
    let
      env = Environment { timeInterval }
      amount = evalValue env semanticState v
    pure $ MakeDeposit a p t amount

  toNamedAction (Choice cid bounds) = pure $ MakeChoice cid bounds Nothing

  toNamedAction (Notify obs) = pure $ MakeNotify obs

-- In reality other situations should never occur as contracts always reduce to When or Close
-- however someone could in theory publish a contract that starts with another Contract constructor
-- and we would want to enable moving forward with Evaluate
extractActionsFromContract _ _ _ =
  pure [ Evaluate { bindings: Map.empty, payments: [] } ]

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

mkInterval :: Instant -> Contract -> Either String TimeInterval
mkInterval currentTime contract =
  case nextTimeout contract of
    Nothing -> map (TimeInterval $ POSIXTime currentTime)
      $ note "Ten seconds from now is outside the range of valid dates."
      $ POSIXTime.adjust (Seconds 10.0) (POSIXTime currentTime)
    Just nextTO
      -- FIXME: We should change this for a Maybe TimeInterval and return Nothing in this case.
      | nextTO < currentTime -> Left "Timeout has already passed."
      | otherwise ->
          note
            ( joinWith "\n"
                [ "1 millisecond before the next timeout is outside the range of valid dates."
                , "This is a bug, please report it at https://github.com/input-output-hk/marlowe-cardano/issues with the following information:"
                , "currentTime: " <> show currentTime
                , "nextTO: " <> show nextTO
                ]
            )
            $ map (TimeInterval $ POSIXTime currentTime)
            $ POSIXTime.adjust (Milliseconds (-1.0))
            $ POSIXTime nextTO

getAllPayments :: State -> List Payment
getAllPayments { history } = concat $ fromFoldable $ map
  (view _resultingPayments)
  history

currentStep :: State -> Int
currentStep { history, mPendingTimeouts } = pastSteps + pendingSteps
  where
  pastSteps = length history
  pendingSteps = maybe 0 (\{ timeouts } -> length timeouts) mPendingTimeouts
