-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Property-based tests for `computeTransaction` of Marlowe semantics.
--
-----------------------------------------------------------------------------


{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans               #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds      #-}


module Spec.Marlowe.Semantics.Compute
  ( -- * Testing
    tests
  ) where


import Control.Applicative (liftA2)
import Control.Lens.Getter (Getter, to, view)
import Control.Monad.Except (MonadError(throwError), unless, when)
import Control.Monad.Reader (ReaderT(runReaderT))
import Data.Bifunctor (second)
import Data.Default (Default(..))
import Data.Function (on)
import Data.List (sort)
import Data.Tuple (swap)
import Language.Marlowe.Core.V1.Semantics
  ( Payment(..)
  , TransactionError(TEAmbiguousTimeIntervalError, TEApplyNoMatchError, TEIntervalError, TEUselessTransaction)
  , TransactionInput(..)
  , TransactionOutput(Error, txOutContract, txOutPayments, txOutState, txOutWarnings)
  , TransactionWarning(TransactionAssertionFailed, TransactionShadowing)
  , computeTransaction
  , evalObservation
  , evalValue
  )
import Language.Marlowe.Core.V1.Semantics.Types
  ( AccountId
  , Accounts
  , Action(Choice, Deposit, Notify)
  , Bound(..)
  , Case(..)
  , ChoiceId
  , ChosenNum
  , Contract(..)
  , Environment(Environment)
  , Input(..)
  , InputContent(IChoice, IDeposit, INotify)
  , IntervalError(IntervalInPastError, InvalidInterval)
  , Observation(TrueObs)
  , Payee(Party)
  , State(..)
  , TimeInterval
  , Token(..)
  , Value(Constant)
  , ValueId
  , getAction
  , getInputContent
  )
import Language.Marlowe.FindInputs (getAllInputs)
import Plutus.Script.Utils.Scripts (datumHash)
import Plutus.V2.Ledger.Api (CurrencySymbol, Datum(..), DatumHash(..), POSIXTime(..), TokenName, toBuiltinData)
import Spec.Marlowe.Semantics.Arbitrary
  ( Context
  , SemiArbitrary(semiArbitrary)
  , arbitraryContractWeighted
  , arbitraryGoldenTransaction
  , arbitraryPositiveInteger
  , assertContractWeights
  , closeContractWeights
  , defaultContractWeights
  , ifContractWeights
  , letContractWeights
  , whenContractWeights
  )
import Spec.Marlowe.Semantics.AssocMap (assocMapEq, assocMapInsert, assocMapLookup)
import Spec.Marlowe.Semantics.Merkle (deepMerkleize, merkleizeInputs)
import Spec.Marlowe.Semantics.Orphans ()
import System.IO.Unsafe (unsafePerformIO)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary(..)
  , Gen
  , Testable(property)
  , chooseInteger
  , discard
  , elements
  , forAll
  , forAllShrink
  , frequency
  , shuffle
  , suchThat
  , testProperty
  )

import qualified PlutusTx.AssocMap as AM


-- | Record of choices.
type Choices = AM.Map ChoiceId ChosenNum


-- | Record of bound values.
type BoundValues = AM.Map ValueId Integer


-- | Alternative representation of a payment.
newtype Payment' = Payment' (AccountId, Payee, [(CurrencySymbol, TokenName, Integer)])
  deriving (Eq, Ord)


-- | Unpack a payment.
unPayment :: Payment -> Payment'
unPayment (Payment a p (Token c n) i) = Payment' (a, p, [(c, n, i)])


-- | The context of transaction execution for Marlowe.
data MarloweContext =
  MarloweContext
  {
    mcInput    :: TransactionInput   -- ^ The input to the transaction.
  , mcState    :: State              -- ^ The state of the contract.
  , mcContract :: Contract           -- ^ The contract.
  , mcOutput   :: TransactionOutput  -- ^ The output of the transaction.
  }
    deriving (Show)

instance Arbitrary MarloweContext where
  arbitrary = semiArbitrary =<< arbitrary
  shrink mc@MarloweContext{..} =
    fmap updateOutput
      $  [mc {mcInput    = input'   } | input'    <- shrink mcInput   ]
      <> [mc {mcState    = state'   } | state'    <- shrink mcState   ]
      <> [mc {mcContract = contract'} | contract' <- shrink mcContract]

instance SemiArbitrary MarloweContext where
  semiArbitrary context =
    do
      mcState  <- semiArbitrary context
      contract <- semiArbitrary context
      input    <- semiArbitrary context
      isMerkleized <- frequency [(9, pure False), (1, pure True)]
      let
        (contract', continuations) = deepMerkleize contract
        (mcContract, mcInput) =
          case (isMerkleized, merkleizeInputs continuations mcState contract' input) of
            (True, Just input') -> (contract', input')
            _                   -> (contract , input )
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


-- | Generate an arbitrary Marlowe transaction context.
arbitraryMarloweContext :: [(Int, Int, Int, Int, Int, Int)]  -- ^ The weights for contract terms.
                        -> Gen MarloweContext                -- ^ Generator for a transaction context.
arbitraryMarloweContext w =
    do
      context    <- arbitrary
      mcInput    <- semiArbitrary context
      mcState    <- semiArbitrary context
      mcContract <- arbitraryContractWeighted w context
      let
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


-- | Generate an arbitrary valid Marlowe transaction context.
arbitraryValid :: Gen MarloweContext
arbitraryValid =
  let
    randomContext =
      do
        mcContract <- arbitrary `suchThat` (/= Close)
        (time, inputs') <-
          case unsafePerformIO $ getAllInputs mcContract of
            Right candidates -> elements candidates
            Left _           -> discard
        let
          -- TODO: Generalize to arbitrary starting state.
          mcState = State AM.empty AM.empty AM.empty time
          mcInput = head inputs'
          mcOutput = computeTransaction mcInput mcState mcContract
        pure MarloweContext{..}
    goldenContext =
      do
        (mcState, mcContract, mcInput, mcOutput) <- arbitraryGoldenTransaction True
        pure MarloweContext{..}
  in
    frequency [(1, randomContext), (2, goldenContext)]


-- | Generate a simple valid transaction.
simpleTransaction
  :: Context
  -> (TimeInterval -> Contract -> Gen Contract)
  -> (State -> Gen State)
  -> (TransactionInput -> Gen TransactionInput)
  -> Gen MarloweContext
simpleTransaction context modifyContract modifyState modifyInput =
  do
    mcState <- modifyState =<< semiArbitrary context
    intervalStart <- (getPOSIXTime (minTime mcState) +) <$> arbitraryPositiveInteger
    intervalEnd <- (intervalStart +) <$> arbitraryPositiveInteger
    let
      interval = (POSIXTime intervalStart, POSIXTime intervalEnd)
    timeout <- (intervalEnd +) <$> arbitraryPositiveInteger
    mcContract <- modifyContract interval . When [] (POSIXTime timeout) =<< semiArbitrary context
    mcInput <- modifyInput $ TransactionInput interval []
    let
      mcOutput = computeTransaction mcInput mcState mcContract
    pure MarloweContext{..}


-- | Generate a simple payment.
simplePayment :: Gen MarloweContext
simplePayment =
  do
    context <- arbitrary
    balance <- arbitraryPositiveInteger
    payment <- chooseInteger (1, balance)
    account <- semiArbitrary context
    payee <- semiArbitrary context
    token <- semiArbitrary context
    let
      modifyContract _ contract = pure $ Pay account (Party payee) token (Constant payment) contract
      modifyState state =
        do
          accounts' <-
            fmap AM.fromList . shuffle . AM.toList
              . AM.insert (account, token) balance
              . AM.delete (account, token)
              $ accounts state
          pure state {accounts = accounts'}
    simpleTransaction context modifyContract modifyState pure


-- | Generate a simple deposit.
simpleDeposit :: Gen MarloweContext
simpleDeposit =
  do
    context <- arbitrary
    deposit <- arbitraryPositiveInteger
    account <- semiArbitrary context
    payee <- semiArbitrary context
    token <- semiArbitrary context
    let
      modifyContract (_, POSIXTime intervalEnd) contract =
        pure
          $ When [Case (Deposit account payee token $ Constant deposit) contract]
            (POSIXTime $ intervalEnd + 1) contract
      modifyInput (TransactionInput interval _) = pure $ TransactionInput interval [NormalInput $ IDeposit account payee token deposit]
    simpleTransaction context modifyContract pure modifyInput


-- | Generate a simple notification.
simpleChoice :: Gen MarloweContext
simpleChoice =
  do
    context <- arbitrary
    choiceId <- semiArbitrary context
    value <- semiArbitrary context
    bound <- Bound <$> ((value -) <$> arbitraryPositiveInteger) <*> ((value +) <$> arbitraryPositiveInteger)
    let
      modifyContract (_, POSIXTime intervalEnd) contract =
        do
          let
            timeout = POSIXTime $ intervalEnd + 1
          contract' <- When [] timeout <$> semiArbitrary context
          pure $ When [Case (Choice choiceId [bound]) contract'] timeout contract
      modifyInput (TransactionInput interval _) = pure $ TransactionInput interval [NormalInput $ IChoice choiceId value]
    simpleTransaction context modifyContract pure modifyInput


-- | Generate a simple notification.
simpleNotify :: Gen MarloweContext
simpleNotify =
  do
    context <- arbitrary
    let
      modifyContract (_, POSIXTime intervalEnd) contract =
        do
          let
            timeout = POSIXTime $ intervalEnd + 1
          contract' <- When [] timeout <$> semiArbitrary context
          pure $ When [Case (Notify TrueObs) contract'] timeout contract
      modifyInput (TransactionInput interval _) = pure $ TransactionInput interval [NormalInput INotify]
    simpleTransaction context modifyContract pure modifyInput


-- | Generate a simple notification.
simpleMerkleization :: Gen MarloweContext
simpleMerkleization =
  do
    context <- arbitrary
    mcState <- semiArbitrary context
    intervalStart <- (getPOSIXTime (minTime mcState) +) <$> arbitraryPositiveInteger
    intervalEnd <- (intervalStart +) <$> arbitraryPositiveInteger
    let
      interval = (POSIXTime intervalStart, POSIXTime intervalEnd)
    timeout <- (intervalEnd +) <$> arbitraryPositiveInteger
    contract <- When [] (POSIXTime timeout) <$> semiArbitrary context
    let
      DatumHash hash = datumHash . Datum $ toBuiltinData contract
      mcInput = TransactionInput interval [MerkleizedInput INotify hash contract]
    mcContract <- When [MerkleizedCase (Notify TrueObs) hash] (POSIXTime timeout) <$> semiArbitrary context
    let
      mcOutput = computeTransaction mcInput mcState mcContract
    pure MarloweContext{..}


-- | Recompute the output of a Marlowe transaction in an transaction context.
updateOutput :: MarloweContext -> MarloweContext
updateOutput mc@MarloweContext{..} =
  mc {mcOutput = computeTransaction mcInput mcState mcContract}


-- | Generate an invalid time interval.
makeInvalidInterval :: MarloweContext -> MarloweContext
makeInvalidInterval mc@MarloweContext{mcInput=mcInput@TransactionInput{txInterval=i}} =
  updateOutput
    $ mc {mcInput = mcInput {txInterval = swap i}}


-- | Fetch the environment from an transaction context.
environment :: Getter MarloweContext Environment
environment =
  to $ \MarloweContext{..} ->
    Environment
      (
        maximum [minTime mcState, fst $ txInterval mcInput]
      , snd $ txInterval mcInput
      )


-- | Fetch the validity interval from a tranaction context.
validTimes :: Getter MarloweContext TimeInterval
validTimes = to $ txInterval . mcInput


-- | Fetch the earliest time of a validity interval in a transaction context.
earliestTime :: Getter MarloweContext POSIXTime
earliestTime = to $ fst . txInterval . mcInput


-- | Fetch the lastest time of a validity interval in a transaction context.
latestTime :: Getter MarloweContext POSIXTime
latestTime = to $ snd . txInterval . mcInput


-- | Fetch the minimum time of the state in a transaction context.
minimumTime :: Getter MarloweContext POSIXTime
minimumTime = to $ \MarloweContext{..} -> maximum [minTime mcState, fst $ txInterval mcInput]


-- | Fetch the inputs in a transaction context.
inputs :: Getter MarloweContext [Input]
inputs = to $ txInputs . mcInput


-- | Fetch the pre-transaction state in a transaction context.
preState :: Getter MarloweContext State
preState = to mcState


-- | Fetch the post-transaction state in a transaction context.
postState :: Getter MarloweContext State
postState = to $ txOutState . mcOutput


-- | Fetch the pre-transaction accounts in a transaction context.
preAccounts :: Getter MarloweContext Accounts
preAccounts = preState . to accounts


-- | Fetch the post-transaction accounts in a transaction context.
postAccounts :: Getter MarloweContext Accounts
postAccounts = postState . to accounts


-- | Fetch the pre-transaction choices in a transaction context.
preChoices :: Getter MarloweContext Choices
preChoices = preState . to choices


-- | Fetch the post-transaction choices in a transaction context.
postChoices :: Getter MarloweContext Choices
postChoices = postState . to choices


-- | Fetch the pre-transaction bound values in a transaction context.
preValues :: Getter MarloweContext BoundValues
preValues = preState . to boundValues


-- | Fetch the post-transaction bound values in a transaction context.
postValues :: Getter MarloweContext BoundValues
postValues = postState . to boundValues


-- | Fetch the pre-transaction minimum transaction time in a transaction context.
preTime :: Getter MarloweContext POSIXTime
preTime = preState . to minTime


-- | Fetch the post-transaction minimum transaction time in a transaction context.
postTime :: Getter MarloweContext POSIXTime
postTime = postState . to minTime


-- | Fetch the pre-transaction contract in a transaction context.
preContract :: Getter MarloweContext Contract
preContract = to mcContract


-- | Fetch the post-transaction contract in a transaction context.
postContract :: Getter MarloweContext Contract
postContract = to $ txOutContract . mcOutput


-- | Fetch the warnings in a transaction context.
warnings :: Getter MarloweContext [TransactionWarning]
warnings = to $ txOutWarnings . mcOutput


-- | Fetch the payments in a transaction context.
payments :: Getter MarloweContext [Payment]
payments = to $ txOutPayments . mcOutput


-- | Fetch the error in a transaction context, or fail.
transactionError :: Getter MarloweContext TransactionError
transactionError = to $ getError . mcOutput
  where
    getError (Error e) = e
    getError _         = error "TransactionOutput has no error."


-- | Fail if the transaction context does not have an error.
noError :: Getter MarloweContext ()
noError = to $ getNoError . mcOutput
  where
    getNoError (Error e) = error $ "TransactionOutput has error: " <> show e <> "."
    getNoError _         = ()


-- | Monad for tests reading a transaction execution context.
type Testify a = ReaderT MarloweContext (Either String) a


-- | Contextually evaluate a value.
evaluate :: Value Observation -> Testify Integer
evaluate value = evalValue <$> view environment <*> view preState <*> pure value


-- | Contextually evaluate an observation.
observe :: Observation -> Testify Bool
observe observation = evalObservation <$> view environment <*> view preState <*> pure observation


-- | An invariant of the Marlowe transaction execution context.
data Invariant =
    SameState     -- ^ The pre- and post-transaction states are identical.
  | SameAccounts  -- ^ The pre- and post-transaction account records are identical.
  | SameChoices   -- ^ The pre- and post-transaction choice records are identical.
  | SameValues    -- ^ The pre- and post-transaction bound values are identical.
  | SameTime      -- ^ The pre- and post-transaction minimum times in the states are identical.
  | SameContract  -- ^ The pre- and post-transaction contracts are identical.


-- | The pre- and post-transaction states must be identical.
sameState    :: [Invariant]
sameState    = pure SameState


-- | The pre- and post-transaction account records must be identical.
sameAccounts :: [Invariant]
sameAccounts = pure SameAccounts


-- | The pre- and post-transaction choice records must be identifical.
sameChoices  :: [Invariant]
sameChoices  = pure SameChoices


-- | The pre- and post-transaction bound values must be identical.
sameValues   :: [Invariant]
sameValues   = pure SameValues


-- | The pre- and post-transaction minimum times in the states must be identical.
sameTime     :: [Invariant]
sameTime     = pure SameTime


-- | The pre- and post-transaction contracts must be identical.
sameContract :: [Invariant]
sameContract = pure SameContract


-- | Two aspects of the transaction context must be identical.
same :: Eq a
     => String
     -> Getter MarloweContext a
     -> Getter MarloweContext a
     -> Testify ()
same message pre post =
  liftA2 (==) (view pre) (view post)
    >>= (`unless` throwError message)


-- | Assert an invariant.
checkInvariant :: Invariant -> Testify ()
checkInvariant SameState    = same "State changed."        preState    postState
checkInvariant SameAccounts = same "Accounts changed."     preAccounts postAccounts
checkInvariant SameChoices  = same "Choices changed."      preChoices  postChoices
checkInvariant SameValues   = same "Bound values changed." preValues   postValues
checkInvariant SameTime     = same "Minimum time changed." preTime     postTime
checkInvariant SameContract = same "Contract changed."     preContract postContract


-- | Assert a condition.
require :: MonadError e m
        => e
        -> (a -> Bool)
        -> a
        -> m ()
require message = ((`unless` throwError message) .)


-- | Assert a condition on the number of inputs.
requireInputs :: (Int -> Bool) -> Testify ()
requireInputs f = view inputs >>= require "Wrong number of inputs." (f . length)


-- | Assert that there are no accounts in the initial state.
requireNoAccounts :: Testify ()
requireNoAccounts = view preAccounts >>= require "Accounts present." AM.null


-- | Assert that there are accounts in the initial state.
requireAccounts :: Testify ()
requireAccounts = view preAccounts >>= require "Accounts absent." (not . AM.null)


-- | Assert a particular contract before the transaction occurs.
requireContract :: Contract -> Testify ()
requireContract contract = view preContract >>= require "Contract does not match." (== contract)


-- | Assert that the pre-transaction contract starts with `Pay`.
unPay :: Contract -> Testify (AccountId, Payee, Token, Value Observation, Contract)
unPay (Pay a p t n c) = pure (a, p, t, n, c)
unPay _               = throwError "Contract does not start with `Pay`."


-- | Assert that the pre-transaction contract starts with `Let`.
unLet :: Contract -> Testify (ValueId, Value Observation, Contract)
unLet (Let i x c) = pure (i, x, c)
unLet _           = throwError "Contract does not start with `Let`."


-- | Assert that the pre-transaction contract starts with `If`.
unIf :: Contract -> Testify (Observation, Contract, Contract)
unIf (If o c1 c2) = pure (o, c1, c2)
unIf _            = throwError "Contract does not start with `If`."


-- | Assert that the pre-transaction contract starts with `Assert`.
unAssert :: Contract -> Testify (Observation, Contract)
unAssert (Assert o c) = pure (o, c)
unAssert _            = throwError "Contract does not start with `Assert`."


-- | Assert that the pre-transaction contract starts with `When`.
unWhen :: Contract -> Testify ([Case Contract], POSIXTime, Contract)
unWhen (When cs t c) = pure (cs, t, c)
unWhen _             = throwError "Contract does not start with `When`."


-- | Asset that the pre-transaction contract starts with `Let` followed by `When`.
requireLetWhen :: Testify ()
requireLetWhen =
  do
    contract <- view preContract
    (_, _, contract') <- unLet contract
    (_, timeout, _) <- unWhen contract'
    view minimumTime `requireLE` pure timeout
    view latestTime  `requireLT` pure timeout


-- | Assert that the pre-transaction contract starts with `If` followed by `When`.
requireIfWhen :: Testify ()
requireIfWhen =
  do
    contract <- view preContract
    (_, thenContract, elseContract) <- unIf contract
    (_, thenTimeout, _) <- unWhen thenContract
    view minimumTime `requireLE` pure thenTimeout
    view latestTime  `requireLT` pure thenTimeout
    (_, elseTimeout, _) <- unWhen elseContract
    view minimumTime `requireLE` pure elseTimeout
    view latestTime  `requireLT` pure elseTimeout


-- | Assert that the pre-transaction contract starts with `Assert` followed by `When`.
requireAssertWhen :: Testify ()
requireAssertWhen =
  do
    contract <- view preContract
    (_, contract') <- unAssert contract
    (_, timeout, _) <- unWhen contract'
    view minimumTime `requireLE` pure timeout
    view latestTime  `requireLT` pure timeout


-- | Assert that a first quantity is less than a second one.
requireLT :: Ord a => Testify a -> Testify a -> Testify ()
requireLT x y = liftA2 (<) x y >>= (`unless` throwError "Not less than.")


-- | Assert that a first quantity is less than or equal to a second one.
requireLE :: Ord a => Testify a -> Testify a -> Testify ()
requireLE x y = liftA2 (<=) x y >>= (`unless` throwError "Not less than or equal to.")


-- | Assert that the earliest time in the validity interval is less than the minimum time in the pre-transaction state.
requireEarliestLtPre :: Testify ()
requireEarliestLtPre = view earliestTime `requireLT` view preTime


-- | Assert that the earliest time in the valiidity interval is less than or equal to the minimum time in the pre-transaction state.
requireEarliestLeLatest :: Testify ()
requireEarliestLeLatest = view earliestTime `requireLE` view latestTime


-- | Assert that the validity interval is valid for transacting, and that the start of the validity interval does not preceed the minimum time of the pre-transaction state.
requireValidTime :: Testify ()
requireValidTime =
     view earliestTime `requireLE` view preTime
  >> view preTime      `requireLE` view latestTime


-- | Assert that the validity interval is in the past.
requireInPast :: Testify ()
requireInPast =
     view earliestTime `requireLE` view latestTime
  >> view latestTime   `requireLT` view preTime


-- | Assert that the validity interval is not valid for transacting.
requireInvalidInterval :: Testify ()
requireInvalidInterval = view latestTime `requireLT` view earliestTime


-- | Assert that the pre-transaction contract starts with `When` and will time out.
requireNextTimeout :: Testify POSIXTime
requireNextTimeout =
  do
    c <- view preContract
    case c of
      When _ timeout _ -> pure timeout
      _                -> throwError "Not `When`."


-- | Assert that the transaction will not time out.
requireNotTimeout :: Testify ()
requireNotTimeout =
     view minimumTime `requireLE` requireNextTimeout
  >> view latestTime  `requireLT` requireNextTimeout


-- | Assert that the validity interval is ambiguous with respect to timing out.
requireAmbiguousTimeout :: Testify ()
requireAmbiguousTimeout =
     view minimumTime   `requireLT` requireNextTimeout
  >> requireNextTimeout `requireLE` view latestTime


-- | Require a payment to a party.
requirePayout :: Testify ()
requirePayout =
  let
    isPayout (Payment _ (Party _) _ i) = i > 0
    isPayout _ = False
  in
    view payments >>= "Positive payout" `require` any isPayout


-- | Require not deposits.
requireNoDeposits :: Testify ()
requireNoDeposits =
  let
    isDeposit (NormalInput (IDeposit _ _ _ i)) = i > 0
    isDeposit (MerkleizedInput (IDeposit _ _ _ i) _ _) = i > 0
    isDeposit _ = False
  in
    view inputs >>= "No deposits" `require` (not . any isDeposit)


-- | Throw an error unless a condition holds.
throwUnless :: MonadError String m
            => String
            -> (a -> Bool)
            -> a
            -> m ()
throwUnless message = ((`unless` throwError message) .)


-- | Assert that the post-transaction accounts are empty.
hasNoAccounts :: Testify ()
hasNoAccounts = view postAccounts >>= throwUnless "Has outgoing accounts." AM.null


-- | Assert that there are no warnings.
noWarnings :: Testify ()
noWarnings = view warnings >>= throwUnless "Warnings present." null


-- | Assert that there are no payments.
noPayments :: Testify ()
noPayments = view payments >>= throwUnless "Payments present." null


-- | Pay tokens to oneself.
paySelf :: ((AccountId, Token), Integer) -> Payment
paySelf ((a, t), i) = Payment a (Party a) t i


-- | Assert that a set of payments are made.
makesPayments :: [Payment] -> Testify ()
makesPayments ps =
  do
    ps' <- view payments
    let
      okay = ((==) `on` (sort . fmap unPayment)) ps ps'
    unless okay
      $ throwError "Payments do not match."


-- | Assert that all pre-transaction accounts are paid.
paysAllAccounts :: Testify ()
paysAllAccounts =
  do
    payments' <- fmap paySelf . AM.toList <$> view preAccounts
    makesPayments payments'


-- | Assert that a particular error has occurred.
hasError :: TransactionError -> Testify ()
hasError e = view transactionError >>= throwUnless "Error not found."  (== e)


-- | Assert that the type of the first input does not match the type of any of the actions in the next 'When'.
requireIncompatibleInput :: Testify ()
requireIncompatibleInput =
  do
    requireInputs (> 0)
    input <- getInputContent . head <$> view inputs
    (cs, _, _) <- unWhen =<< view preContract
    let matches IDeposit{} Deposit{}  = True
        matches IChoice{}  Choice{}   = True
        matches INotify{}  Notify{}   = True
        matches  _                  _ = False
    any (matches input . getAction) cs
      `when` throwError "Input may be compatible with action."


-- | Extract the variable, value, warnings, and post-transaction contract from a `Let` at the start of the pre-transaction contract.
extractLet :: Testify ((ValueId, Integer), [TransactionWarning], Contract)
extractLet =
  do
    (variable, value, continuation) <- unLet =<< view preContract
    values <- view preValues
    value' <- evaluate value
    pure
      (
        (variable, value')
      , maybe [] (\x -> [TransactionShadowing variable x value']) $ assocMapLookup variable values
      , continuation
      )


-- | Extract the post-transaction contract from an `If` at the start of the pre-transaction contract.
extractIf :: Testify Contract
extractIf =
  do
    (observation, thenContinuation, elseContinuation) <- unIf =<< view preContract
    observation' <- observe observation
    pure
      $ if observation'
          then thenContinuation
          else elseContinuation


-- | Extract the warnings, and post-transaction contract from an `Assert` at the start of the pre-transaction contract.
extractAssert :: Testify ([TransactionWarning], Contract)
extractAssert =
  do
    (observation, continuation) <- unAssert =<< view preContract
    observation' <- observe observation
    pure
      (
        [TransactionAssertionFailed | not observation']
      , continuation
      )


-- | Assert the post-transaction values.
checkValues :: AM.Map ValueId Integer -> Testify ()
checkValues expected =
   view postValues
    >>= (`unless` throwError "Mismatch in expected bound values.")
    . (expected `assocMapEq`)


-- | Assert the post-transaction contract.
checkContinuation :: Contract -> Testify ()
checkContinuation expected =
  view postContract
    >>= (`unless` throwError "Mismatch in expected contract.")
    . (== expected)


-- | Specify a test for a Marlowe transaction.
data TransactionTest =
  TransactionTest
  {
    name           :: String
  , generator      :: Gen MarloweContext
  , precondition   :: Testify ()
  , invariant      :: [Invariant]
  , postcondition  :: Testify ()
  , allowShrinkage :: Bool
  }

instance Default TransactionTest where
  def =
    TransactionTest
    {
      name           = mempty
    , generator      = arbitrary
    , precondition   = pure ()
    , invariant      = mempty
    , postcondition  = pure ()
    , allowShrinkage = True
    }


-- | Test a Marlowe transaction.
test :: TransactionTest  -- ^ The test.
     -> TestTree         -- ^ The result.
test TransactionTest{..} =
  testProperty name
    . property
    $ let
        preResolve :: Testify () -> MarloweContext -> Bool
        preResolve predicate initial = runReaderT predicate initial == Right ()
        postResolve :: Testify () -> MarloweContext -> Bool
        postResolve predicate initial =
          case runReaderT predicate initial of
            Left _   -> False  -- Test failed.
            Right () -> True   -- Test passed.
        gen = generator `suchThat` preResolve precondition
      in
        (if allowShrinkage then forAllShrink gen shrink else forAll gen)
          . postResolve
          $ mapM_ checkInvariant invariant >> postcondition


-- | Test detection of invalid time intervals.
invalidInterval :: TransactionTest
invalidInterval =
  def
  {
    name          = "Detect invalid time interval"
  , generator     = makeInvalidInterval <$> arbitrary
  , precondition  = requireInvalidInterval
  , postcondition = view validTimes >>= hasError . TEIntervalError . InvalidInterval
  }


-- | Test detection of intervals in the past.
tooEarly :: TransactionTest
tooEarly =
  def
  {
    name          = "Detect time interval in past"
  , precondition  = requireInPast
  , postcondition = IntervalInPastError <$> view preTime <*> view validTimes >>= hasError . TEIntervalError
  }


-- | Test the detection of ambiguous time intervals.
ambiguousTimeout :: TransactionTest
ambiguousTimeout =
  def
  {
    name          = "Ambiguous interval for timeout"
  , precondition  = requireAmbiguousTimeout >> requireInputs (== 0)
  , postcondition = hasError TEAmbiguousTimeIntervalError
  }


-- | Test the detection of a useless transaction.
uselessNoInput :: TransactionTest
uselessNoInput =
  def
  {
    name          = "Applying no inputs is useless until timeout"
  , precondition  = requireValidTime >> requireNotTimeout >> requireInputs (== 0)
  , postcondition = hasError TEUselessTransaction
  }


-- | Test that closing empy accounts is useless.
explicitClose :: TransactionTest
explicitClose =
  def
  {
    name          = "Closing no accounts is useless"
  , generator     = arbitraryMarloweContext [closeContractWeights]
  , precondition  = requireContract Close >> requireNoAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = mempty
  , postcondition = hasError TEUselessTransaction
  }


-- | Test that closing pays all accounts.
implicitClose :: TransactionTest
implicitClose =
  def
  {
    name          = "Pay all accounts on close"
  , generator     = arbitraryMarloweContext [closeContractWeights]
  , precondition  = requireContract Close >> requireAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = sameChoices <> sameValues
  , postcondition = hasNoAccounts >> noWarnings >> paysAllAccounts
  }


-- | Test the detection of no matching input.
noMatch :: TransactionTest
noMatch =
  def
  {
    name          = "No matching input"
  , generator     = arbitraryMarloweContext
                      . (whenContractWeights :)
                      . (`replicate` defaultContractWeights)
                      =<< arbitrary `suchThat` (< 4)
  , precondition  = requireValidTime >> requireNotTimeout >> requireIncompatibleInput
  , postcondition = hasError TEApplyNoMatchError
  }


-- | Test that `Let` correctly sets a variable and may warn about shadowing.
letSets :: TransactionTest
letSets =
  def
  {
    name          = "Let sets variable"
  , generator     = arbitraryMarloweContext [letContractWeights, whenContractWeights, defaultContractWeights]
  , precondition  = requireLetWhen >> requireAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = sameAccounts <> sameChoices
  , postcondition = do
                      ((variable, value), shadowing, continuation) <- extractLet
                      expected <- assocMapInsert variable value <$> view preValues
                      checkValues expected
                      view warnings
                        >>= (`unless` throwError "Erroneous shadowing warning.") . (== shadowing)
                      checkContinuation continuation
  }


-- | Test that `If` correctly branches.
ifBranches :: TransactionTest
ifBranches =
  def
  {
    name          = "If branches"
  , generator     = arbitraryMarloweContext [ifContractWeights, whenContractWeights, defaultContractWeights]
  , precondition  = requireIfWhen >> requireAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = sameState
  , postcondition = checkContinuation =<< extractIf
  }


-- | Test that `Assert` correctly warns.
assertWarns :: TransactionTest
assertWarns =
  def
  {
    name          = "Asset warns"
  , generator     = arbitraryMarloweContext [assertContractWeights, whenContractWeights, defaultContractWeights]
  , precondition  = requireAssertWhen >> requireAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = sameState
  , postcondition = do
                      (failing, continuation) <- extractAssert
                      view warnings
                        >>= (`unless` throwError "Erroneous assertion warning.") . (failing ==)
                      checkContinuation continuation
  }


-- | Test that transacting with input from static analysis does not yield an error.
anyInput :: TransactionTest
anyInput =
  def
  {
    name          = "Static-analysis input"
  , generator     = arbitraryValid
  , postcondition = view noError
  }


-- | Test that payments substract value from internal accounts.
payingSubtractsFromAccount :: TransactionTest
payingSubtractsFromAccount =
  def
  {
    name           = "Paying subtracts from account"
  , allowShrinkage = False
  , generator      = simplePayment
  , postcondition  = do
                       delta <-
                         fmap (AM.filter (/= 0))
                           $ AM.unionWith (+)
                           <$> view preAccounts
                           <*> (AM.fromList . fmap (second negate) . AM.toList <$> view postAccounts)
                       require "Only one balance changes." ((== 1) . length . AM.toList) delta
                       require "Some balance decreases." (all (> 0) . AM.elems) delta
  }


-- | Test that deposits add value to internal accounts.
depositAddsToAccount :: TransactionTest
depositAddsToAccount =
  def
  {
    name           = "Deposit adds to account"
  , allowShrinkage = False
  , generator      = simpleDeposit
  , postcondition  = do
                       delta <-
                         fmap (AM.filter (/= 0))
                           $ AM.unionWith (+)
                           <$> view postAccounts
                           <*> (AM.fromList . fmap (second negate) . AM.toList <$> view preAccounts)
                       require "Only one balance changes." ((== 1) . length . AM.toList) delta
                       require "Some balance increases." (all (> 0) . AM.elems) delta
  }


-- | Test that notify contines as expected
notifyContinues :: TransactionTest
notifyContinues =
  def
  {
    name           = "Notify continues as expected"
  , allowShrinkage = False
  , generator      = simpleNotify
  , postcondition  = view preContract >>=
                       \case
                         When [Case (Notify TrueObs) contract] _ _ -> checkContinuation contract
                         _ -> throwError "Test setup failed."
  }


-- | Test that choice continues as expected.
choiceContinues :: TransactionTest
choiceContinues =
  def
  {
    name           = "Choice continues as expected"
  , allowShrinkage = False
  , generator      = simpleChoice
  , postcondition  = view preContract >>=
                       \case
                         When [Case (Choice _ _) contract] _ _ -> checkContinuation contract
                         _ -> throwError "Test setup failed."
  }


-- | Test that deposit continues as expected.
depositContinues :: TransactionTest
depositContinues =
  def
  {
    name           = "Deposit continues as expected"
  , allowShrinkage = False
  , generator      = simpleDeposit
  , postcondition  = view preContract >>=
                       \case
                         When [Case Deposit{} contract] _ _ -> checkContinuation contract
                         _ -> throwError "Test setup failed."
  }


-- | Test that deposit continues as expected.
merkleizationContinues :: TransactionTest
merkleizationContinues =
  def
  {
    name           = "Merkleization continues as expected"
  , allowShrinkage = False
  , generator      = simpleMerkleization
  , postcondition  = view inputs >>=
                       \case
                         [MerkleizedInput INotify _ contract] -> checkContinuation contract
                         _ -> throwError "Test setup failed."
  }


-- | Test that deposit continues as expected.
choiceSets :: TransactionTest
choiceSets =
  def
  {
    name           = "Choice records the value of the choice"
  , allowShrinkage = False
  , generator      = simpleChoice
  , postcondition  = view inputs >>=
                       \case
                         [NormalInput (IChoice choiceId value)] ->
                           do
                             choices' <- choices <$> view postState
                             unless (AM.lookup choiceId choices' == Just value)
                               $ throwError "Choice missing from state"
                         _ -> throwError "Test setup failed."
  }


-- | Run the tests.
tests :: TestTree
tests =
  testGroup "Compute Transaction"
    $ test <$>
    [
      invalidInterval
    , tooEarly
    , ambiguousTimeout
    , uselessNoInput
    , explicitClose
    , implicitClose
    , noMatch
    , letSets
    , ifBranches
    , assertWarns
    , anyInput
    , payingSubtractsFromAccount
    , depositAddsToAccount
    , depositContinues
    , choiceContinues
    , notifyContinues
    , choiceSets
    , merkleizationContinues
    ]
