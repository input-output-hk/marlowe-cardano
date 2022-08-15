
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}


module Spec.Marlowe.Transaction (
  tests
) where


import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens.Getter
import Control.Monad.Except
import Control.Monad.Reader
import Data.Function (on)
import Data.List (sort)
import Data.Tuple (swap)
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types
import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime (..), TokenName)
import Plutus.V1.Ledger.Value (flattenValue)
import Spec.Marlowe.Arbitrary
import Spec.Marlowe.Orphans ()
import Test.Tasty
import Test.Tasty.QuickCheck

import qualified PlutusTx.AssocMap as AM

import qualified Ledger.Value as Money (singleton)


type Choices = AM.Map ChoiceId ChosenNum


type BoundValues = AM.Map ValueId Integer


newtype Payment' = Payment' (AccountId, Payee, [(CurrencySymbol, TokenName, Integer)])
  deriving (Eq, Ord)


unPayment :: Payment -> Payment'
unPayment (Payment a p m) = Payment' (a, p, flattenValue m)


instance Arbitrary TransactionInput where
  arbitrary = arbitrary' =<< arbitrary
  shrink TransactionInput{..} =
       [TransactionInput   interval' txInputs  | interval' <- shrink txInterval]
    <> [TransactionInput txInterval    inputs' | inputs'   <- shrink txInputs  ]

instance ContextuallyArbitrary TransactionInput where
  arbitrary' context =
    do
      Environment txInterval <- arbitrary' context
      n <- arbitraryFibonacci [1, 1, 1, 1, 0, 2]  -- TODO: Review.
      TransactionInput txInterval <$> vectorOf n (arbitrary' context)


data MarloweContext =
  MarloweContext
  {
    mcInput    :: TransactionInput
  , mcState    :: State
  , mcContract :: Contract
  , mcOutput   :: TransactionOutput
  }
    deriving (Show)

instance Arbitrary MarloweContext where
  arbitrary = arbitrary' =<< arbitrary
  shrink mc@MarloweContext{..} =
       [mc {mcInput    = input'   , mcOutput = computeTransaction   input' mcState  mcContract } | input'    <- shrink mcInput   ]
    <> [mc {mcState    = state'   , mcOutput = computeTransaction mcInput    state' mcContract } | state'    <- shrink mcState   ]
    <> [mc {mcContract = contract', mcOutput = computeTransaction mcInput  mcState    contract'} | contract' <- shrink mcContract]

instance ContextuallyArbitrary MarloweContext where
  arbitrary' context =
    do
      mcInput    <- arbitrary' context
      mcState    <- arbitrary' context
      mcContract <- arbitrary' context
      let
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


updateOutput :: MarloweContext -> MarloweContext
updateOutput mc@MarloweContext{..} =
  mc {mcOutput = computeTransaction mcInput mcState mcContract}


makeInvalidInterval :: MarloweContext -> MarloweContext
makeInvalidInterval mc@MarloweContext{mcInput=mcInput@TransactionInput{txInterval=i}} =
  updateOutput
    $ mc {mcInput = mcInput {txInterval = swap i}}


validTimes :: Getter MarloweContext TimeInterval
validTimes = to $ txInterval . mcInput


earliestTime :: Getter MarloweContext POSIXTime
earliestTime = to $ fst . txInterval . mcInput


latestTime :: Getter MarloweContext POSIXTime
latestTime = to $ snd . txInterval . mcInput


minimumTime :: Getter MarloweContext POSIXTime
minimumTime = to $ \MarloweContext{..} -> maximum [minTime mcState, fst $ txInterval mcInput]


inputs :: Getter MarloweContext [Input]
inputs = to $ txInputs . mcInput


preState :: Getter MarloweContext State
preState = to mcState


postState :: Getter MarloweContext State
postState = to $ txOutState . mcOutput


preAccounts :: Getter MarloweContext Accounts
preAccounts = preState . to accounts


postAccounts :: Getter MarloweContext Accounts
postAccounts = postState . to accounts


preChoices :: Getter MarloweContext Choices
preChoices = preState . to choices


postChoices :: Getter MarloweContext Choices
postChoices = postState . to choices


preValues :: Getter MarloweContext BoundValues
preValues = preState . to boundValues


postValues :: Getter MarloweContext BoundValues
postValues = postState . to boundValues


preTime :: Getter MarloweContext POSIXTime
preTime = preState . to minTime


postTime :: Getter MarloweContext POSIXTime
postTime = postState . to minTime


preContract :: Getter MarloweContext Contract
preContract = to mcContract


postContract :: Getter MarloweContext Contract
postContract = to $ txOutContract . mcOutput


warnings :: Getter MarloweContext [TransactionWarning]
warnings = to $ txOutWarnings . mcOutput


payments :: Getter MarloweContext [Payment]
payments = to $ txOutPayments . mcOutput


transactionError :: Getter MarloweContext TransactionError
transactionError = to $ getError . mcOutput
  where
    getError (Error e) = e
    getError _         = error "TransactionOutput has no error."


type Testify a = ReaderT MarloweContext (Either String) a


data Invariant =
    SameState
  | SameAccounts
  | SameChoices
  | SameValues
  | SameTime
  | SameContract


sameState    = pure SameState    :: [Invariant]
sameAccounts = pure SameAccounts :: [Invariant]
sameChoices  = pure SameChoices  :: [Invariant]
sameValues   = pure SameValues   :: [Invariant]
sameTime     = pure SameTime     :: [Invariant]
sameContract = pure SameContract :: [Invariant]


same :: Eq a
     => String
     -> Getter MarloweContext a
     -> Getter MarloweContext a
     -> Testify ()
same message pre post =
  liftA2 (==) (view pre) (view post)
    >>= (`unless` throwError message)


checkInvariant :: Invariant -> Testify ()
checkInvariant SameState    = same "State changed."        preState    postState
checkInvariant SameAccounts = same "Accounts changed."     preAccounts postAccounts
checkInvariant SameChoices  = same "Choices changed."      preChoices  postChoices
checkInvariant SameValues   = same "Bound values changed." preValues   postValues
checkInvariant SameTime     = same "Minimum time changed." preTime     postTime
checkInvariant SameContract = same "Contract changed."     preContract postContract


require :: MonadError e m
        => e
        -> (a -> Bool)
        -> a
        -> m ()
require message = ((`unless` throwError message) .)


requireInputs :: (Int -> Bool) -> Testify ()
requireInputs f = view inputs >>= require "Wrong number of inputs." (f . length)


requireNoAccounts :: Testify ()
requireNoAccounts = view preAccounts >>= require "Accounts present." AM.null


requireAccounts :: Testify ()
requireAccounts = view preAccounts >>= require "Accounts absent." (not . AM.null)


requireContract :: Contract -> Testify ()
requireContract contract = view preContract >>= require "Contract does not match." (== contract)


requireLT :: Ord a => Testify a -> Testify a -> Testify ()
requireLT x y = liftA2 (<) x y >>= (`unless` throwError "Not less than.")


requireLE :: Ord a => Testify a -> Testify a -> Testify ()
requireLE x y = liftA2 (<=) x y >>= (`unless` throwError "Not less than or equal to.")


requireEarliestLtPre :: Testify ()
requireEarliestLtPre = view earliestTime `requireLT` view preTime


requireEarliestLeLatest :: Testify ()
requireEarliestLeLatest = view earliestTime `requireLE` view latestTime


requireValidTime :: Testify ()
requireValidTime =
     view earliestTime `requireLE` view preTime
  >> view preTime      `requireLE` view latestTime


requireInPast :: Testify ()
requireInPast =
     view earliestTime `requireLE` view latestTime
  >> view latestTime   `requireLT` view preTime


requireInvalidInterval :: Testify ()
requireInvalidInterval = view latestTime `requireLT` view earliestTime


requireNextTimeout :: Testify POSIXTime
requireNextTimeout =
  do
    c <- view preContract
    case c of
      When _ timeout _ -> pure timeout
      _                -> throwError "Not `When`."


requireNotTimeout :: Testify ()
requireNotTimeout =
     view minimumTime `requireLE` requireNextTimeout
  >> view latestTime  `requireLT` requireNextTimeout


requireAmbiguousTimeout :: Testify ()
requireAmbiguousTimeout =
     view minimumTime   `requireLT` requireNextTimeout
  >> requireNextTimeout `requireLE` view latestTime


throwUnless :: MonadError String m
            => String
            -> (a -> Bool)
            -> a
            -> m ()
throwUnless message = ((`unless` throwError message) .)


hasNoAccounts :: Testify ()
hasNoAccounts = view postAccounts >>= throwUnless "Has outgoing accounts." AM.null


noWarnings :: Testify ()
noWarnings = view warnings >>= throwUnless "Warnings present." null


noPayments :: Testify ()
noPayments = view payments >>= throwUnless "Payments present." null


paySelf :: ((AccountId, Token), Integer) -> Payment
paySelf ((a, Token c n), i) = Payment a (Party a) $ Money.singleton c n i


makesPayments :: [Payment] -> Testify ()
makesPayments ps =
  do
    ps' <- view payments
    let
      okay = ((==) `on` (sort . fmap unPayment)) ps ps'
    unless okay
      $ throwError "Payments do not match."


paysAllAccounts :: Testify ()
paysAllAccounts =
  do
    payments' <- fmap paySelf . AM.toList <$> view preAccounts
    makesPayments payments'


hasError :: TransactionError -> Testify ()
hasError e = view transactionError >>= throwUnless "Error not found."  (== e)


data TransactionTest =
  TransactionTest
  {
    name          :: String
  , generator     :: Gen MarloweContext
  , precondition  :: Testify ()
  , invariant     :: [Invariant]
  , postcondition :: Testify ()
  }


test :: Bool -> TransactionTest -> TestTree
test doShrink TransactionTest{..} =
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
        (if doShrink then forAllShrink gen shrink else forAll gen)
          . postResolve
          $ mapM_ checkInvariant invariant >> postcondition


invalidInterval :: TransactionTest
invalidInterval =
  TransactionTest
  {
    name          = "Detect invalid time interval"
  , generator     = makeInvalidInterval <$> arbitrary
  , precondition  = requireInvalidInterval
  , invariant     = mempty
  , postcondition = view validTimes >>= hasError . TEIntervalError . InvalidInterval
  }


tooEarly :: TransactionTest
tooEarly =
  TransactionTest
  {
    name          = "Detect time interval in past"
  , generator     = arbitrary
  , precondition  = requireInPast
  , invariant     = mempty
  , postcondition = IntervalInPastError <$> view preTime <*> view validTimes >>= hasError . TEIntervalError
  }


ambiguousTimeout :: TransactionTest
ambiguousTimeout =
  TransactionTest
  {
    name          = "Ambiguous interval for timeout"
  , generator     = arbitrary
  , precondition  = requireAmbiguousTimeout >> requireInputs (== 0)
  , invariant     = mempty
  , postcondition = hasError TEAmbiguousTimeIntervalError
  }


uselessNoInput :: TransactionTest
uselessNoInput =
  TransactionTest
  {
    name          = "Applying no inputs is useless until timeout"
  , generator     = arbitrary
  , precondition  = requireValidTime >> requireNotTimeout >> requireInputs (== 0)
  , invariant     = mempty
  , postcondition = hasError TEUselessTransaction
  }


explicitClose :: TransactionTest
explicitClose =
  TransactionTest
  {
    name          = "Closing no accounts is useless"
  , generator     = arbitrary
  , precondition  = requireContract Close >> requireNoAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = mempty
  , postcondition = hasError TEUselessTransaction
  }


implicitClose :: TransactionTest
implicitClose =
  TransactionTest
  {
    name          = "Pay all accounts on close"
  , generator     = arbitrary
  , precondition  = requireContract Close >> requireAccounts >> requireValidTime >> requireInputs (== 0)
  , invariant     = sameChoices <> sameValues
  , postcondition = hasNoAccounts >> noWarnings >> paysAllAccounts
  }


tests :: TestTree
tests =
  testGroup "Transactions"
    $ fmap (test False)
    [
      invalidInterval
    , tooEarly
    , ambiguousTimeout
    , uselessNoInput
    , explicitClose
    , implicitClose
    ]
