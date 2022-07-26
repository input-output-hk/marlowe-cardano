
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Spec.Marlowe.Transaction (
  tests
) where


import Control.Applicative
import Control.Lens.Getter
import Control.Monad.Except
import Control.Monad.Reader
import Data.Function (on)
import Data.List (sort)
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

instance ContextuallyArbitrary MarloweContext where
  arbitrary' context =
    do
      Environment txInterval <- arbitrary' context
      n <- arbitraryFibonacci [1, 1, 1, 1, 0, 2]  -- TODO: Review.
      mcInput    <- TransactionInput txInterval <$> vectorOf n (arbitrary' context)
      mcState    <- arbitrary' context
      mcContract <- arbitrary' context
      let
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


validTimes :: Getter MarloweContext TimeInterval
validTimes = to $ txInterval . mcInput


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


type Testify a = ReaderT MarloweContext (Either (Maybe String) ) a


same :: Eq a
     => String
     -> Getter MarloweContext a
     -> Getter MarloweContext a
     -> Testify ()
same message pre post =
  liftA2 (==) (view pre) (view post)
    >>= (`unless` throwError (pure message))


sameState :: Testify ()
sameState = same "State changed." preState postState


sameStateDetailed :: Testify ()
sameStateDetailed = sameAccounts >> sameChoices >> sameValues >> sameTime


sameAccounts :: Testify ()
sameAccounts = same "Accounts changed." preAccounts postAccounts


sameChoices :: Testify ()
sameChoices = same "Choices changed." preChoices postChoices


sameValues :: Testify ()
sameValues = same "Bound values changed." preValues postValues


sameTime :: Testify ()
sameTime = same "Minimum time changed." preTime postTime


sameContract :: Testify ()
sameContract = same "Contract changed." preContract postContract


require :: MonadError (Maybe e) m
        => (a -> Bool)
        -> a
        -> m ()
require = ((`unless` throwError Nothing) .)


requireNoInputs :: Testify ()
requireNoInputs = view inputs >>= require null


requireInputs :: Testify ()
requireInputs = view inputs >>= require (not . null)


requireNoAccounts :: Testify ()
requireNoAccounts = view preAccounts >>= require AM.null


requireAccounts :: Testify ()
requireAccounts = view preAccounts >>= require (not . AM.null)


requireContract :: Contract -> Testify ()
requireContract contract = view preContract >>= require (== contract)


requireValidTime :: Testify ()
requireValidTime =
  do
    time <- view preTime
    (lower, upper) <- view validTimes
    unless (lower <= time && time <= upper)
      $ throwError Nothing


requireInPast :: Testify ()
requireInPast =
  do
    time <- view preTime
    (lower, upper) <- view validTimes
    unless (lower <= upper && upper < time)
      $ throwError Nothing


throwUnless :: MonadError (Maybe String) m
            => String
            -> (a -> Bool)
            -> a
            -> m ()
throwUnless message = ((`unless` throwError (Just message)) .)


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
      . throwError
      $ Just "Payments do not match."


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
  , invariant     :: Testify ()
  , postcondition :: Testify ()
  }


test :: TransactionTest -> TestTree
test TransactionTest{..} =
  testProperty name
    . property
    $ let
        preResolve :: Testify () -> MarloweContext -> Bool
        preResolve predicate initial = runReaderT predicate initial == Right ()
        postResolve :: Testify () -> MarloweContext -> Bool
        postResolve predicate initial =
          case runReaderT predicate initial of
            Left Nothing  -> True   -- Precondition not satisfied.
            Left (Just _) -> False  -- Test failed.
            Right ()      -> True   -- Test passed.
        gen = generator `suchThat` preResolve precondition
      in
        forAll gen
          . postResolve
          $ precondition >> invariant >> postcondition


explicitClose :: TransactionTest
explicitClose =
  TransactionTest
  {
    name          = "Closing no accounts is useless"
  , generator     = arbitrary
  , precondition  = requireContract Close >> requireNoAccounts >> requireValidTime >> requireNoInputs
  , invariant     = pure ()
  , postcondition = hasError TEUselessTransaction
  }


implicitClose :: TransactionTest
implicitClose =
  TransactionTest
  {
    name          = "Pay all accounts on close"
  , generator     = arbitrary
  , precondition  = requireContract Close >> requireAccounts >> requireValidTime >> requireNoInputs
  , invariant     = sameChoices >> sameValues
  , postcondition = hasNoAccounts >> noWarnings >> paysAllAccounts
  }


tooEarly :: TransactionTest
tooEarly =
  TransactionTest
  {
    name          = "Detect interval in past"
  , generator     = arbitrary
  , precondition  = requireInPast
  , invariant     = pure ()
  , postcondition = IntervalInPastError <$> view preTime <*> view validTimes >>= hasError . TEIntervalError
  }


tests :: TestTree
tests =
  testGroup "Transactions"
    $ fmap test
    [
      explicitClose
    , implicitClose
    , tooEarly
    ]
