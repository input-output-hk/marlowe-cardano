
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-orphans               #-}


module Spec.Marlowe.Semantics.Compute (
  tests
) where


import Control.Applicative
import Control.Arrow ((&&&))
import Control.Lens.Getter
import Control.Monad.Except
import Control.Monad.Reader
import Data.Default (Default (..))
import Data.Function (on)
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Tuple (swap)
import Language.Marlowe.Core.V1.Semantics
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.FindInputs (getAllInputs)
import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime (..), TokenName)
import Plutus.V1.Ledger.Value (flattenValue)
import Spec.Marlowe.Semantics.Arbitrary
import Spec.Marlowe.Semantics.AssocMap
import Spec.Marlowe.Semantics.Orphans ()
import System.IO.Unsafe (unsafePerformIO)
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
  arbitrary = semiArbitrary =<< arbitrary
  shrink mc@MarloweContext{..} =
    fmap updateOutput
      $  [mc {mcInput    = input'   } | input'    <- shrink mcInput   ]
      <> [mc {mcState    = state'   } | state'    <- shrink mcState   ]
      <> [mc {mcContract = contract'} | contract' <- shrink mcContract]

instance SemiArbitrary MarloweContext where
  semiArbitrary context =
    do
      mcInput    <- semiArbitrary context
      mcState    <- semiArbitrary context
      mcContract <- semiArbitrary context
      let
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


arbitraryMarloweContext :: [(Int, Int, Int, Int, Int, Int)] -> Gen MarloweContext
arbitraryMarloweContext w =
    do
      context    <- arbitrary
      mcInput    <- semiArbitrary context
      mcState    <- semiArbitrary context
      mcContract <- arbitraryContractWeighted w context
      let
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


arbitraryValid :: Gen MarloweContext
arbitraryValid =
    do
      mcContract <- arbitrary `suchThat` (/= Close)
      (time, inputs) <-
        case unsafePerformIO $ getAllInputs mcContract of
          Right candidates -> elements candidates
          Left _           -> discard
      let
        -- FIXME: Generalize to arbitrary starting state.
        mcState = State AM.empty AM.empty AM.empty time
        mcInput = head inputs
        mcOutput = computeTransaction mcInput mcState mcContract
      pure MarloweContext{..}


updateOutput :: MarloweContext -> MarloweContext
updateOutput mc@MarloweContext{..} =
  mc {mcOutput = computeTransaction mcInput mcState mcContract}


makeInvalidInterval :: MarloweContext -> MarloweContext
makeInvalidInterval mc@MarloweContext{mcInput=mcInput@TransactionInput{txInterval=i}} =
  updateOutput
    $ mc {mcInput = mcInput {txInterval = swap i}}


environment :: Getter MarloweContext Environment
environment =
  to $ \MarloweContext{..} ->
    Environment
      (
        maximum [minTime mcState, fst $ txInterval mcInput]
      , snd $ txInterval mcInput
      )


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


noError :: Getter MarloweContext ()
noError = to $ getNoError . mcOutput
  where
    getNoError (Error e) = error $ "TransactionOutput has error: " <> show e <> "."
    getNoError _         = ()


type Testify a = ReaderT MarloweContext (Either String) a


evaluate :: Value Observation -> Testify Integer
evaluate value = evalValue <$> view environment <*> view preState <*> pure value


observe :: Observation -> Testify Bool
observe observation = evalObservation <$> view environment <*> view preState <*> pure observation


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


unPay :: Contract -> Testify (AccountId, Payee, Token, Value Observation, Contract)
unPay (Pay a p t n c) = pure (a, p, t, n, c)
unPay _               = throwError "Contract does not start with `Pay`."


unLet :: Contract -> Testify (ValueId, Value Observation, Contract)
unLet (Let i x c) = pure (i, x, c)
unLet _           = throwError "Contract does not start with `Let`."


unIf :: Contract -> Testify (Observation, Contract, Contract)
unIf (If o c1 c2) = pure (o, c1, c2)
unIf _            = throwError "Contract does not start with `If`."


unAssert :: Contract -> Testify (Observation, Contract)
unAssert (Assert o c) = pure (o, c)
unAssert _            = throwError "Contract does not start with `Assert`."


unWhen :: Contract -> Testify ([Case Contract], POSIXTime, Contract)
unWhen (When cs t c) = pure (cs, t, c)
unWhen _             = throwError "Contract does not start with `When`."


requireLetWhen :: Testify ()
requireLetWhen =
  do
    contract <- view preContract
    (_, _, contract') <- unLet contract
    (_, timeout, _) <- unWhen contract'
    view minimumTime `requireLE` pure timeout
    view latestTime  `requireLT` pure timeout


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


requireAssertWhen :: Testify ()
requireAssertWhen =
  do
    contract <- view preContract
    (_, contract') <- unAssert contract
    (_, timeout, _) <- unWhen contract'
    view minimumTime `requireLE` pure timeout
    view latestTime  `requireLT` pure timeout


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

instance Default TransactionTest where
  def =
    TransactionTest
    {
      name          = mempty
    , generator     = arbitrary
    , precondition  = pure ()
    , invariant     = mempty
    , postcondition = pure ()
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
  def
  {
    name          = "Detect invalid time interval"
  , generator     = makeInvalidInterval <$> arbitrary
  , precondition  = requireInvalidInterval
  , postcondition = view validTimes >>= hasError . TEIntervalError . InvalidInterval
  }


tooEarly :: TransactionTest
tooEarly =
  def
  {
    name          = "Detect time interval in past"
  , precondition  = requireInPast
  , postcondition = IntervalInPastError <$> view preTime <*> view validTimes >>= hasError . TEIntervalError
  }


ambiguousTimeout :: TransactionTest
ambiguousTimeout =
  def
  {
    name          = "Ambiguous interval for timeout"
  , precondition  = requireAmbiguousTimeout >> requireInputs (== 0)
  , postcondition = hasError TEAmbiguousTimeIntervalError
  }


uselessNoInput :: TransactionTest
uselessNoInput =
  def
  {
    name          = "Applying no inputs is useless until timeout"
  , precondition  = requireValidTime >> requireNotTimeout >> requireInputs (== 0)
  , postcondition = hasError TEUselessTransaction
  }


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


requireIncompatibleInput :: Testify ()
requireIncompatibleInput =
  do
    requireInputs (> 0)
    input <- getInputContent . head <$> view inputs
    (cs, _, _) <- unWhen =<< view preContract
    let matches (IDeposit _ _ _ _) (Deposit _ _ _ _) = True
        matches (IChoice _ _     ) (Choice _ _     ) = True
        matches  INotify           (Notify _       ) = True
        matches  _                  _                = False
    any (matches input . getAction) cs
      `when` throwError "Input may be compatible with action."


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


extractIf :: Testify Contract
extractIf =
  do
    (observation, thenContinuation, elseContinuation) <- unIf =<< view preContract
    observation' <- observe observation
    pure
      $ if observation'
          then thenContinuation
          else elseContinuation


extractAssert :: Testify ([TransactionWarning], Contract)
extractAssert =
  do
    (observation, continuation) <- unAssert =<< view preContract
    observation' <- observe observation
    pure
      (
        if observation'
          then []
          else [TransactionAssertionFailed]
      , continuation
      )


checkValues :: AM.Map ValueId Integer -> Testify ()
checkValues expected =
  (expected `assocMapEq`) <$> view postValues
    >>= (`unless` throwError "Mismatch in expected bound values.")


checkContinuation :: Contract -> Testify ()
checkContinuation expected =
  (expected ==) <$> view postContract
    >>= (`unless` throwError "Mismatch in expected contract.")


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
                      (shadowing ==) <$> view warnings
                        >>= (`unless` throwError "Erroneous shadowing warning.")
                      checkContinuation continuation
  }


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
                      (failing ==) <$> view warnings
                        >>= (`unless` throwError "Erroneous assertion warning.")
                      checkContinuation continuation
  }


anyInput :: TransactionTest
anyInput =
  def
  {
    name          = "Static-analysis input"
  , generator     = arbitraryValid
  , postcondition = view noError
  }


tests :: TestTree
tests =
  testGroup "Compute Transaction"
    $ fmap (test True)
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
    ]
