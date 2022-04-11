module Test.Marlowe.Execution where

import Prologue

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Except (runExcept)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.DateTime.Instant (Instant, unInstant)
import Data.List as List
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Time.Duration (negateDuration)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut as UUID
import Effect.Exception (Error, error)
import Effect.Exception.Unsafe (unsafeThrow)
import Errors.Explain (explainString)
import Examples.PureScript.ZeroCouponBond as Loan
import Language.Marlowe.Client (ContractHistory(..))
import Marlowe.Execution.State
  ( currentStep
  , extractNamedActions
  , getAllPayments
  , isClosed
  , pendingTimeouts
  , restoreState
  )
import Marlowe.Execution.Types (NamedAction(..), State)
import Marlowe.Extended (resolveRelativeTimes, toCore)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Semantics
  ( Contract
  , Input(..)
  , MarloweData(..)
  , MarloweParams(..)
  , Party(..)
  , Payee(..)
  , Payment(..)
  , TimeInterval(..)
  , TransactionInput(..)
  , ada
  )
import Marlowe.Semantics as Semantic
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Time (unsafeInstantFromInt)
import Plutus.V1.Ledger.Address (Address(..))
import Plutus.V1.Ledger.Credential (Credential(..))
import Plutus.V1.Ledger.Time (POSIXTime(..))
import Test.Data.Marlowe (adaToken, adjustInstant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions
  ( expectError
  , shouldContain
  , shouldEqual
  , shouldSatisfy
  )

spec :: Spec Unit
spec = do
  restoreWithoutTx
  restoreWithEmptyTx
  restoreWithFirstInput

lender :: Party
lender = Role "Lender"

borrower :: Party
borrower = Role "Borrower"

restoreWithoutTx :: Spec Unit
restoreWithoutTx =
  describe "Restoring the Loan contract without any transaction" do
    let startTime = unsafeInstantFromInt 10

    it "should be waiting for the first action if the contract didn't timeouted"
      do
        -- Given
        let
          currentTime = startTime /+/ unsafeInstantFromInt 1
        -- When
        state <- restoreLoanState { currentTime, startTime }
          defaultLoanParameters
          []
        -- Then
        isClosed state `shouldEqual` false
        currentStep state `shouldEqual` 0
        getAllPayments state `shouldEqual` mempty
        pendingTimeouts state `shouldEqual` Nothing
        extractNamedActions currentTime state `shouldEqual`
          [ MakeDeposit
              lender
              lender
              adaToken
              defaultLoanParameters.amount
          ]
    it "should have a timeouted state if the first timeout passed" do
      -- Given
      let
        currentTime = startTime /+/ defaultLoanParameters.loanDeadline /+/
          unsafeInstantFromInt 1
      -- When
      state <- restoreLoanState { currentTime, startTime } defaultLoanParameters
        []
      -- Then
      isClosed state `shouldEqual` false
      currentStep state `shouldEqual` 1
      getAllPayments state `shouldEqual` mempty
      pendingTimeouts state `shouldSatisfy` isJust
      extractNamedActions currentTime state `shouldEqual`
        [ CloseContract
        ]

restoreWithEmptyTx :: Spec Unit
restoreWithEmptyTx = describe
  "Restoring the Loan contract with a single empty transaction"
  do
    let startTime = unsafeInstantFromInt 10

    it "should not be possible to have it commited before the first timeout" do
      -- Given
      let
        txInitialTime = startTime /+/ unsafeInstantFromInt 5
        txFinalTime = startTime /+/ defaultLoanParameters.loanDeadline /-/
          unsafeInstantFromInt 1
        currentTime = txInitialTime /+/ unsafeInstantFromInt 1
        confirmedTxs = [ mkEmptyTx txInitialTime txFinalTime ]
      -- When/Then
      -- This transaction should fail with the Error `TEUselessTransaction`. The idea behind this
      -- error is to avoid a DoS attack of useless transactions.
      expectError $ restoreLoanState { currentTime, startTime }
        defaultLoanParameters
        confirmedTxs

    it "should close the contract if it's after the first timeout" do
      -- Given
      let
        txInitialTime = startTime /+/ defaultLoanParameters.loanDeadline /+/
          unsafeInstantFromInt 1
        txFinalTime = txInitialTime /+/ unsafeInstantFromInt 10
        currentTime = txInitialTime /+/ unsafeInstantFromInt 1
        confirmedTxs = [ mkEmptyTx txInitialTime txFinalTime ]
      -- When
      state <- restoreLoanState { currentTime, startTime } defaultLoanParameters
        confirmedTxs
      -- Then
      isClosed state `shouldEqual` true
      currentStep state `shouldEqual` 1
      getAllPayments state `shouldEqual` mempty
      pendingTimeouts state `shouldEqual` Nothing
      extractNamedActions currentTime state `shouldEqual` []

restoreWithFirstInput :: Spec Unit
restoreWithFirstInput = describe
  "Restoring the Loan contract with the first valid transaction"
  do
    let startTime = unsafeInstantFromInt 10

    it "should advance to the second step if its applied before the deadline" do
      -- Given
      let
        txInitialTime = startTime /+/ unsafeInstantFromInt 5
        txFinalTime = startTime /+/ defaultLoanParameters.loanDeadline /-/
          unsafeInstantFromInt 1
        currentTime = txInitialTime /+/ unsafeInstantFromInt 1
        confirmedTxs =
          [ mkTx
              txInitialTime
              txFinalTime
              $ IDeposit lender lender adaToken defaultLoanParameters.amount
          ]
      -- When
      state <- restoreLoanState { currentTime, startTime } defaultLoanParameters
        confirmedTxs
      -- Then
      let
        firstPayment = Payment lender (Party borrower) $ ada
          defaultLoanParameters.amount

      isClosed state `shouldEqual` false
      currentStep state `shouldEqual` 1
      getAllPayments state `shouldContain` firstPayment
      pendingTimeouts state `shouldEqual` Nothing
      extractNamedActions currentTime state `shouldEqual`
        [ MakeDeposit
            borrower
            borrower
            adaToken
            (defaultLoanParameters.amount + defaultLoanParameters.interest)
        ]
    it
      "should fail to restore the state if the transaction is 'commited' as applied after the deadline"
      do
        -- Given
        let
          txInitialTime = startTime /+/ defaultLoanParameters.loanDeadline /+/
            unsafeInstantFromInt 1
          txFinalTime = txInitialTime /+/ unsafeInstantFromInt 100
          currentTime = txInitialTime /+/ unsafeInstantFromInt 1
          confirmedTxs =
            [ mkTx
                txInitialTime
                txFinalTime
                $ IDeposit lender lender adaToken defaultLoanParameters.amount
            ]
        expectError $ restoreLoanState { currentTime, startTime }
          defaultLoanParameters
          confirmedTxs

--------------------------------------------------------------------------------
type LoanParameters =
  { loanDeadline :: Instant
  , paybackDeadline :: Instant
  , amount :: BigInt
  , interest :: BigInt
  }

loan :: forall m. MonadThrow Error m => Instant -> LoanParameters -> m Contract
loan startTime params = case toCore filledContract of
  Nothing -> throwError $ error "Can't create instance of loan contract"
  Just contract -> pure contract
  where
  filledContract =
    resolveRelativeTimes startTime $
      fillTemplate
        ( TemplateContent
            { timeContent: Map.fromFoldable
                [ "Loan deadline" /\ params.loanDeadline
                , "Payback deadline" /\ params.paybackDeadline
                ]
            , valueContent: Map.fromFoldable
                [ "Amount" /\ params.amount
                , "Interest" /\ params.interest
                ]
            }
        )
        Loan.fullExtendedContract

defaultLoanParameters :: LoanParameters
defaultLoanParameters =
  { loanDeadline: unsafeInstantFromInt 600_000
  , paybackDeadline: unsafeInstantFromInt 1_500_000
  , amount: BigInt.fromInt 10_000_000
  , interest: BigInt.fromInt 1_000_000
  }

marloweParams :: MarloweParams
marloweParams =
  MarloweParams
    { rolePayoutValidatorHash: "someHash"
    , rolesCurrency: { unCurrencySymbol: "someCurrency" }
    }

scriptAddress :: Address
scriptAddress = Address
  { addressCredential: ScriptCredential "someCredential"
  , addressStakingCredential: Nothing
  }

emptyMarloweState :: Instant -> Semantic.State
emptyMarloweState minTime =
  Semantic.State
    { accounts: Map.empty
    , choices: Map.empty
    , boundValues: Map.empty
    , minTime: POSIXTime minTime
    }

mkEmptyTx :: Instant -> Instant -> TransactionInput
mkEmptyTx from to =
  TransactionInput
    { interval: TimeInterval (POSIXTime from) (POSIXTime to)
    , inputs: List.Nil
    }

mkTx :: Instant -> Instant -> Input -> TransactionInput
mkTx from to input =
  TransactionInput
    { interval: TimeInterval (POSIXTime from) (POSIXTime to)
    , inputs: List.singleton input
    }

restoreLoanState
  :: forall m
   . MonadThrow Error m
  => { currentTime :: Instant, startTime :: Instant }
  -> LoanParameters
  -> Array TransactionInput
  -> m State
restoreLoanState { currentTime, startTime } loanParams chHistory = do
  contract <- loan startTime loanParams
  let
    contractHistory = mkContractHistory contract
    mState = restoreState
      (PlutusAppId UUID.emptyUUID)
      currentTime
      Nothing
      Loan.metaData
      contractHistory
  case mState of
    Left err -> throwError $ error $ "Can't restore state: " <> explainString
      err
    Right state -> pure state
  where
  mkContractHistory contract = ContractHistory
    { chParams: marloweParams
    , chInitialData:
        MarloweData
          { marloweContract: contract
          , marloweState: emptyMarloweState startTime
          }
    , chHistory
    , chAddress: scriptAddress
    }

infixl 6 unsafeAddInstant as /+/

unsafeAddInstant :: Instant -> Instant -> Instant
unsafeAddInstant a b =
  case runExcept $ adjustInstant (unInstant a) b of
    Left _ -> unsafeThrow $ "Cannot add " <> show a <> " + " <> show b
    Right i -> i

infixl 6 unsafeRemoveInstant as /-/

unsafeRemoveInstant :: Instant -> Instant -> Instant
unsafeRemoveInstant a b =
  case runExcept $ adjustInstant (negateDuration $ unInstant b) a of
    Left _ -> unsafeThrow $ "Cannot remove " <> show a <> " - " <> show b
    Right i -> i
