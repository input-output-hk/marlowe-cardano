-- TODO: Move these tests to marlowe-commons
module Marlowe.ContractTests where

import Prologue

import Control.Bind (bindFlipped)
import Control.Monad.Gen (class MonadGen, chooseInt, elements, oneOf)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (StateT, evalStateT, execState, get, gets, lift)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Either (hush)
import Data.Int (round)
import Data.Lens (_Just, preview, previewOn, set, (^.))
import Data.Lens.NonEmptyList (_Head)
import Data.List.NonEmpty as NEL
import Data.Map as Map
import Data.Maybe (fromJust)
import Data.NonEmpty ((:|))
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested ((/\))
import Examples.Marlowe.Contracts as Contracts
import Examples.PureScript.ContractForDifferences as ContractForDifferences
import Examples.PureScript.ContractForDifferencesWithOracle as ContractForDifferencesWithOracle
import Examples.PureScript.CouponBondGuaranteed as CouponBondGuaranteed
import Examples.PureScript.Escrow as Escrow
import Examples.PureScript.EscrowWithCollateral as EscrowWithCollateral
import Examples.PureScript.Swap as Swap
import Examples.PureScript.ZeroCouponBond as ZeroCouponBond
import Language.Marlowe.Core.V1.Semantics.Types
  ( Bound(..)
  , ChoiceId(..)
  , Contract(..)
  , Input(..)
  , Party(..)
  , Token(..)
  , TransactionError
  , TransactionWarning
  )
import Language.Marlowe.Extended.V1 (resolveRelativeTimes)
import Language.Marlowe.Extended.V1 as EM
import Language.Marlowe.Extended.V1.Metadata (emptyContractMetadata)
import Marlowe.Holes (Term(..), fromTerm)
import Marlowe.Holes as T
import Marlowe.Parser (parseContract)
import Marlowe.Template (TemplateContent(..), fillTemplate)
import Marlowe.Time (secondsSinceShelley, shelleyEpoch, unixEpoch)
import Page.Simulation.State (mkStateBase)
import Page.Simulation.Types as Simulation
import Partial.Unsafe (unsafePartial)
import Record (insert) as Record
import Simulator.Lenses
  ( _SimulationRunning
  , _currentContract
  , _currentMarloweState
  , _currentPossibleActions
  , _executionState
  , _marloweState
  , _transactionError
  , _transactionWarnings
  )
import Simulator.State
  ( advanceTime
  , applyInput
  , getAllActions
  , initialMarloweState
  , startSimulation
  )
import Simulator.Types (ActionInput(..))
import Test.QuickCheck (Result(..))
import Test.QuickCheck.Gen (Gen)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.QuickCheck (quickCheck)
import Text.Pretty (pretty)
import Type.Prelude (Proxy(..))

mkState :: Term T.Contract -> Simulation.State
mkState contract =
  let
    baseState = Record.insert
      (Proxy :: Proxy "projectName")
      "Contract"
      (mkStateBase $ Minutes 0.0)
  in
    set
      _marloweState
      ( NEL.singleton $ initialMarloweState unixEpoch contract
          emptyContractMetadata
      )
      baseState

all :: Spec Unit
all =
  describe "Contract Tests" do
    examplesMatch
    escrowSimpleFlow
    exampleContractsHaveNoErrors

-- We don't currently have a function that goes from semantic contract to term contract, so for the purposes
-- of these test we print it and parse it.
toTerm :: EM.Contract -> Term T.Contract
toTerm contract = unsafePartial
  $ fromJust
  $ hush
  $ parseContract
  $ show
  $ pretty
  $ resolveRelativeTimes shelleyEpoch contract

contractToExtended :: String -> Maybe EM.Contract
contractToExtended = fromTerm <=< hush <<< parseContract

examplesMatch :: Spec Unit
examplesMatch =
  describe "Purescript and Haskell examples match" do
    it "Simple escrow"
      $ shouldEqual (Just Escrow.fullExtendedContract)
          (contractToExtended Contracts.escrow)
    it "Escrow with collateral"
      $ shouldEqual (Just EscrowWithCollateral.fullExtendedContract)
          (contractToExtended Contracts.escrowWithCollateral)
    it "Zero coupon bond"
      $ shouldEqual (Just ZeroCouponBond.fullExtendedContract)
          (contractToExtended Contracts.zeroCouponBond)
    it "Coupon bond guaranteed"
      $ shouldEqual (Just CouponBondGuaranteed.contract)
          (contractToExtended Contracts.couponBondGuaranteed)
    it "Swap"
      $ shouldEqual (Just Swap.fullExtendedContract)
          (contractToExtended Contracts.swap)
    it "Contract for differences"
      $ shouldEqual (Just ContractForDifferences.contract)
          (contractToExtended Contracts.contractForDifferences)
    it "Contract for differences with oracle"
      $ shouldEqual (Just ContractForDifferencesWithOracle.contract)
          (contractToExtended Contracts.contractForDifferencesWithOracle)

seller :: Party
seller = Role "Seller"

buyer :: Party
buyer = Role "Buyer"

arbiter :: Party
arbiter = Role "Mediator"

ada :: Token
ada = Token "" ""

filledEscrow :: Term T.Contract
filledEscrow =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent:
                Map.fromFoldable
                  [ "Payment deadline" /\ secondsSinceShelley 10
                  , "Complaint deadline" /\ secondsSinceShelley 50
                  , "Complaint response deadline" /\ secondsSinceShelley 100
                  , "Mediation deadline" /\ secondsSinceShelley 1000
                  ]
            , valueContent:
                Map.fromFoldable
                  [ "Price" /\ BigInt.fromInt 450
                  ]
            }
        )
        Escrow.fullExtendedContract
    )

filledEscrowWithCollateral :: Term T.Contract
filledEscrowWithCollateral =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent: Map.empty
            , valueContent:
                Map.fromFoldable
                  [ "Price" /\ BigInt.fromInt 450
                  , "Collateral amount" /\ BigInt.fromInt 500
                  ]
            }
        )
        EscrowWithCollateral.fixedTimeoutContract
    )

filledZeroCouponBond :: Term T.Contract
filledZeroCouponBond =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent:
                Map.fromFoldable
                  [ "Interest" /\ secondsSinceShelley 100
                  , "Amount" /\ secondsSinceShelley 200
                  ]
            , valueContent:
                Map.fromFoldable
                  [ "Interest" /\ BigInt.fromInt 100
                  , "Amount" /\ BigInt.fromInt 200
                  ]
            }
        )
        ZeroCouponBond.fixedTimeoutContract
    )

filledCouponBondGuaranteed :: Term T.Contract
filledCouponBondGuaranteed =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent: Map.empty
            , valueContent:
                Map.fromFoldable
                  [ "Interest instalment" /\ BigInt.fromInt 100
                  , "Principal" /\ BigInt.fromInt 200
                  ]
            }
        )
        CouponBondGuaranteed.contract
    )

filledSwap :: Term T.Contract
filledSwap =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent: Map.empty
            , valueContent:
                Map.fromFoldable
                  [ "Amount of Ada" /\ BigInt.fromInt 1500000
                  , "Amount of dollars" /\ BigInt.fromInt 1
                  ]
            }
        )
        Swap.fixedTimeoutContract
    )

filledContractForDifferences :: Term T.Contract
filledContractForDifferences =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent:
                Map.fromFoldable
                  [ "Party deposit deadline" /\ secondsSinceShelley 10
                  , "Counterparty deposit deadline" /\ secondsSinceShelley 20
                  , "First window beginning" /\ secondsSinceShelley 30
                  , "First window deadline" /\ secondsSinceShelley 40
                  , "Second window beginning" /\ secondsSinceShelley 100
                  , "Second window deadline" /\ secondsSinceShelley 110
                  ]
            , valueContent:
                Map.fromFoldable
                  [ "Amount paid by party" /\ BigInt.fromInt 100000000
                  , "Amount paid by counterparty" /\ BigInt.fromInt 100000000
                  ]
            }
        )
        ContractForDifferences.contract
    )

filledContractForDifferencesWithOracle :: Term T.Contract
filledContractForDifferencesWithOracle =
  toTerm
    ( fillTemplate
        ( TemplateContent
            { timeContent:
                Map.fromFoldable
                  [ "Party deposit deadline" /\ secondsSinceShelley 10
                  , "Counterparty deposit deadline" /\ secondsSinceShelley 20
                  , "First window beginning" /\ secondsSinceShelley 30
                  , "First window deadline" /\ secondsSinceShelley 40
                  , "Second window beginning" /\ secondsSinceShelley 100
                  , "Second window deadline" /\ secondsSinceShelley 110
                  ]
            , valueContent:
                Map.fromFoldable
                  [ "Amount paid by party" /\ BigInt.fromInt 100000000
                  , "Amount paid by counterparty" /\ BigInt.fromInt 100000000
                  , "Amount of Ada to use as asset" /\ BigInt.fromInt 100000000
                  ]
            }
        )
        ContractForDifferencesWithOracle.contract
    )

-- TODO:  We should combine this test with the ones defined in Marlowe.Holes.SemanticTest
--       so that we can have a single definition of contracts and flows, and then test what we care in each one. In semantic
--       test we care that the compute transaction of term and semantic are the same, in here we care about the output of the simulation.
escrowSimpleFlow :: Spec Unit
escrowSimpleFlow =
  it "Escrow simple flow" do
    -- A simple test that runs the Escrow contract to completion
    let
      deposit = IDeposit seller buyer ada (BigInt.fromInt 450)

      choice1 = IChoice ((ChoiceId "Report problem") buyer) (BigInt.fromInt 1)

      choice2 = IChoice ((ChoiceId "Confirm problem") seller) (BigInt.fromInt 1)

      finalState =
        (flip execState (mkState filledEscrow)) do
          startSimulation unixEpoch filledEscrow
          applyInput deposit
          applyInput choice1
          applyInput choice2

      finalContract = previewOn finalState _currentContract

      txError = do
        executionState <- preview
          (_marloweState <<< _Head <<< _executionState <<< _SimulationRunning)
          finalState
        executionState ^. _transactionError
    shouldEqual Nothing txError
    shouldEqual (Just Close) (fromTerm =<< finalContract)
    pure unit

--
exampleContractsHaveNoErrors :: Spec Unit
exampleContractsHaveNoErrors =
  describe "Provided Examples don't throw errors nor have warnings" do
    contractHasNoErrors "Simple Escrow" filledEscrow
    contractHasNoErrors "Escrow with collateral" filledEscrowWithCollateral
    contractHasNoErrors "Zero coupon bond" filledZeroCouponBond
    contractHasNoErrors "Coupon bond guaranteed" filledCouponBondGuaranteed
    contractHasNoErrors "Swap" filledSwap
    contractHasNoErrors "Contract for differences" filledContractForDifferences
    contractHasNoErrors "Contract for differences with oracle"
      filledContractForDifferencesWithOracle

-- This is a property based test that checks that for a given contract, the possible actions available
-- during the simulation don't throw errors nor warnings.
contractHasNoErrors :: String -> Term T.Contract -> Spec Unit
contractHasNoErrors contractName contract =
  it contractName
    $ quickCheck
    $ evalStateT property (mkState contract)
  where
  property :: StateT Simulation.State Gen Result
  property = do
    startSimulation unixEpoch contract
    runContract (Just contract)

  -- When the possible action is a Choose, we need to generate a random value inside the provided Bound.
  -- We use this function (which looses precision) in order to select a small integer and wrap it in a BigInt
  looselyToInt :: BigInt -> Int
  looselyToInt = round <<< BigInt.toNumber

  genValueInBound :: forall m. MonadGen m => MonadRec m => Bound -> m BigInt
  genValueInBound (Bound from to) = BigInt.fromInt <$> chooseInt
    (looselyToInt from)
    (looselyToInt to)

  -- TODO: For the moment it was not needed, as none of the examples triggered a warning nor an error
  --       but we should add an extra parameter to runContract that includes the current Action list
  --       and in case of a failure, we should print what is the list that causes the problem.
  runContract :: Maybe (Term T.Contract) -> StateT Simulation.State Gen Result
  runContract Nothing = pure $ Failed "Cant get contract to test"

  runContract (Just (Term T.Close _)) = pure Success

  runContract (Just _) = do
    simulationState <- get
    let
      mPossibleActions :: Maybe (NonEmptyArray ActionInput)
      mPossibleActions =
        simulationState
          # preview _currentPossibleActions
          # map getAllActions
          # bindFlipped fromArray
    case mPossibleActions of
      Nothing -> pure $ Failed "no available actions"
      Just possibleActions -> do
        action <- lift $ elements possibleActions
        mRunActionResult <- case action of
          MoveToTime _ newTime -> runMaybeT $ advanceTime newTime
          DepositInput accountId party token value -> pure
            <$> applyInput (IDeposit accountId party token value)
          ChoiceInput choiceId bounds value -> pure <$> do
            randomValue <- lift $ oneOf $ pure value :|
              (genValueInBound <$> bounds)
            applyInput $ IChoice choiceId randomValue
          NotifyInput -> pure <$> applyInput INotify
        (mError :: Maybe TransactionError) <- gets $ preview
          ( _currentMarloweState <<< _executionState <<< _SimulationRunning
              <<< _transactionError
              <<< _Just
          )
        (mWarnings :: Maybe (Array TransactionWarning)) <- gets $ preview
          ( _currentMarloweState <<< _executionState <<< _SimulationRunning <<<
              _transactionWarnings
          )
        mContract <- gets (preview _currentContract)
        case mRunActionResult, mError, mWarnings of
          -- TODO: If we run into this message, we'll need to implement the refactor described in runContract to know how to reach this state
          Just _, Nothing, Just [] -> runContract mContract
          Just _, Just _, _ -> pure $ Failed "it has errors"
          Just _, _, Just _ -> pure $ Failed "it has warnings"
          _, _, _ -> pure $ Failed "Wrong simulation state"
