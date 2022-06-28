{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module Spec.Actus.QCTests
  ( tests )
where

import Actus.Core
import Actus.Domain.BusinessEvents
import Actus.Domain.ContractTerms
import Actus.Domain.Schedule
import Actus.Model.Applicability
import Data.Maybe (isJust)
import Data.Time (LocalTime)
import Data.Validation as V
import Spec.Actus.Haskell (TestContractTerms, TestRiskFactors, setDefaultContractTermValues)
import Spec.Actus.QCGenerator
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck

tests :: TestTree
tests = testGroup "QuickCheck"
  [ testProperty "Non empty cashflows" prop_non_empty
  , testProperty "Purchase event" prop_purchase
  , testProperty "Principal repayment (PAM)" prop_principal_payment
  ]

newtype ContractTermsQC = ContractTermsQC TestContractTerms deriving (Show)
newtype ContractTermsPAM = ContractTermsPAM TestContractTerms deriving (Show)
newtype ContractTermsLAM = ContractTermsLAM TestContractTerms deriving (Show)
newtype ContractTermsNAM = ContractTermsNAM TestContractTerms deriving (Show)
newtype ContractTermsANN = ContractTermsANN TestContractTerms deriving (Show)

instance Arbitrary ContractTermsQC where
  arbitrary = ContractTermsQC . setDefaultContractTermValues <$> contractTermsGen

instance Arbitrary ContractTermsPAM where
  arbitrary = ContractTermsPAM . setDefaultContractTermValues <$> contractTermsGen' PAM

instance Arbitrary ContractTermsLAM where
  arbitrary = ContractTermsLAM . setDefaultContractTermValues <$> contractTermsGen' LAM

instance Arbitrary ContractTermsNAM where
  arbitrary = ContractTermsNAM . setDefaultContractTermValues <$> contractTermsGen' NAM

instance Arbitrary ContractTermsANN where
  arbitrary = ContractTermsANN . setDefaultContractTermValues <$> contractTermsGen' ANN

validContract :: TestContractTerms -> Bool
validContract ct = case validateTerms ct of
  V.Success _ -> True
  V.Failure _ -> False

defaultRiskFactors :: EventType -> LocalTime -> TestRiskFactors
defaultRiskFactors _ _ =
  RiskFactors
    { o_rf_CURS = 1.0,
      o_rf_RRMO = 1.0,
      o_rf_SCMO = 1.0,
      pp_payoff = 0.0,
      xd_payoff = 0.0,
      dv_payoff = 0.0
    }

prop_non_empty :: ContractTermsQC -> Property
prop_non_empty (ContractTermsQC ct) =
  validContract ct
    ==> let cf = genProjectedCashflows defaultRiskFactors ct
         in not (null cf)

prop_purchase :: ContractTermsQC -> Property
prop_purchase (ContractTermsQC ct) =
  validContract ct && isJust (purchaseDate ct)
    ==> let cf = genProjectedCashflows defaultRiskFactors ct
         in PRD `elem` map cashEvent cf

prop_principal_payment :: ContractTermsPAM -> Property
prop_principal_payment (ContractTermsPAM ct) =
  validContract ct && isJust (cycleAnchorDateOfInterestPayment ct)
    ==> let cf =
              genProjectedCashflows
                defaultRiskFactors
                ct
                  { purchaseDate = Nothing,
                    terminationDate = Nothing,
                    notionalScalingMultiplier = Just 1.0,
                    interestScalingMultiplier = Just 1.0,
                    feeRate = Just 0.0,
                    premiumDiscountAtIED = Just 0.0,
                    accruedInterest = Just 0.0,
                    interestCalculationBaseA = Just 0.0,
                    interestCalculationBase = Nothing,
                    cycleAnchorDateOfScalingIndex = Nothing,
                    capitalizationEndDate = Nothing,
                    scalingEffect = Nothing,
                    feeAccrued = Nothing,
                    cycleOfScalingIndex = Nothing,
                    cycleOfRateReset = Nothing
                  }
            ied = sum $ map amount $ filter (\c -> cashEvent c == IED) cf
            md = sum $ map amount $ filter (\c -> cashEvent c == MD) cf
         in ied + md == 0.0
