{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}

-- | = Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Actus.Marlowe
  ( genContract,
    genContract',
    defaultRiskFactors,
    CashFlowMarlowe,
    ContractTermsMarlowe,
    RiskFactorsMarlowe,

    -- == Conversion from Double to Marlowe representation
    -- re-export
    module Actus.Domain,
    genProjectedCashflows,

    -- utility
    toMarlowe,
    toMarloweFixedPoint,
    fromMarloweFixedPoint
  )
where

import Actus.Core (genProjectedCashflows)
import Actus.Domain
import Actus.Marlowe.Instance (CashFlowMarlowe, ContractTermsMarlowe, RiskFactorsMarlowe, fromMarloweFixedPoint,
                               reduceContract, toMarloweFixedPoint)
import Actus.Model (validateTerms)
import Data.List as L (foldl')
import Data.String (IsString (fromString))
import Data.Time (LocalTime (..), UTCTime (UTCTime), timeOfDayToTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), utcToSystemTime)
import Data.Validation (Validation (..))
import Language.Marlowe.Extended.V1
import Ledger.Value (TokenName (TokenName))
import PlutusTx.Builtins.Class (stringToBuiltinByteString)

-- | 'genContract' validatates the applicabilty of the contract terms in order
-- to genereate a Marlowe contract with risk factors observed at a given point
-- in time
genContract ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  -- | ACTUS contract terms
  ContractTermsMarlowe ->
  -- | Marlowe contract or applicabilty errors
  Validation [TermValidationError] Contract
genContract rf = fmap (genContract' rf) . validateTerms

-- | Same as 'getContract', but does not validate the applicabilty of the contract
-- terms.
genContract' ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  -- | ACTUS contract terms
  ContractTermsMarlowe ->
  -- | Marlowe contract
  Contract
genContract' rf ct =
  let cfs = genProjectedCashflows rf ct []
   in foldl' gen Close $ reverse cfs
  where
    gen :: Contract -> CashFlow Value -> Contract
    gen cont cf@CashFlow {..}
      | hasRiskFactor cf =
        When
          [ Case
              (Choice (cashFlowToChoiceId cf) [Bound 0 100_000_000_000])
              (stub cont cf)
          ]
          (toTimeout cashPaymentDay)
          Close
    gen cont cf = stub cont cf

    stub cont CashFlow {..} =
      let t = toTimeout cashPaymentDay
          c = reduceContract cont
       in reduceContract $
            If
              (0 `ValueLT` amount)
              ( invoice
                  "party"
                  "counterparty"
                  amount
                  t
                  c
              )
              ( If
                  (amount `ValueLT` 0)
                  ( invoice
                      "counterparty"
                      "party"
                      (NegValue amount)
                      t
                      c
                  )
                  c
              )

    invoice :: String -> String -> Value -> Timeout -> Contract -> Contract
    invoice from to amount timeout continue =
      let party = Role $ TokenName $ fromString from
          counterparty = Role $ TokenName $ fromString to
       in When
            [ Case
                (Deposit party party ada amount)
                ( Pay
                    party
                    (Party counterparty)
                    ada
                    amount
                    continue
                )
            ]
            timeout
            Close

cashFlowToChoiceId :: CashFlow a -> ChoiceId
cashFlowToChoiceId CashFlow {..} =
  let l = show cashEvent <> show cashPaymentDay
   in ChoiceId (stringToBuiltinByteString l) (Role "RiskFactor")

hasRiskFactor :: CashFlow Value -> Bool
hasRiskFactor cf = hasRiskFactor' (amount cf)
  where
    hasRiskFactor' :: Value -> Bool
    hasRiskFactor' (ChoiceValue j) | cashFlowToChoiceId cf == j = True
    hasRiskFactor' (ChoiceValue _) = False
    hasRiskFactor' (Constant _) = False
    hasRiskFactor' (AvailableMoney _ _) = False
    hasRiskFactor' (UseValue _) = False
    hasRiskFactor' (AddValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (SubValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (MulValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (DivValue a b) = hasRiskFactor' a || hasRiskFactor' b
    hasRiskFactor' (NegValue a) = hasRiskFactor' a
    hasRiskFactor' TimeIntervalStart = False
    hasRiskFactor' TimeIntervalEnd = False
    hasRiskFactor' (ConstantParam _) = False
    hasRiskFactor' (Cond _ a b) = hasRiskFactor' a || hasRiskFactor' b

defaultRiskFactors :: EventType -> LocalTime -> RiskFactors Value
defaultRiskFactors ev t =
  let choiceId = ChoiceId (stringToBuiltinByteString $ show ev <> show t) (Role "RiskFactor")
      value = ChoiceValue choiceId
  in mkRiskFactor ev value

mkRiskFactor :: EventType -> Value -> RiskFactors Value
mkRiskFactor PP value =
  RiskFactors
    { o_rf_CURS = 1,
      o_rf_RRMO = 1,
      o_rf_SCMO = 1,
      pp_payoff = value,
      xd_payoff = 0,
      dv_payoff = 0
    }
mkRiskFactor XD value =
  RiskFactors
    { o_rf_CURS = 1,
      o_rf_RRMO = 1,
      o_rf_SCMO = 1,
      pp_payoff = 0,
      xd_payoff = value,
      dv_payoff = 0
    }
mkRiskFactor DV value =
  RiskFactors
    { o_rf_CURS = 1,
      o_rf_RRMO = 1,
      o_rf_SCMO = 1,
      pp_payoff = 0,
      xd_payoff = 0,
      dv_payoff = value
    }
mkRiskFactor _ _ =
  RiskFactors
    { o_rf_CURS = 1,
      o_rf_RRMO = 1,
      o_rf_SCMO = 1,
      pp_payoff = 0,
      xd_payoff = 0,
      dv_payoff = 0
    }

constant :: Double -> Value
constant = Constant . toMarloweFixedPoint

toTimeout :: LocalTime -> Timeout
toTimeout LocalTime {..} =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in POSIXTime (toInteger secs)

toMarlowe :: ContractTerms Double -> ContractTermsMarlowe
toMarlowe ct =
  ContractTerms
    { contractId = contractId ct,
      contractType = contractType ct,
      contractStructure = map trans (contractStructure ct),
      contractRole = contractRole ct,
      settlementCurrency = settlementCurrency ct,
      initialExchangeDate = initialExchangeDate ct,
      dayCountConvention = dayCountConvention ct,
      scheduleConfig = scheduleConfig ct,
      statusDate = statusDate ct,
      marketObjectCodeRef = Nothing,
      contractPerformance = contractPerformance ct,
      creditEventTypeCovered = creditEventTypeCovered ct,
      coverageOfCreditEnhancement = constant <$> coverageOfCreditEnhancement ct,
      guaranteedExposure = guaranteedExposure ct,
      cycleOfFee = cycleOfFee ct,
      cycleAnchorDateOfFee = cycleAnchorDateOfFee ct,
      feeAccrued = constant <$> feeAccrued ct,
      feeBasis = feeBasis ct,
      feeRate = constant <$> feeRate ct,
      cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment ct,
      cycleOfInterestPayment = cycleOfInterestPayment ct,
      accruedInterest = constant <$> accruedInterest ct,
      capitalizationEndDate = capitalizationEndDate ct,
      cycleAnchorDateOfInterestCalculationBase = cycleAnchorDateOfInterestCalculationBase ct,
      cycleOfInterestCalculationBase = cycleOfInterestCalculationBase ct,
      interestCalculationBase = interestCalculationBase ct,
      interestCalculationBaseA = constant <$> interestCalculationBaseA ct,
      nominalInterestRate = constant <$> nominalInterestRate ct,
      nominalInterestRate2 = constant <$> nominalInterestRate2 ct,
      interestScalingMultiplier = constant <$> interestScalingMultiplier ct,
      notionalPrincipal = constant <$> notionalPrincipal ct,
      premiumDiscountAtIED = constant <$> premiumDiscountAtIED ct,
      maturityDate = maturityDate ct,
      amortizationDate = amortizationDate ct,
      exerciseDate = exerciseDate ct,
      cycleAnchorDateOfPrincipalRedemption = cycleAnchorDateOfPrincipalRedemption ct,
      cycleOfPrincipalRedemption = cycleOfPrincipalRedemption ct,
      nextPrincipalRedemptionPayment = constant <$> nextPrincipalRedemptionPayment ct,
      purchaseDate = purchaseDate ct,
      priceAtPurchaseDate = constant <$> priceAtPurchaseDate ct,
      terminationDate = terminationDate ct,
      priceAtTerminationDate = constant <$> priceAtTerminationDate ct,
      quantity = constant <$> quantity ct,
      currency = currency ct,
      currency2 = currency2 ct,
      scalingIndexAtStatusDate = constant <$> scalingIndexAtStatusDate ct,
      cycleAnchorDateOfScalingIndex = cycleAnchorDateOfScalingIndex ct,
      cycleOfScalingIndex = cycleOfScalingIndex ct,
      scalingEffect = scalingEffect ct,
      scalingIndexAtContractDealDate = constant <$> scalingIndexAtContractDealDate ct,
      marketObjectCodeOfScalingIndex = marketObjectCodeOfScalingIndex ct,
      notionalScalingMultiplier = constant <$> notionalScalingMultiplier ct,
      cycleOfOptionality = cycleOfOptionality ct,
      cycleAnchorDateOfOptionality = cycleAnchorDateOfOptionality ct,
      optionType = optionType ct,
      optionStrike1 = constant <$> optionStrike1 ct,
      optionExerciseType = optionExerciseType ct,
      settlementPeriod = settlementPeriod ct,
      deliverySettlement = deliverySettlement ct,
      exerciseAmount = constant <$> exerciseAmount ct,
      futuresPrice = constant <$> futuresPrice ct,
      penaltyRate = constant <$> penaltyRate ct,
      penaltyType = penaltyType ct,
      prepaymentEffect = prepaymentEffect ct,
      cycleOfRateReset = cycleOfRateReset ct,
      cycleAnchorDateOfRateReset = cycleAnchorDateOfRateReset ct,
      nextResetRate = constant <$> nextResetRate ct,
      rateSpread = constant <$> rateSpread ct,
      rateMultiplier = constant <$> rateMultiplier ct,
      periodFloor = constant <$> periodFloor ct,
      periodCap = constant <$> periodCap ct,
      lifeCap = constant <$> lifeCap ct,
      lifeFloor = constant <$> lifeFloor ct,
      marketObjectCodeOfRateReset = marketObjectCodeOfRateReset ct,
      cycleOfDividend = cycleOfDividend ct,
      cycleAnchorDateOfDividend = cycleAnchorDateOfDividend ct,
      nextDividendPaymentAmount = constant <$> nextDividendPaymentAmount ct,
      enableSettlement = enableSettlement ct,
      constraints = constraints ct
    }
  where
    trans :: ContractStructure Double -> ContractStructure Value
    trans cs =
      cs
        { reference = case reference cs of
            ReferenceId r    -> ReferenceId r
            ReferenceTerms t -> ReferenceTerms $ toMarlowe t
        }
