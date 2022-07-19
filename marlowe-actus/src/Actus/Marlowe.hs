{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

-- | = Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Actus.Marlowe
  ( constant,
    letval,
    useval,
    genContract,
    genContract',
    CashFlowMarlowe,
    ContractTermsMarlowe,
    RiskFactorsMarlowe,

    -- == Conversion from Double to Marlowe representation
    -- re-export
    module Actus.Domain,
    genProjectedCashflows,

    -- utility
    toMarlowe
  )
where

import Actus.Core (genProjectedCashflows)
import Actus.Domain
import Actus.Marlowe.Instance (CashFlowMarlowe, ContractTermsMarlowe, RiskFactorsMarlowe, reduceContract)
import Actus.Model (validateTerms)
import Data.List as L (foldl')
import Data.String (IsString (fromString))
import Data.Time (LocalTime (..), UTCTime (UTCTime), timeOfDayToTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), utcToSystemTime)
import Data.Validation (Validation (..))
import Language.Marlowe (Action (..), Case (..), Contract (..), Observation (..), POSIXTime (..), Party (..),
                         Payee (..), Value (..), ValueId (ValueId), ada)
import Ledger.Value (TokenName (TokenName))

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
    gen :: Contract -> CashFlow (Value Observation) -> Contract
    gen cont CashFlow {..} =
      let t = POSIXTime $ timeToSlotNumber cashPaymentDay
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

    invoice :: String -> String -> Value Observation -> POSIXTime -> Contract -> Contract
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

useval :: String -> Integer -> Value Observation
useval name t = UseValue $ ValueId $ fromString $ name ++ "_" ++ show t

letval :: String -> Integer -> Value Observation -> Contract -> Contract
letval name t = Let $ ValueId $ fromString $ name ++ "_" ++ show t

toMarloweFixedPoint :: Double -> Integer
toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint Prelude.*)

constant :: Double -> Value Observation
constant = Constant . toMarloweFixedPoint

cardanoEpochStart :: Integer
cardanoEpochStart = 100

timeToSlotNumber :: LocalTime -> Integer
timeToSlotNumber LocalTime {..} =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in fromIntegral secs Prelude.- cardanoEpochStart

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
    trans :: ContractStructure Double -> ContractStructure (Value Observation)
    trans cs =
      cs
        { reference = case reference cs of
            ReferenceId r    -> ReferenceId r
            ReferenceTerms t -> ReferenceTerms $ toMarlowe t
        }
