{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

-- | = ACTUS Generator
--
-- Given ACTUS contract terms a Marlowe contract is generated.
-- With 'genStaticContract' the risk factors are all known at contract creation
module Actus.Generator
  ( constant,
    genStaticContract,
    genFsContract,
    genFsContract',
    toMarlowe
  )
where

import Actus.Core (genProjectedCashflows)
import Actus.Domain.BusinessEvents (EventType (..))
import Actus.Domain.ContractTerms (ContractStructure (..), ContractTermsPoly (..), Reference (..),
                                   TermValidationError (..))
import Actus.Domain.Ops (ActusOps (..), marloweFixedPoint)
import Actus.Domain.Schedule (CashFlowPoly (..))
import Actus.Haskell (ContractTerms, RiskFactors)
import Actus.Marlowe (ContractTermsMarlowe, RiskFactorsMarlowe, reduceContract, reduceValue)
import Actus.Model.Applicability (validateTerms)
import Data.List as L (foldl')
import Data.String (IsString (fromString))
import Data.Time (Day, LocalTime (..), UTCTime (UTCTime), timeOfDayToTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), utcToSystemTime)
import Data.Validation (Validation (..))
import Language.Marlowe (Action (..), Case (..), Contract (..), Observation (..), POSIXTime (..), Party (..),
                         Payee (..), Value (..), ValueId (ValueId), ada)
import Ledger.Value (TokenName (TokenName))

-- | 'genStaticContract' validates the contract terms in order to generate a
--  Marlowe contract with risk factors known in advance. The contract therefore
--  only consists of transactions, i.e. 'Deposit' and 'Pay'
genStaticContract ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactors) ->
  -- | ACTUS contract terms
  ContractTerms ->
  -- | Marlowe contract or applicability errors
  Validation [TermValidationError] Contract
genStaticContract rf = fmap (genStaticContract' rf) . validateTerms

-- | Same as 'genStaticContract' without validation
genStaticContract' ::
  (EventType -> LocalTime -> RiskFactors) ->
  ContractTerms ->
  Contract
genStaticContract' rf ct =
  let cfs = genProjectedCashflows rf ct
      gen CashFlowPoly {..}
        | amount == 0.0 = id
        | amount > 0.0 =
          invoice
            "party"
            "counterparty"
            (Constant $ round amount)
            (POSIXTime $ timeToSlotNumber cashPaymentDay)
        | otherwise =
          invoice
            "counterparty"
            "party"
            (Constant $ round $ - amount)
            (POSIXTime $ timeToSlotNumber cashPaymentDay)
   in foldl' (flip gen) Close $ reverse cfs

-- | 'genFsContract' validatates the applicabilty of the contract terms in order
--  to genereate a Marlowe contract with risk factors observed at a given point
--  in time
genFsContract ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  -- | ACTUS contract terms
  ContractTermsMarlowe ->
  -- | Marlowe contract or applicabilty errors
  Validation [TermValidationError] Contract
genFsContract rf = fmap (genFsContract' rf) . validateTerms

genFsContract' ::
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  ContractTermsMarlowe ->
  Contract
genFsContract' rf ct =
  let cfs = genProjectedCashflows rf ct

      gen :: CashFlowPoly (Value Observation) -> Contract -> Contract
      gen CashFlowPoly {..} cont =
        let t = POSIXTime $ timeToSlotNumber cashPaymentDay
            a = reduceValue $ DivValue amount (Constant marloweFixedPoint)
         in reduceContract $
              If
                (_zero `ValueLT` a)
                ( invoice
                    "party"
                    "counterparty"
                    a
                    t
                    cont
                )
                ( If
                    (a `ValueLT` _zero)
                    ( invoice
                        "counterparty"
                        "party"
                        (NegValue a)
                        t
                        cont
                    )
                    cont
                )
   in foldl' (flip gen) Close $ reverse cfs

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

letval' :: String -> Integer -> Maybe (Value Observation) -> Contract -> Contract
letval' name t (Just o) c = letval name t o c
letval' _ _ Nothing c     = c

toMarloweFixedPoint :: Double -> Integer
toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint *)

constant :: Double -> Value Observation
constant = Constant . toMarloweFixedPoint

enum :: a -> a
enum = id

cardanoEpochStart :: Integer
cardanoEpochStart = 100

dayToSlotNumber :: Day -> Integer
dayToSlotNumber d =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime d 0)
   in fromIntegral secs - cardanoEpochStart

timeToSlotNumber :: LocalTime -> Integer
timeToSlotNumber LocalTime {..} =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in fromIntegral secs - cardanoEpochStart

marloweDate :: Day -> Value Observation
marloweDate = Constant . fromInteger . dayToSlotNumber

marloweTime :: LocalTime -> Value Observation
marloweTime = Constant . fromInteger . timeToSlotNumber

toMarlowe :: ContractTerms -> ContractTermsMarlowe
toMarlowe ct =
  ContractTermsPoly
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
