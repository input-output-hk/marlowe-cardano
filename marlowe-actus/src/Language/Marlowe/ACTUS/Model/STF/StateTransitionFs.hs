{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.STF.StateTransitionFs
  (
    stateTransition
  )
where

import Control.Monad.Reader
import Data.Maybe (fromMaybe, maybeToList)
import Data.Time (LocalTime)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsMarlowe)
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStateMarlowe)
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTerms, ContractTermsMarlowe,
                                                    ContractTermsPoly (..))
import Language.Marlowe.ACTUS.Domain.Ops (YearFractionOps (_y))
import Language.Marlowe.ACTUS.Generator.MarloweCompat (constnt, marloweTime)
import Language.Marlowe.ACTUS.Model.STF.StateTransition (CtxSTF (..))
import Language.Marlowe.ACTUS.Model.STF.StateTransitionModel
import Language.Marlowe.ACTUS.Utility.ScheduleGenerator (inf', sup')

stateTransition :: EventType -> RiskFactorsMarlowe -> LocalTime -> LocalTime -> ContractStateMarlowe -> Reader (CtxSTF Double LocalTime) ContractStateMarlowe
stateTransition ev rf prevDate t st = reader stateTransitionFs'
  where
    stateTransitionFs' CtxSTF{ contractTerms = ct'@ContractTermsPoly {maturityDate = md, dayCountConvention = Just dcc, cycleAnchorDateOfInterestPayment = Just ipanx}, ..} =
        stf ev (toMarlowe ct')
        where
          stf AD _ = _STF_AD_PAM st time y_sd_t
          stf IED ct@ContractTermsPoly {contractType = PAM} = _STF_IED_PAM ct st time y_ipanx_t
          stf IED ct@ContractTermsPoly {contractType = LAM} = _STF_IED_LAM ct st time y_ipanx_t
          stf IED ct@ContractTermsPoly {contractType = NAM} = _STF_IED_LAM ct st time y_ipanx_t
          stf IED ct@ContractTermsPoly {contractType = ANN} = _STF_IED_LAM ct st time y_ipanx_t
          stf PR ct@ContractTermsPoly {contractType = LAM} = _STF_PR_LAM ct st time y_sd_t
          stf PR ct@ContractTermsPoly {contractType = NAM} = _STF_PR_NAM ct st time y_sd_t
          stf PR ct@ContractTermsPoly {contractType = ANN} = _STF_PR_NAM ct st time y_sd_t
          stf MD ContractTermsPoly {contractType = PAM} = _STF_MD_PAM st time
          stf MD ContractTermsPoly {contractType = LAM} = _STF_MD_LAM st time
          stf MD ContractTermsPoly {contractType = NAM} = _STF_MD_LAM st time
          stf MD ContractTermsPoly {contractType = ANN} = _STF_MD_LAM st time
          stf PP ct@ContractTermsPoly {contractType = PAM} = _STF_PP_PAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PP ct@ContractTermsPoly {contractType = LAM} = _STF_PP_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PP ct@ContractTermsPoly {contractType = NAM} = _STF_PP_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PP ct@ContractTermsPoly {contractType = ANN} = _STF_PP_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PY ct@ContractTermsPoly {contractType = PAM} = _STF_PY_PAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PY ct@ContractTermsPoly {contractType = LAM} = _STF_PY_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PY ct@ContractTermsPoly {contractType = NAM} = _STF_PY_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PY ct@ContractTermsPoly {contractType = ANN} = _STF_PY_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf FP ContractTermsPoly {contractType = PAM} = _STF_FP_PAM st time y_sd_t
          stf FP ContractTermsPoly {contractType = LAM} = _STF_FP_LAM st time y_sd_t
          stf FP ContractTermsPoly {contractType = NAM} = _STF_FP_LAM st time y_sd_t
          stf FP ContractTermsPoly {contractType = ANN} = _STF_FP_LAM st time y_sd_t
          stf PRD ct@ContractTermsPoly {contractType = PAM} = _STF_PRD_PAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PRD ct@ContractTermsPoly {contractType = LAM} = _STF_PRD_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PRD ct@ContractTermsPoly {contractType = NAM} = _STF_PRD_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PRD ct@ContractTermsPoly {contractType = ANN} = _STF_PRD_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf TD _ = _STF_TD_PAM st time
          stf IP ct = _STF_IP_PAM ct st time y_sd_t
          stf IPCI ct@ContractTermsPoly {contractType = PAM} = _STF_IPCI_PAM ct st time y_sd_t
          stf IPCI ct@ContractTermsPoly {contractType = LAM} = _STF_IPCI_LAM ct st time y_sd_t
          stf IPCI ct@ContractTermsPoly {contractType = NAM} = _STF_IPCI_LAM ct st time y_sd_t
          stf IPCI ct@ContractTermsPoly {contractType = ANN} = _STF_IPCI_LAM ct st time y_sd_t
          stf IPCB ct@ContractTermsPoly {contractType = LAM} = _STF_IPCB_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf IPCB ct@ContractTermsPoly {contractType = NAM} = _STF_IPCB_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf IPCB ct@ContractTermsPoly {contractType = ANN} = _STF_IPCB_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RR ct@ContractTermsPoly {contractType = PAM} = _STF_RR_PAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RR ct@ContractTermsPoly {contractType = LAM} = _STF_RR_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RR ct@ContractTermsPoly {contractType = NAM} = _STF_RR_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RR ct@ContractTermsPoly {contractType = ANN} = _STF_RR_ANN ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus ti
          stf RRF ct@ContractTermsPoly {contractType = PAM} = _STF_RRF_PAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RRF ct@ContractTermsPoly {contractType = LAM} = _STF_RRF_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RRF ct@ContractTermsPoly {contractType = NAM} = _STF_RRF_LAM ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf RRF ct@ContractTermsPoly {contractType = ANN} = _STF_RRF_ANN ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus ti
          stf SC ct@ContractTermsPoly {contractType = PAM} = _STF_SC_PAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf PRF ct@ContractTermsPoly {contractType = ANN} = _STF_PRF_ANN ct st time y_sd_t y_tfpminus_t y_tfpminus_tfpplus y_t ti
          stf CE ContractTermsPoly {contractType = PAM} = _STF_CE_PAM st time y_sd_t
          stf SC ct@ContractTermsPoly {contractType = LAM} = _STF_SC_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf CE ContractTermsPoly {contractType = LAM} = _STF_CE_PAM st time y_sd_t
          stf SC ct@ContractTermsPoly {contractType = NAM} = _STF_SC_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf SC ct@ContractTermsPoly {contractType = ANN} = _STF_SC_LAM ct st rf time y_sd_t y_tfpminus_t y_tfpminus_tfpplus
          stf XD ct@ContractTermsPoly {contractType = OPTNS} = _STF_XD_OPTNS ct st rf time
          stf XD ct@ContractTermsPoly {contractType = FUTUR} = _STF_XD_FUTUR ct st rf time
          stf CE _ = _STF_AD_PAM st time y_sd_t
          stf _ _ = st

          time = marloweTime t

          tfp_minus = fromMaybe t (sup' fpSchedule t)
          tfp_plus = fromMaybe t (inf' fpSchedule t)
          tpr_plus = fromMaybe t (inf' prSchedule t)

          y_tfpminus_t = constnt $ _y dcc tfp_minus t md
          y_tfpminus_tfpplus = constnt $ _y dcc tfp_minus tfp_plus md
          y_ipanx_t = constnt $ _y dcc ipanx t md
          y_sd_t = constnt $ _y dcc prevDate t md
          y_t = constnt $ _y dcc t tpr_plus md

          prDates = prSchedule ++ maybeToList maturity
          prDatesAfterSd = filter (> t) prDates
          ti = zipWith (\tn tm -> constnt $ _y dcc tn tm md) prDatesAfterSd (tail prDatesAfterSd)
    stateTransitionFs' _ = st

toMarlowe :: ContractTerms -> ContractTermsMarlowe
toMarlowe ct =
  ContractTermsPoly
    { contractId = contractId ct,
      contractType = contractType ct,
      contractStructure = contractStructure ct,
      contractRole = contractRole ct,
      settlementCurrency = settlementCurrency ct,
      initialExchangeDate = marloweTime <$> initialExchangeDate ct,
      dayCountConvention = dayCountConvention ct,
      scheduleConfig = scheduleConfig ct,
      statusDate = marloweTime $ statusDate ct,
      contractPerformance = contractPerformance ct,
      cycleOfFee = cycleOfFee ct,
      cycleAnchorDateOfFee = marloweTime <$> cycleAnchorDateOfFee ct,
      feeAccrued = constnt <$> feeAccrued ct,
      feeBasis = feeBasis ct,
      feeRate = constnt <$> feeRate ct,
      cycleAnchorDateOfInterestPayment = marloweTime <$> cycleAnchorDateOfInterestPayment ct,
      cycleOfInterestPayment = cycleOfInterestPayment ct,
      accruedInterest = constnt <$> accruedInterest ct,
      capitalizationEndDate = marloweTime <$> capitalizationEndDate ct,
      cycleAnchorDateOfInterestCalculationBase = marloweTime <$> cycleAnchorDateOfInterestCalculationBase ct,
      cycleOfInterestCalculationBase = cycleOfInterestCalculationBase ct,
      interestCalculationBase = interestCalculationBase ct,
      interestCalculationBaseA = constnt <$> interestCalculationBaseA ct,
      nominalInterestRate = constnt <$> nominalInterestRate ct,
      interestScalingMultiplier = constnt <$> interestScalingMultiplier ct,
      notionalPrincipal = constnt <$> notionalPrincipal ct,
      premiumDiscountAtIED = constnt <$> premiumDiscountAtIED ct,
      maturityDate = marloweTime <$> maturityDate ct,
      amortizationDate = marloweTime <$> amortizationDate ct,
      exerciseDate = marloweTime <$> exerciseDate ct,
      cycleAnchorDateOfPrincipalRedemption = marloweTime <$> cycleAnchorDateOfPrincipalRedemption ct,
      cycleOfPrincipalRedemption = cycleOfPrincipalRedemption ct,
      nextPrincipalRedemptionPayment = constnt <$> nextPrincipalRedemptionPayment ct,
      purchaseDate = marloweTime <$> purchaseDate ct,
      priceAtPurchaseDate = constnt <$> priceAtPurchaseDate ct,
      terminationDate = marloweTime <$> terminationDate ct,
      priceAtTerminationDate = constnt <$> priceAtTerminationDate ct,
      scalingIndexAtStatusDate = constnt <$> scalingIndexAtStatusDate ct,
      cycleAnchorDateOfScalingIndex = marloweTime <$> cycleAnchorDateOfScalingIndex ct,
      cycleOfScalingIndex = cycleOfScalingIndex ct,
      scalingEffect = scalingEffect ct,
      scalingIndexAtContractDealDate = constnt <$> scalingIndexAtContractDealDate ct,
      marketObjectCodeOfScalingIndex = marketObjectCodeOfScalingIndex ct,
      notionalScalingMultiplier = constnt <$> notionalScalingMultiplier ct,
      cycleOfOptionality = cycleOfOptionality ct,
      cycleAnchorDateOfOptionality = marloweTime <$> cycleAnchorDateOfOptionality ct,
      optionType = optionType ct,
      optionStrike1 = constnt <$> optionStrike1 ct,
      optionExerciseType = optionExerciseType ct,
      settlementPeriod = settlementPeriod ct,
      deliverySettlement = deliverySettlement ct,
      exerciseAmount = constnt <$> exerciseAmount ct,
      futuresPrice = constnt <$> futuresPrice ct,
      penaltyRate = constnt <$> penaltyRate ct,
      penaltyType = penaltyType ct,
      prepaymentEffect = prepaymentEffect ct,
      cycleOfRateReset = cycleOfRateReset ct,
      cycleAnchorDateOfRateReset = marloweTime <$> cycleAnchorDateOfRateReset ct,
      nextResetRate = constnt <$> nextResetRate ct,
      rateSpread = constnt <$> rateSpread ct,
      rateMultiplier = constnt <$> rateMultiplier ct,
      periodFloor = constnt <$> periodFloor ct,
      periodCap = constnt <$> periodCap ct,
      lifeCap = constnt <$> lifeCap ct,
      lifeFloor = constnt <$> lifeFloor ct,
      marketObjectCodeOfRateReset = marketObjectCodeOfRateReset ct,
      cycleOfDividend = cycleOfDividend ct,
      cycleAnchorDateOfDividend = marloweTime <$> cycleAnchorDateOfDividend ct,
      nextDividendPaymentAmount = constnt <$> nextDividendPaymentAmount ct,
      enableSettlement = enableSettlement ct,
      constraints = constraints ct,
      collateralAmount = collateralAmount ct
    }
