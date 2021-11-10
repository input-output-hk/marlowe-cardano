{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel where

import Control.Applicative (liftA2)
import Control.Monad (liftM4)
import Data.Functor ((<&>))
import Data.List as L (find, nub)
import Data.Maybe (fromMaybe, isJust, isNothing, maybeToList)
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime (..), addLocalTime)
import Language.Marlowe.ACTUS.Domain.ContractTerms (ContractTerms, ContractTermsPoly (..), Cycle (..), IPCB (IPCB_NTL),
                                                    PPEF (..), PYTP (..), SCEF (..))
import Language.Marlowe.ACTUS.Domain.Schedule (ShiftedDay (..))
import Language.Marlowe.ACTUS.Utility.DateShift (applyBDCWithCfg)
import Language.Marlowe.ACTUS.Utility.ScheduleGenerator (generateRecurrentScheduleWithCorrections, inf, remove, (<+>),
                                                         (<->))

-- Principal at Maturity (PAM)

_SCHED_IED_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_IED_PAM
  ContractTermsPoly
    { scheduleConfig,
      initialExchangeDate = Just ied
    } = [applyBDCWithCfg scheduleConfig ied]
_SCHED_IED_PAM _ = []

_SCHED_MD_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_MD_PAM
  ContractTermsPoly
    { scheduleConfig,
      maturityDate = Just md
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_MD_PAM _ = []

_SCHED_PP_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_PP_PAM ContractTermsPoly {prepaymentEffect = Just PPEF_N} = []
_SCHED_PP_PAM
  ContractTermsPoly
    { cycleAnchorDateOfOptionality = Just opanx,
      cycleOfOptionality = Just opcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections opanx opcl md scheduleConfig
_SCHED_PP_PAM
  ContractTermsPoly
    { cycleAnchorDateOfOptionality = Nothing,
      cycleOfOptionality = Just opcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> opcl) opcl md scheduleConfig
_SCHED_PP_PAM _ = []

_SCHED_PY_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_PY_PAM ContractTermsPoly {penaltyType = Just PYTP_O} = []
_SCHED_PY_PAM ct                                            = _SCHED_PP_PAM ct

_SCHED_FP_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_FP_PAM ContractTermsPoly {feeRate = Nothing} = []
_SCHED_FP_PAM ContractTermsPoly {feeRate = Just 0.0} = []
_SCHED_FP_PAM
  ContractTermsPoly
    { cycleAnchorDateOfFee = Just feanx,
      cycleOfFee = Just fecl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections feanx fecl {includeEndDay = True} md scheduleConfig
_SCHED_FP_PAM
  ContractTermsPoly
    { cycleAnchorDateOfFee = Nothing,
      cycleOfFee = Just fecl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> fecl) fecl {includeEndDay = True} md scheduleConfig
_SCHED_FP_PAM _ = []

_SCHED_PRD_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_PRD_PAM
  ContractTermsPoly
    { scheduleConfig,
      purchaseDate = Just prd
    } = [applyBDCWithCfg scheduleConfig prd]
_SCHED_PRD_PAM _ = []

_SCHED_TD_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_TD_PAM
  ContractTermsPoly
    { scheduleConfig,
      terminationDate = Just td
    } = [applyBDCWithCfg scheduleConfig td]
_SCHED_TD_PAM _ = []

_SCHED_IP_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_IP_PAM
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      capitalizationEndDate = ipced,
      scheduleConfig
    } =
    let s = generateRecurrentScheduleWithCorrections ipanx ipcl {includeEndDay = True} md scheduleConfig
     in filter (\d -> Just (calculationDay d) > ipced) s
_SCHED_IP_PAM
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      capitalizationEndDate = ipced,
      scheduleConfig
    } =
    let s = generateRecurrentScheduleWithCorrections (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
     in filter (\d -> Just (calculationDay d) > ipced) s
_SCHED_IP_PAM _ = []

_SCHED_IPCI_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_IPCI_PAM
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      capitalizationEndDate = Just ipced,
      scheduleConfig
    } =
    let s = generateRecurrentScheduleWithCorrections ipanx ipcl {includeEndDay = True} md scheduleConfig
     in filter (\d -> calculationDay d < ipced) s ++ [applyBDCWithCfg scheduleConfig ipced]
_SCHED_IPCI_PAM
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      capitalizationEndDate = Just ipced,
      scheduleConfig
    } =
    let s = generateRecurrentScheduleWithCorrections (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
     in filter (\d -> calculationDay d < ipced) s ++ [applyBDCWithCfg scheduleConfig ipced]
_SCHED_IPCI_PAM _ = []

_SCHED_RR_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_RR_PAM
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      maturityDate = Just md,
      statusDate,
      scheduleConfig
    } =
    let tt = generateRecurrentScheduleWithCorrections rranx rrcl {includeEndDay = False} md scheduleConfig
     in fromMaybe [] (inf tt statusDate <&> flip remove tt)
_SCHED_RR_PAM
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections rranx rrcl {includeEndDay = False} md scheduleConfig
_SCHED_RR_PAM
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Nothing,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      statusDate,
      scheduleConfig
    } =
    let tt = generateRecurrentScheduleWithCorrections (ied <+> rrcl) rrcl {includeEndDay = False} md scheduleConfig
     in fromMaybe [] (inf tt statusDate <&> flip remove tt)
_SCHED_RR_PAM
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Nothing,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Nothing,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> rrcl) rrcl {includeEndDay = False} md scheduleConfig
_SCHED_RR_PAM _ = []

_SCHED_RRF_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_RRF_PAM
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      maturityDate = Just md,
      statusDate,
      scheduleConfig
    } =
    let tt = generateRecurrentScheduleWithCorrections rranx rrcl {includeEndDay = False} md scheduleConfig
     in maybeToList (L.find (\ShiftedDay{..} -> calculationDay > statusDate) tt)
_SCHED_RRF_PAM
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Nothing,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      statusDate,
      scheduleConfig
    } =
    let tt = generateRecurrentScheduleWithCorrections (ied <+> rrcl) rrcl md scheduleConfig
     in maybeToList (L.find (\ShiftedDay{..} -> calculationDay > statusDate) tt)
_SCHED_RRF_PAM _ = []

_SCHED_SC_PAM :: ContractTerms -> [ShiftedDay]
_SCHED_SC_PAM ContractTermsPoly {scalingEffect = Just SE_OOO} = []
_SCHED_SC_PAM
  ContractTermsPoly
    { cycleAnchorDateOfScalingIndex = Just scanx,
      cycleOfScalingIndex = Just sccl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections scanx sccl {includeEndDay = False} md scheduleConfig
_SCHED_SC_PAM
  ContractTermsPoly
    { cycleAnchorDateOfScalingIndex = Nothing,
      cycleOfScalingIndex = Just sccl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> sccl) sccl {includeEndDay = False} md scheduleConfig
_SCHED_SC_PAM _ = []

-- Linear Amortizer (LAM)

_SCHED_PR_LAM :: ContractTerms -> [ShiftedDay]
_SCHED_PR_LAM
  ContractTermsPoly
    { cycleAnchorDateOfPrincipalRedemption = Just pranx,
      cycleOfPrincipalRedemption = Just prcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections pranx prcl {includeEndDay = False} md scheduleConfig
_SCHED_PR_LAM
  ContractTermsPoly
    { cycleAnchorDateOfPrincipalRedemption = Nothing,
      cycleOfPrincipalRedemption = Just prcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> prcl) prcl {includeEndDay = False} md scheduleConfig
_SCHED_PR_LAM _ = []

_SCHED_MD_LAM :: ContractTerms -> [ShiftedDay]
_SCHED_MD_LAM
  ContractTermsPoly
    { maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_MD_LAM _ = []

_SCHED_IPCB_LAM :: ContractTerms -> [ShiftedDay]
_SCHED_IPCB_LAM ContractTermsPoly {..} | interestCalculationBase /= Just IPCB_NTL = []
_SCHED_IPCB_LAM
  ContractTermsPoly
    { cycleAnchorDateOfInterestCalculationBase = Just ipcbanx,
      cycleOfInterestCalculationBase = Just ipcbcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections ipcbanx ipcbcl {includeEndDay = False} md scheduleConfig
_SCHED_IPCB_LAM
  ContractTermsPoly
    { cycleAnchorDateOfInterestCalculationBase = Nothing,
      cycleOfInterestCalculationBase = Just ipcbcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> ipcbcl) ipcbcl {includeEndDay = False} md scheduleConfig
_SCHED_IPCB_LAM _ = []

-- Negative Amortizer (NAM)

_SCHED_IP_NAM :: ContractTerms -> [ShiftedDay]
_SCHED_IP_NAM ContractTermsPoly {..} =
  let s
        | isNothing cycleAnchorDateOfPrincipalRedemption = liftA2 (<+>) initialExchangeDate cycleOfPrincipalRedemption
        | otherwise = cycleAnchorDateOfPrincipalRedemption

      v = liftM4 generateRecurrentScheduleWithCorrections s cycleOfPrincipalRedemption maturityDate (Just scheduleConfig)

      r
        | isJust cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment
        | isJust cycleOfInterestPayment = liftA2 (<+>) initialExchangeDate cycleOfInterestPayment
        | otherwise = Nothing

      _T = liftA2 (<->) s cycleOfPrincipalRedemption

      u
        | isNothing cycleAnchorDateOfInterestPayment && isNothing cycleOfInterestPayment = Nothing
        | isJust capitalizationEndDate && Just True == liftA2 (>) capitalizationEndDate _T = Nothing
        | otherwise = liftM4 generateRecurrentScheduleWithCorrections r ((\c -> c {includeEndDay = True}) <$> cycleOfInterestPayment) maturityDate (Just scheduleConfig)

      result = nub <$> liftA2 (++) u v

      result'
        | isJust result && isJust capitalizationEndDate = filter (\ShiftedDay {..} -> Just calculationDay > capitalizationEndDate) <$> result
        | otherwise = result
   in fromMaybe [] result'

_SCHED_IPCI_NAM :: ContractTerms -> [ShiftedDay]
_SCHED_IPCI_NAM ContractTermsPoly {..} =
  let s
        | isNothing cycleAnchorDateOfPrincipalRedemption = liftA2 (<+>) initialExchangeDate cycleOfPrincipalRedemption
        | otherwise = cycleAnchorDateOfPrincipalRedemption

      v = liftM4 generateRecurrentScheduleWithCorrections s cycleOfPrincipalRedemption maturityDate (Just scheduleConfig)

      r
        | isJust capitalizationEndDate = capitalizationEndDate
        | isJust cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment
        | isJust cycleOfInterestPayment = liftA2 (<+>) initialExchangeDate cycleOfInterestPayment
        | otherwise = Nothing

      _T = liftA2 (<->) s cycleOfPrincipalRedemption

      u
        | isNothing cycleAnchorDateOfInterestPayment && isNothing cycleOfInterestPayment = Nothing
        | isJust capitalizationEndDate && Just True == liftA2 (>) capitalizationEndDate _T = Nothing
        | otherwise = liftM4 generateRecurrentScheduleWithCorrections r ((\c -> c {includeEndDay = True}) <$> cycleOfInterestPayment) maturityDate (Just scheduleConfig)

      result = Just $ nub (fromMaybe [] u ++ fromMaybe [] v)

      result'
        | isJust result && isJust capitalizationEndDate = filter (\ShiftedDay {..} -> Just calculationDay <= capitalizationEndDate) <$> result
        | otherwise = Nothing
   in fromMaybe [] result'

-- Annuity (ANN)

_SCHED_PRF_ANN :: ContractTerms -> [ShiftedDay]
_SCHED_PRF_ANN
  ct@ContractTermsPoly
    { cycleAnchorDateOfPrincipalRedemption = Just pranx,
      nextPrincipalRedemptionPayment = Nothing,
      initialExchangeDate = Just ied
    } =
    let prf
          | pranx > ied = let p = addLocalTime (-86400) pranx in [ShiftedDay p p]
          | otherwise = []
        rr = _SCHED_RR_PAM ct
        rrf = _SCHED_RRF_PAM ct
     in prf ++ rr ++ rrf
_SCHED_PRF_ANN _ = []

-- Stock (STK)

_SCHED_DV_STK :: ContractTerms -> [ShiftedDay]
_SCHED_DV_STK
  ContractTermsPoly
    { cycleAnchorDateOfDividend = Just dvanx,
      cycleOfDividend = Just dvcl,
      nextDividendPaymentAmount = Nothing,
      scheduleConfig = scheduleConfig
    } = let tMax = LocalTime (addDays (10*365) $ localDay dvanx) (localTimeOfDay dvanx)
         in generateRecurrentScheduleWithCorrections dvanx dvcl tMax scheduleConfig
_SCHED_DV_STK
  ContractTermsPoly
    { cycleAnchorDateOfDividend = Just dvanx,
      cycleOfDividend = Just dvcl,
      scheduleConfig = scheduleConfig
    } = let tMax = LocalTime (addDays (10*365) $ localDay dvanx) (localTimeOfDay dvanx)
          in generateRecurrentScheduleWithCorrections (dvanx <+> dvcl) dvcl tMax scheduleConfig
_SCHED_DV_STK _ = []

-- Options (OPTNS)

_SCHED_XD_OPTNS :: ContractTerms -> [ShiftedDay]
_SCHED_XD_OPTNS
  ContractTermsPoly
    { exerciseDate = Just xd,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig xd]
_SCHED_XD_OPTNS
  ContractTermsPoly
    { maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_XD_OPTNS _ = []

_SCHED_STD_OPTNS :: ContractTerms -> [ShiftedDay]
_SCHED_STD_OPTNS
  ContractTermsPoly
    { scheduleConfig,
      maturityDate = Just md,
      settlementPeriod = Just stp
    } = [applyBDCWithCfg scheduleConfig (md <+> stp)]
_SCHED_STD_OPTNS
  ContractTermsPoly
    { scheduleConfig
    , maturityDate = Just md
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_STD_OPTNS
  ContractTermsPoly
    { scheduleConfig,
      exerciseDate = Just xd,
      settlementPeriod = Just stp
    } = [applyBDCWithCfg scheduleConfig (xd <+> stp)]
_SCHED_STD_OPTNS
  ContractTermsPoly
    { scheduleConfig,
      exerciseDate = Just xd
    } = [applyBDCWithCfg scheduleConfig xd]
_SCHED_STD_OPTNS _ = []

