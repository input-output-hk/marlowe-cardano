{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS contract schedules

The implementation is a transliteration of the ACTUS specification v1.1

-}

module Language.Marlowe.ACTUS.Model.ContractSchedule
  ( schedule
  , maturity
  )
where

import           Control.Applicative                              (Alternative ((<|>)), liftA2)
import           Control.Monad                                    (liftM4)
import           Data.Functor                                     ((<&>))
import           Data.List                                        as L (find, nub)
import           Data.Maybe                                       (fromMaybe, isJust, isNothing, maybeToList)
import           Data.Ord                                         (Down (..))
import           Data.Sort                                        (sortOn)
import           Data.Time                                        (LocalTime)
import           Data.Time.Calendar                               (addDays)
import           Data.Time.LocalTime                              (LocalTime (..), addLocalTime)
import           Language.Marlowe.ACTUS.Domain.BusinessEvents     (EventType (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms      (CT (..), ContractTermsPoly (..), Cycle (..), DS (..),
                                                                   IPCB (..), PPEF (..), PYTP (..), SCEF (..),
                                                                   ScheduleConfig (..))
import           Language.Marlowe.ACTUS.Domain.Ops                as O (ActusNum (..), ActusOps (..),
                                                                        YearFractionOps (_y))
import           Language.Marlowe.ACTUS.Domain.Schedule           (ShiftedDay (..))
import           Language.Marlowe.ACTUS.Utility.DateShift         (applyBDCWithCfg)
import           Language.Marlowe.ACTUS.Utility.ScheduleGenerator (applyEOMC, generateRecurrentScheduleWithCorrections,
                                                                   inf, remove, (<+>), (<->))

schedule :: (ActusNum a, ActusOps a, YearFractionOps a) => EventType -> ContractTermsPoly a -> [ShiftedDay]
schedule ev c = schedule' ev c { maturityDate = maturity c }
  where

    schedule' :: (ActusNum a, ActusOps a) => EventType -> ContractTermsPoly a -> [ShiftedDay]
    schedule' IED  ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_IED_PAM ct
    schedule' MD   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_MD_PAM ct
    schedule' PP   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_IP_PAM ct
    schedule' IPCI ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_IPCI_PAM ct
    schedule' RR   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTermsPoly{ contractType = PAM }   = _SCHED_SC_PAM ct

    schedule' IED  ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_IED_PAM ct
    schedule' PR   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_PR_LAM ct
    schedule' MD   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_MD_LAM ct
    schedule' PP   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_IP_PAM ct
    schedule' IPCI ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_IPCI_PAM ct
    schedule' IPCB ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTermsPoly{ contractType = LAM }   = _SCHED_SC_PAM ct

    schedule' IED  ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_IED_PAM ct
    schedule' PR   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_PR_LAM ct
    schedule' MD   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_MD_PAM ct
    schedule' PP   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_IP_NAM ct
    schedule' IPCI ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_IPCI_NAM ct
    schedule' IPCB ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTermsPoly{ contractType = NAM }   = _SCHED_SC_PAM ct

    schedule' IED  ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_IED_PAM ct
    schedule' PR   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_PR_LAM ct
    schedule' MD   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_MD_PAM c { maturityDate = maturityDate c <|> maturityDate ct }
    schedule' PP   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_PP_PAM ct
    schedule' PY   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_PY_PAM ct
    schedule' FP   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_FP_PAM ct
    schedule' PRD  ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_TD_PAM ct
    schedule' IP   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_IP_NAM c { maturityDate = maturityDate c <|> maturityDate ct }
    schedule' IPCI ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_IPCI_PAM ct
    schedule' IPCB ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_IPCB_LAM ct
    schedule' RR   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_RR_PAM ct
    schedule' RRF  ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_RRF_PAM ct
    schedule' SC   ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_SC_PAM ct
    schedule' PRF  ct@ContractTermsPoly{ contractType = ANN }   = _SCHED_PRF_ANN ct

    schedule' PRD  ct@ContractTermsPoly{ contractType = STK }   = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = STK }   = _SCHED_TD_PAM ct
    schedule' DV   ct@ContractTermsPoly{ contractType = STK }   = _SCHED_DV_STK ct

    schedule' PRD  ct@ContractTermsPoly{ contractType = OPTNS } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = OPTNS } = _SCHED_TD_PAM ct
    schedule' MD   ct@ContractTermsPoly{ contractType = OPTNS } = _SCHED_MD_PAM c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO
    schedule' XD   ct@ContractTermsPoly{ contractType = OPTNS } = _SCHED_XD_OPTNS c { maturityDate = maturityDate c <|> maturityDate ct }
    schedule' STD  ct@ContractTermsPoly{ contractType = OPTNS } = _SCHED_STD_OPTNS c { maturityDate = maturityDate c <|> maturityDate ct }

    schedule' PRD  ct@ContractTermsPoly{ contractType = FUTUR } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = FUTUR } = _SCHED_TD_PAM ct
    schedule' MD   ct@ContractTermsPoly{ contractType = FUTUR } = _SCHED_MD_PAM c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO
    schedule' XD   ct@ContractTermsPoly{ contractType = FUTUR } = _SCHED_XD_OPTNS ct { maturityDate = maturityDate c <|> maturityDate ct }
    schedule' STD  ct@ContractTermsPoly{ contractType = FUTUR } = _SCHED_STD_OPTNS c { maturityDate = maturityDate c <|> maturityDate ct }

    schedule' PRD  ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_PRD_PAM ct
    schedule' TD   ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_TD_PAM ct
    schedule' IED  ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_IED_PAM ct
    schedule' RR   ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_RR_SWPPV c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO
    schedule' IP   ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_IP_SWPPV c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO
    schedule' IPFX ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_IPFX_SWPPV c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO
    schedule' IPFL ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_IPFL_SWPPV c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO
    schedule' MD   ct@ContractTermsPoly{ contractType = SWPPV } = _SCHED_MD_PAM c { maturityDate = maturityDate c <|> maturityDate ct } -- TODO

    schedule' _ _                                               = []

maturity :: (ActusNum a, ActusOps a, YearFractionOps a) => ContractTermsPoly a -> Maybe LocalTime
maturity ContractTermsPoly {contractType = PAM, ..} = maturityDate
maturity ContractTermsPoly {contractType = LAM, maturityDate = md@(Just _)} = md
maturity
  ContractTermsPoly
    { contractType = LAM,
      maturityDate = Nothing,
      cycleAnchorDateOfPrincipalRedemption = Just pranx,
      cycleOfInterestPayment = Just ipcl,
      cycleOfPrincipalRedemption = Just prcl,
      nextPrincipalRedemptionPayment = Just prnxt,
      notionalPrincipal = Just nt,
      statusDate,
      scheduleConfig
    } =
    let (lastEvent, remainingPeriods)
          | pranx < statusDate =
            let previousEvents = generateRecurrentScheduleWithCorrections pranx prcl statusDate scheduleConfig
                f1 = (\ShiftedDay {..} -> calculationDay > statusDate <-> ipcl)
                f2 = (\ShiftedDay {..} -> calculationDay == statusDate)
                ShiftedDay {calculationDay = lastEventCalcDay} = head . filter f2 . filter f1 $ previousEvents
             in (lastEventCalcDay, nt O./ prnxt)
          | otherwise = (pranx, nt O./ prnxt O.- _one)
        m = lastEvent <+> (prcl {n = n prcl Prelude.* _toInteger remainingPeriods})
     in endOfMonthConvention scheduleConfig >>= \d -> return $ applyEOMC lastEvent prcl d m
maturity ContractTermsPoly {contractType = NAM, maturityDate = md@(Just _)} = md
maturity
  ContractTermsPoly
    { contractType = NAM,
      maturityDate = Nothing,
      cycleAnchorDateOfPrincipalRedemption = Just pranx,
      nextPrincipalRedemptionPayment = Just prnxt,
      initialExchangeDate = Just ied,
      cycleOfPrincipalRedemption = Just prcl,
      notionalPrincipal = Just nt,
      nominalInterestRate = Just ipnr,
      dayCountConvention = Just dcc,
      statusDate,
      scheduleConfig
    } =
    let lastEvent
          | pranx >= statusDate = pranx
          | ied <+> prcl >= statusDate = ied <+> prcl
          | otherwise =
            let previousEvents = generateRecurrentScheduleWithCorrections pranx prcl statusDate scheduleConfig
                f = (\ShiftedDay {..} -> calculationDay == statusDate)
                ShiftedDay {calculationDay = lastEventCalcDay} = head . filter f $ previousEvents
             in lastEventCalcDay

        yLastEventPlusPRCL = _y dcc lastEvent (lastEvent <+> prcl) Nothing
        redemptionPerCycle = prnxt O.- (yLastEventPlusPRCL O.* ipnr O.* nt)
        remainingPeriods = _toInteger $ (nt O./ redemptionPerCycle) O.- _one
        m = lastEvent <+> prcl {n = n prcl Prelude.* remainingPeriods}
     in endOfMonthConvention scheduleConfig >>= \d -> return $ applyEOMC lastEvent prcl d m
maturity
  ContractTermsPoly
    { contractType = ANN,
      amortizationDate = Nothing,
      maturityDate = Nothing,
      cycleAnchorDateOfPrincipalRedemption = Just pranx,
      nextPrincipalRedemptionPayment = Just prnxt,
      initialExchangeDate = Just ied,
      cycleOfPrincipalRedemption = Just prcl,
      notionalPrincipal = Just nt,
      nominalInterestRate = Just ipnr,
      dayCountConvention = Just dcc,
      statusDate,
      scheduleConfig
    } =
    let tplus = ied <+> prcl
        lastEvent
          | pranx >= statusDate = pranx
          | tplus >= statusDate = tplus
          | otherwise =
            let previousEvents = generateRecurrentScheduleWithCorrections statusDate prcl pranx scheduleConfig
             in calculationDay . head . sortOn (Down . calculationDay) . filter (\ShiftedDay {..} -> calculationDay > statusDate) $ previousEvents
        timeFromLastEventPlusOneCycle = _y dcc lastEvent (lastEvent <+> prcl) Nothing
        redemptionPerCycle = prnxt O.- timeFromLastEventPlusOneCycle O.* ipnr O.* nt
        remainingPeriods = _toInteger $ (nt O./ redemptionPerCycle) O.- _one
    in Just . calculationDay . applyBDCWithCfg scheduleConfig $ lastEvent <+> prcl { n = remainingPeriods }
maturity
  ContractTermsPoly
    { contractType = ANN,
      amortizationDate = ad@(Just _)
    } = ad
maturity
  ContractTermsPoly
    { contractType = ANN,
      amortizationDate = Nothing,
      maturityDate = md@(Just _)
    } = md
maturity _ = Nothing

-- Principal at Maturity (PAM)

_SCHED_IED_PAM :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_IED_PAM
  ContractTermsPoly
    { scheduleConfig,
      initialExchangeDate = Just ied
    } = [applyBDCWithCfg scheduleConfig ied]
_SCHED_IED_PAM _ = []

_SCHED_MD_PAM :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_MD_PAM
  ContractTermsPoly
    { scheduleConfig,
      maturityDate = Just md
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_MD_PAM _ = []

_SCHED_PP_PAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_PY_PAM :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_PY_PAM ContractTermsPoly {penaltyType = Just PYTP_O} = []
_SCHED_PY_PAM ct                                            = _SCHED_PP_PAM ct

_SCHED_FP_PAM :: (ActusNum a, ActusOps a) => ContractTermsPoly a -> [ShiftedDay]
_SCHED_FP_PAM ContractTermsPoly {feeRate = Nothing} = []
_SCHED_FP_PAM ContractTermsPoly {feeRate = Just x} | x == _zero = []
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

_SCHED_PRD_PAM :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_PRD_PAM
  ContractTermsPoly
    { scheduleConfig,
      purchaseDate = Just prd
    } = [applyBDCWithCfg scheduleConfig prd]
_SCHED_PRD_PAM _ = []

_SCHED_TD_PAM :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_TD_PAM
  ContractTermsPoly
    { scheduleConfig,
      terminationDate = Just td
    } = [applyBDCWithCfg scheduleConfig td]
_SCHED_TD_PAM _ = []

_SCHED_IP_PAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_IPCI_PAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_RR_PAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_RRF_PAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_SC_PAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_PR_LAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_MD_LAM :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_MD_LAM
  ContractTermsPoly
    { maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_MD_LAM _ = []

_SCHED_IPCB_LAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_IP_NAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_IPCI_NAM :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_PRF_ANN :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_DV_STK :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_DV_STK
  ContractTermsPoly
    { cycleAnchorDateOfDividend = Just dvanx,
      cycleOfDividend = Just dvcl,
      nextDividendPaymentAmount = Nothing,
      scheduleConfig = scheduleConfig
    } = let tMax = LocalTime (addDays (10 Prelude.* 365) $ localDay dvanx) (localTimeOfDay dvanx)
         in generateRecurrentScheduleWithCorrections dvanx dvcl tMax scheduleConfig
_SCHED_DV_STK
  ContractTermsPoly
    { cycleAnchorDateOfDividend = Just dvanx,
      cycleOfDividend = Just dvcl,
      scheduleConfig = scheduleConfig
    } = let tMax = LocalTime (addDays (10 Prelude.* 365) $ localDay dvanx) (localTimeOfDay dvanx)
          in generateRecurrentScheduleWithCorrections (dvanx <+> dvcl) dvcl tMax scheduleConfig
_SCHED_DV_STK _ = []

-- Options (OPTNS)

_SCHED_XD_OPTNS :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_STD_OPTNS :: ContractTermsPoly a -> [ShiftedDay]
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

_SCHED_IP_SWPPV :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_IP_SWPPV
  ContractTermsPoly
    { deliverySettlement = Just DS_D
    } = []
_SCHED_IP_SWPPV
  ContractTermsPoly
    { cycleOfInterestPayment = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_IP_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections ipanx ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IP_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IP_SWPPV _ = []

_SCHED_IPFX_SWPPV :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_IPFX_SWPPV
  ContractTermsPoly
    { deliverySettlement = Just DS_S
    } = []
_SCHED_IPFX_SWPPV
  ContractTermsPoly
    { cycleOfInterestPayment = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_IPFX_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections ipanx ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFX_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFX_SWPPV _ = []

_SCHED_IPFL_SWPPV :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_IPFL_SWPPV
  ContractTermsPoly
    { deliverySettlement = Just DS_S
    } = []
_SCHED_IPFL_SWPPV
  ContractTermsPoly
    { cycleOfInterestPayment = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_IPFL_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections ipanx ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFL_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFL_SWPPV _ = []

_SCHED_RR_SWPPV :: ContractTermsPoly a -> [ShiftedDay]
_SCHED_RR_SWPPV
  ContractTermsPoly
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections rranx rrcl {includeEndDay = False} md scheduleConfig
_SCHED_RR_SWPPV
  ContractTermsPoly
    { cycleOfRateReset = Just rrcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentScheduleWithCorrections (ied <+> rrcl) rrcl {includeEndDay = False} md scheduleConfig
_SCHED_RR_SWPPV
  ContractTermsPoly
    { cycleOfRateReset = Nothing,
      cycleAnchorDateOfRateReset = Just rranx,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig rranx]
_SCHED_RR_SWPPV _ = []
