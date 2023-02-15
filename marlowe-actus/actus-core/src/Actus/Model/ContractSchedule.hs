{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS contract schedules -}
module Actus.Model.ContractSchedule
  ( maturity
  , schedule
  ) where

import Actus.Domain
  ( ActusFrac(..)
  , CR(..)
  , CT(..)
  , ContractStructure(..)
  , ContractTerms(..)
  , Cycle(..)
  , DS(..)
  , EventType(..)
  , IPCB(..)
  , PPEF(..)
  , PYTP(..)
  , Reference(..)
  , ReferenceRole(..)
  , SCEF(..)
  , ScheduleConfig(..)
  , ShiftedDay(..)
  , mkShiftedDay
  )
import Actus.Utility (applyBDCWithCfg, applyEOMC, generateRecurrentSchedule, inf, yearFraction, (<+>), (<->))
import Control.Applicative ((<|>))
import Control.Monad (liftM2, liftM4)
import Data.Functor ((<&>))
import Data.List as L (delete, find, nub)
import Data.Maybe (fromMaybe, isJust, isNothing, mapMaybe, maybeToList)
import Data.Ord (Down(..))
import Data.Sort (sortOn)
import Data.Time (LocalTime)
import Data.Time.Calendar (addDays)
import Data.Time.LocalTime (LocalTime(..), addLocalTime)

-- |Generate the schedule for a given event type
schedule ::
  (ActusFrac a) =>
  -- | Event type
  EventType ->
  -- | Contract terms
  ContractTerms a ->
  -- | Schedule
  [(String, ShiftedDay)]
schedule IED ct@ContractTerms {contractId, contractType = PAM} = zip (repeat contractId) $ _SCHED_IED_PAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule PP   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_PP_PAM ct
schedule PY   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_PY_PAM ct
schedule FP   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_FP_PAM ct
schedule PRD  ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule IP   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_IP_PAM ct
schedule IPCI ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_IPCI_PAM ct
schedule RR   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_RR_PAM ct
schedule RRF  ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_RRF_PAM ct
schedule SC   ct@ContractTerms{ contractId, contractType = PAM }   = zip (repeat contractId) $ _SCHED_SC_PAM ct
schedule IED  ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_IED_PAM ct
schedule PR   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_PR_LAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_MD_LAM ct
schedule PP   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_PP_PAM ct
schedule PY   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_PY_PAM ct
schedule FP   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_FP_PAM ct
schedule PRD  ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule IP   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_IP_PAM ct
schedule IPCI ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_IPCI_PAM ct
schedule IPCB ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_IPCB_LAM ct
schedule RR   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_RR_PAM ct
schedule RRF  ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_RRF_PAM ct
schedule SC   ct@ContractTerms{ contractId, contractType = LAM }   = zip (repeat contractId) $ _SCHED_SC_PAM ct
schedule IED  ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_IED_PAM ct
schedule PR   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_PR_LAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule PP   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_PP_PAM ct
schedule PY   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_PY_PAM ct
schedule FP   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_FP_PAM ct
schedule PRD  ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule IP   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_IP_NAM ct
schedule IPCI ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_IPCI_NAM ct
schedule IPCB ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_IPCB_LAM ct
schedule RR   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_RR_PAM ct
schedule RRF  ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_RRF_PAM ct
schedule SC   ct@ContractTerms{ contractId, contractType = NAM }   = zip (repeat contractId) $ _SCHED_SC_PAM ct
schedule IED  ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_IED_PAM ct
schedule PR   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_PR_LAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule PP   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_PP_PAM ct
schedule PY   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_PY_PAM ct
schedule FP   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_FP_PAM ct
schedule PRD  ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule IP   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_IP_NAM ct
schedule IPCI ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_IPCI_PAM ct
schedule IPCB ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_IPCB_LAM ct
schedule RR   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_RR_PAM ct
schedule RRF  ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_RRF_PAM ct
schedule SC   ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_SC_PAM ct
schedule PRF  ct@ContractTerms{ contractId, contractType = ANN }   = zip (repeat contractId) $ _SCHED_PRF_ANN ct
schedule PRD  ct@ContractTerms{ contractId, contractType = STK }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = STK }   = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule DV   ct@ContractTerms{ contractId, contractType = STK }   = zip (repeat contractId) $ _SCHED_DV_STK ct
schedule PRD  ct@ContractTerms{ contractId, contractType = OPTNS } = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = OPTNS } = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = OPTNS } = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule XD   ct@ContractTerms{ contractId, contractType = OPTNS } = zip (repeat contractId) $ _SCHED_XD_OPTNS ct
schedule STD  ct@ContractTerms{ contractId, contractType = OPTNS } = zip (repeat contractId) $ _SCHED_STD_OPTNS ct
schedule PRD  ct@ContractTerms{ contractId, contractType = FUTUR } = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = FUTUR } = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = FUTUR } = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule XD   ct@ContractTerms{ contractId, contractType = FUTUR } = zip (repeat contractId) $ _SCHED_XD_OPTNS ct
schedule STD  ct@ContractTerms{ contractId, contractType = FUTUR } = zip (repeat contractId) $ _SCHED_STD_OPTNS ct
schedule PRD  ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule IED  ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_IED_PAM ct
schedule RR   ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_RR_SWPPV ct
schedule IP   ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_IP_SWPPV ct
schedule IPFX ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_IPFX_SWPPV ct
schedule IPFL ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_IPFL_SWPPV ct
schedule MD   ct@ContractTerms{ contractId, contractType = SWPPV } = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule PRD  ct@ContractTerms{ contractId, contractType = CEG }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = CEG }   = zip (repeat contractId) $ _SCHED_MD_CEG ct
schedule XD   ct@ContractTerms{ contractId, contractType = CEG }   = zip (repeat contractId) $ _SCHED_XD_CEG ct -- added as unscheduled events
schedule FP   ct@ContractTerms{ contractId, contractType = CEG }   = zip (repeat contractId) $ _SCHED_FP_CEG ct
schedule PRD  ct@ContractTerms{ contractId, contractType = CEC }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = CEC }   = zip (repeat contractId) $ _SCHED_MD_CEC ct
schedule XD   ct@ContractTerms{ contractId, contractType = CEC }   = zip (repeat contractId) $ _SCHED_XD_CEG ct -- added as unscheduled events
schedule PRD  ct@ContractTerms{ contractId, contractType = COM }   = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = COM }   = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule IED  ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_IED_PAM ct
schedule MD   ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_MD_PAM ct
schedule FP   ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_FP_PAM ct
schedule PR   ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_PR_LAM ct
schedule IP   ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_IP_CLM ct
schedule IPCI ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_IPCI_CLM ct
schedule RR   ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_RR_PAM ct
schedule RRF  ct@ContractTerms{ contractId, contractType = CLM }   = zip (repeat contractId) $ _SCHED_RRF_PAM ct
schedule PRD  ct@ContractTerms{ contractId, contractType = SWAPS}  = zip (repeat contractId) $ _SCHED_PRD_PAM ct
schedule TD   ct@ContractTerms{ contractId, contractType = SWAPS}  = zip (repeat contractId) $ _SCHED_TD_PAM ct
schedule ev   ct@ContractTerms{ contractRole, contractType = SWAPS}  =
  let fil = head $ filter (\cs -> referenceRole cs == FIL) $ contractStructure ct
      sel = head $ filter (\cs -> referenceRole cs == SEL) $ contractStructure ct
      s = maybe [] (schedule ev) (setRoleFil contractRole <$> termsFromStructure fil)
        ++ maybe [] (schedule ev) (setRoleSel contractRole <$> termsFromStructure sel)
   in s
schedule _ _                                           = []

setRoleFil :: CR -> ContractTerms a -> ContractTerms a
setRoleFil CR_RFL ct = ct {contractRole = CR_RPA}
setRoleFil _ ct      = ct {contractRole = CR_RPL}

setRoleSel :: CR -> ContractTerms a -> ContractTerms a
setRoleSel CR_RFL ct = ct {contractRole = CR_RPL}
setRoleSel _ ct      = ct {contractRole = CR_RPA}

termsFromStructure :: ContractStructure a -> Maybe (ContractTerms a)
termsFromStructure cs = case reference cs of
  ReferenceTerms rt -> Just rt
  ReferenceId _     -> Nothing

-- |Determine the maturity of a contract
maturity ::
  ActusFrac a =>
  -- | Contract terms
  ContractTerms a ->
  -- | Maturity, if available
  Maybe LocalTime
maturity ContractTerms {contractType = PAM, ..} = maturityDate
maturity ContractTerms {contractType = LAM, maturityDate = md@(Just _)} = md
maturity
  ContractTerms
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
            let previousEvents = generateRecurrentSchedule pranx prcl statusDate scheduleConfig
                f1 = (\ShiftedDay {..} -> calculationDay > statusDate <-> ipcl)
                f2 = (\ShiftedDay {..} -> calculationDay == statusDate)
                ShiftedDay {calculationDay = lastEventCalcDay} = head . filter f2 . filter f1 $ previousEvents
             in (lastEventCalcDay, nt / prnxt)
          | otherwise = (pranx, nt / prnxt - 1)
        m = lastEvent <+> (prcl {n = n prcl * _ceiling remainingPeriods})
     in endOfMonthConvention scheduleConfig >>= \d -> return $ applyEOMC lastEvent prcl d m
maturity ContractTerms {contractType = NAM, maturityDate = md@(Just _)} = md
maturity
  ContractTerms
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
            let previousEvents = generateRecurrentSchedule pranx prcl statusDate scheduleConfig
                f = (\ShiftedDay {..} -> calculationDay == statusDate)
                ShiftedDay {calculationDay = lastEventCalcDay} = head . filter f $ previousEvents
             in lastEventCalcDay

        yLastEventPlusPRCL = yearFraction dcc lastEvent (lastEvent <+> prcl) Nothing
        redemptionPerCycle = prnxt - (yLastEventPlusPRCL * ipnr * nt)
        remainingPeriods = _ceiling $ (nt / redemptionPerCycle) - 1
        m = lastEvent <+> prcl {n = n prcl * remainingPeriods}
     in endOfMonthConvention scheduleConfig >>= \d -> return $ applyEOMC lastEvent prcl d m
maturity
  ContractTerms
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
            let previousEvents = generateRecurrentSchedule statusDate prcl pranx scheduleConfig
             in calculationDay . head . sortOn (Down . calculationDay) . filter (\ShiftedDay {..} -> calculationDay > statusDate) $ previousEvents
        timeFromLastEventPlusOneCycle = yearFraction dcc lastEvent (lastEvent <+> prcl) Nothing
        redemptionPerCycle = prnxt - timeFromLastEventPlusOneCycle * ipnr * nt
        remainingPeriods = _ceiling $ (nt / redemptionPerCycle) - 1
    in Just . calculationDay . applyBDCWithCfg scheduleConfig $ lastEvent <+> prcl { n = remainingPeriods }
maturity
  ContractTerms
    { contractType = ANN,
      amortizationDate = ad@(Just _)
    } = ad
maturity
  ContractTerms
    { contractType = ANN,
      amortizationDate = Nothing,
      maturityDate = md@(Just _)
    } = md
maturity _ = Nothing

-- Principal at Maturity (PAM)

_SCHED_IED_PAM :: ContractTerms a -> [ShiftedDay]
_SCHED_IED_PAM
  ContractTerms
    { scheduleConfig,
      initialExchangeDate = Just ied
    } = [applyBDCWithCfg scheduleConfig ied]
_SCHED_IED_PAM _ = []

_SCHED_MD_PAM :: ActusFrac a => ContractTerms a -> [ShiftedDay]
_SCHED_MD_PAM
  ct@ContractTerms
    { maturityDate,
      scheduleConfig
    } = case maturityDate <|> maturity ct of
    Just m  -> [let d = applyBDCWithCfg scheduleConfig m in d {paymentDay = m}]
    Nothing -> []

_SCHED_PP_PAM :: ContractTerms a -> [ShiftedDay]
_SCHED_PP_PAM
  ContractTerms
    { prepaymentEffect = Just PPEF_N
    } = []
_SCHED_PP_PAM
  ContractTerms
    { cycleAnchorDateOfOptionality = Just opanx,
      cycleOfOptionality = Just opcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentSchedule opanx opcl md scheduleConfig
_SCHED_PP_PAM
  ContractTerms
    { cycleAnchorDateOfOptionality = Nothing,
      cycleOfOptionality = Just opcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentSchedule (ied <+> opcl) opcl md scheduleConfig
_SCHED_PP_PAM _ = []

_SCHED_PY_PAM :: ContractTerms a -> [ShiftedDay]
_SCHED_PY_PAM
  ContractTerms
    { penaltyType = Just PYTP_O
    } = []
_SCHED_PY_PAM ct = _SCHED_PP_PAM ct

_SCHED_FP_PAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_FP_PAM
  ContractTerms
    { feeRate = Nothing
    } = []
_SCHED_FP_PAM
  ct@ContractTerms
    { cycleAnchorDateOfFee = Just feanx,
      cycleOfFee = Just fecl,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule feanx fecl {includeEndDay = True} m scheduleConfig
    Nothing -> []
_SCHED_FP_PAM
  ct@ContractTerms
    { cycleAnchorDateOfFee = Nothing,
      cycleOfFee = Just fecl,
      initialExchangeDate = Just ied,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule (ied <+> fecl) fecl {includeEndDay = True} m scheduleConfig
    Nothing -> []
_SCHED_FP_PAM _ = []

_SCHED_PRD_PAM :: ContractTerms a -> [ShiftedDay]
_SCHED_PRD_PAM
  ContractTerms
    { scheduleConfig,
      purchaseDate = Just prd
    } = [applyBDCWithCfg scheduleConfig prd]
_SCHED_PRD_PAM _ = []

_SCHED_TD_PAM :: ContractTerms a -> [ShiftedDay]
_SCHED_TD_PAM
  ContractTerms
    { scheduleConfig,
      terminationDate = Just td
    } = [applyBDCWithCfg scheduleConfig td]
_SCHED_TD_PAM _ = []

_SCHED_IP_PAM ::
  ActusFrac a =>
  ContractTerms a ->
  [ShiftedDay]
_SCHED_IP_PAM
  ct@ContractTerms
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      capitalizationEndDate = ipced,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let s = generateRecurrentSchedule ipanx ipcl {includeEndDay = True} m scheduleConfig
       in filter (\d -> Just (calculationDay d) > ipced) s
    Nothing -> []
_SCHED_IP_PAM
  ct@ContractTerms
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      initialExchangeDate = Just ied,
      capitalizationEndDate = ipced,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let s = generateRecurrentSchedule (ied <+> ipcl) ipcl {includeEndDay = True} m scheduleConfig
       in filter (\d -> Just (calculationDay d) > ipced) s
    Nothing -> []
_SCHED_IP_PAM _ = []

_SCHED_IPCI_PAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_IPCI_PAM
  ct@ContractTerms
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      capitalizationEndDate = Just ipced,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let s = generateRecurrentSchedule ipanx ipcl {includeEndDay = True} m scheduleConfig
       in filter (\d -> calculationDay d < ipced) s ++ [applyBDCWithCfg scheduleConfig ipced]
    Nothing -> []
_SCHED_IPCI_PAM
  ct@ContractTerms
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      initialExchangeDate = Just ied,
      capitalizationEndDate = Just ipced,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let s = generateRecurrentSchedule (ied <+> ipcl) ipcl {includeEndDay = True} m scheduleConfig
       in filter (\d -> calculationDay d < ipced) s ++ [applyBDCWithCfg scheduleConfig ipced]
    Nothing -> []
_SCHED_IPCI_PAM _ = []

_SCHED_RR_PAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_RR_PAM
  ct@ContractTerms
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      statusDate,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let tt = generateRecurrentSchedule rranx rrcl {includeEndDay = False} m scheduleConfig
       in fromMaybe [] (inf tt (mkShiftedDay statusDate) <&> flip delete tt)
    Nothing -> []
_SCHED_RR_PAM
  ct@ContractTerms
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Nothing,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule rranx rrcl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_RR_PAM
  ct@ContractTerms
    { cycleAnchorDateOfRateReset = Nothing,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      initialExchangeDate = Just ied,
      statusDate,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let tt = generateRecurrentSchedule (ied <+> rrcl) rrcl {includeEndDay = False} m scheduleConfig
       in fromMaybe [] (inf tt (mkShiftedDay statusDate) <&> flip delete tt)
    Nothing -> []
_SCHED_RR_PAM
  ct@ContractTerms
    { cycleAnchorDateOfRateReset = Nothing,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Nothing,
      initialExchangeDate = Just ied,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule (ied <+> rrcl) rrcl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_RR_PAM
  ContractTerms
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Nothing,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig rranx] -- if no cycle then only start (if specified) and end dates (see ScheduleFactory.java)
_SCHED_RR_PAM _ = []

_SCHED_RRF_PAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_RRF_PAM
  ct@ContractTerms
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      statusDate,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let tt = generateRecurrentSchedule rranx rrcl {includeEndDay = False} m scheduleConfig
       in maybeToList (L.find (\ShiftedDay {..} -> calculationDay > statusDate) tt)
    Nothing -> []
_SCHED_RRF_PAM
  ct@ContractTerms
    { cycleAnchorDateOfRateReset = Nothing,
      cycleOfRateReset = Just rrcl,
      nextResetRate = Just _,
      initialExchangeDate = Just ied,
      statusDate,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m ->
      let tt = generateRecurrentSchedule (ied <+> rrcl) rrcl m scheduleConfig
       in maybeToList (L.find (\ShiftedDay {..} -> calculationDay > statusDate) tt)
    Nothing -> []
_SCHED_RRF_PAM _ = []

_SCHED_SC_PAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_SC_PAM ContractTerms {scalingEffect = Just SE_OOO} = []
_SCHED_SC_PAM
  ct@ContractTerms
    { cycleAnchorDateOfScalingIndex = Just scanx,
      cycleOfScalingIndex = Just sccl,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule scanx sccl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_SC_PAM
  ct@ContractTerms
    { cycleAnchorDateOfScalingIndex = Nothing,
      cycleOfScalingIndex = Just sccl,
      initialExchangeDate = Just ied,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule (ied <+> sccl) sccl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_SC_PAM _ = []

-- Linear Amortizer (LAM)

_SCHED_PR_LAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_PR_LAM
  ct@ContractTerms
    { cycleAnchorDateOfPrincipalRedemption = Just pranx,
      cycleOfPrincipalRedemption = Just prcl,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule pranx prcl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_PR_LAM
  ct@ContractTerms
    { cycleAnchorDateOfPrincipalRedemption = Nothing,
      cycleOfPrincipalRedemption = Just prcl,
      maturityDate,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule (ied <+> prcl) prcl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_PR_LAM _ = []

_SCHED_MD_LAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_MD_LAM
  ct@ContractTerms
    { maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> [applyBDCWithCfg scheduleConfig m]
    Nothing -> []

_SCHED_IPCB_LAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_IPCB_LAM ContractTerms {..} | interestCalculationBase /= Just IPCB_NTL = []
_SCHED_IPCB_LAM
  ct@ContractTerms
    { cycleAnchorDateOfInterestCalculationBase = Just ipcbanx,
      cycleOfInterestCalculationBase = Just ipcbcl,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule ipcbanx ipcbcl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_IPCB_LAM
  ct@ContractTerms
    { cycleAnchorDateOfInterestCalculationBase = Nothing,
      cycleOfInterestCalculationBase = Just ipcbcl,
      initialExchangeDate = Just ied,
      maturityDate,
      scheduleConfig
    } = case maturity ct <|> maturityDate of
    Just m  -> generateRecurrentSchedule (ied <+> ipcbcl) ipcbcl {includeEndDay = False} m scheduleConfig
    Nothing -> []
_SCHED_IPCB_LAM _ = []

-- Negative Amortizer (NAM)

_SCHED_IP_NAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_IP_NAM ct@ContractTerms {..} =
  let m = maturityDate <|> maturity ct
      s
        | isNothing cycleAnchorDateOfPrincipalRedemption = liftM2 (<+>) initialExchangeDate cycleOfPrincipalRedemption
        | otherwise = cycleAnchorDateOfPrincipalRedemption

      v = liftM4 generateRecurrentSchedule s cycleOfPrincipalRedemption m (Just scheduleConfig)

      r
        | isJust cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment
        | isJust cycleOfInterestPayment = liftM2 (<+>) initialExchangeDate cycleOfInterestPayment
        | otherwise = Nothing

      _T = liftM2 (<->) s cycleOfPrincipalRedemption

      u
        | isNothing cycleAnchorDateOfInterestPayment && isNothing cycleOfInterestPayment = Nothing
        | isJust capitalizationEndDate && Just True == liftM2 (>) capitalizationEndDate _T = Nothing
        | otherwise = liftM4 generateRecurrentSchedule r ((\c -> c {includeEndDay = True}) <$> cycleOfInterestPayment) m (Just scheduleConfig)

      result = nub <$> liftM2 (++) u v

      result'
        | isJust result && isJust capitalizationEndDate = filter (\ShiftedDay {..} -> Just calculationDay > capitalizationEndDate) <$> result
        | otherwise = result
   in fromMaybe [] result'

_SCHED_IPCI_NAM :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_IPCI_NAM ct@ContractTerms {..} =
  let m = maturity ct <|> maturityDate
      s
        | isNothing cycleAnchorDateOfPrincipalRedemption = liftM2 (<+>) initialExchangeDate cycleOfPrincipalRedemption
        | otherwise = cycleAnchorDateOfPrincipalRedemption

      v = liftM4 generateRecurrentSchedule s cycleOfPrincipalRedemption m (Just scheduleConfig)

      r
        | isJust capitalizationEndDate = capitalizationEndDate
        | isJust cycleAnchorDateOfInterestPayment = cycleAnchorDateOfInterestPayment
        | isJust cycleOfInterestPayment = liftM2 (<+>) initialExchangeDate cycleOfInterestPayment
        | otherwise = Nothing

      _T = liftM2 (<->) s cycleOfPrincipalRedemption

      u
        | isNothing cycleAnchorDateOfInterestPayment && isNothing cycleOfInterestPayment = Nothing
        | isJust capitalizationEndDate && Just True == liftM2 (>) capitalizationEndDate _T = Nothing
        | otherwise = liftM4 generateRecurrentSchedule r ((\c -> c {includeEndDay = True}) <$> cycleOfInterestPayment) m (Just scheduleConfig)

      result = Just $ nub (fromMaybe [] u ++ fromMaybe [] v)

      result'
        | isJust result && isJust capitalizationEndDate = filter (\ShiftedDay {..} -> Just calculationDay <= capitalizationEndDate) <$> result
        | otherwise = Nothing
   in fromMaybe [] result'

-- Annuity (ANN)

_SCHED_PRF_ANN :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_PRF_ANN
  ct@ContractTerms
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

_SCHED_DV_STK :: ContractTerms a -> [ShiftedDay]
_SCHED_DV_STK
  ContractTerms
    { cycleAnchorDateOfDividend = Just dvanx,
      cycleOfDividend = Just dvcl,
      nextDividendPaymentAmount = Nothing,
      scheduleConfig = scheduleConfig
    } = let tMax = LocalTime (addDays (10 * 365) $ localDay dvanx) (localTimeOfDay dvanx)
         in generateRecurrentSchedule dvanx dvcl tMax scheduleConfig
_SCHED_DV_STK
  ContractTerms
    { cycleAnchorDateOfDividend = Just dvanx,
      cycleOfDividend = Just dvcl,
      scheduleConfig = scheduleConfig
    } = let tMax = LocalTime (addDays (10 * 365) $ localDay dvanx) (localTimeOfDay dvanx)
          in generateRecurrentSchedule (dvanx <+> dvcl) dvcl tMax scheduleConfig
_SCHED_DV_STK _ = []

-- Options (OPTNS)

_SCHED_XD_OPTNS :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_XD_OPTNS
  ContractTerms
    { exerciseDate = Just xd,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig xd]
_SCHED_XD_OPTNS
  ContractTerms
    { maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_XD_OPTNS
  ct@ContractTerms
    { scheduleConfig
    } = case maturity ct of
          Just m  -> [applyBDCWithCfg scheduleConfig m]
          Nothing -> []

_SCHED_STD_OPTNS :: ActusFrac a =>
  ContractTerms a -> [ShiftedDay]
_SCHED_STD_OPTNS
  ContractTerms
    { scheduleConfig,
      maturityDate = Just md,
      settlementPeriod = Just stp
    } = [applyBDCWithCfg scheduleConfig (md <+> stp)]
_SCHED_STD_OPTNS
  ContractTerms
    { scheduleConfig
    , maturityDate = Just md
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_STD_OPTNS
  ContractTerms
    { scheduleConfig,
      exerciseDate = Just xd,
      settlementPeriod = Just stp
    } = [applyBDCWithCfg scheduleConfig (xd <+> stp)]
_SCHED_STD_OPTNS
  ContractTerms
    { scheduleConfig,
      exerciseDate = Just xd
    } = [applyBDCWithCfg scheduleConfig xd]
_SCHED_STD_OPTNS
  ct@ContractTerms
    { scheduleConfig
    } = case maturity ct of
          Just m  -> [applyBDCWithCfg scheduleConfig m]
          Nothing -> []

_SCHED_IP_SWPPV :: ContractTerms a -> [ShiftedDay]
_SCHED_IP_SWPPV
  ContractTerms
    { deliverySettlement = Just DS_D
    } = []
_SCHED_IP_SWPPV
  ContractTerms
    { cycleOfInterestPayment = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_IP_SWPPV
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentSchedule ipanx ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IP_SWPPV
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentSchedule (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IP_SWPPV _ = []

_SCHED_IPFX_SWPPV :: ContractTerms a -> [ShiftedDay]
_SCHED_IPFX_SWPPV
  ContractTerms
    { deliverySettlement = Just DS_S
    } = []
_SCHED_IPFX_SWPPV
  ContractTerms
    { cycleOfInterestPayment = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_IPFX_SWPPV
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentSchedule ipanx ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFX_SWPPV
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentSchedule (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFX_SWPPV _ = []

_SCHED_IPFL_SWPPV :: ContractTerms a -> [ShiftedDay]
_SCHED_IPFL_SWPPV
  ContractTerms
    { deliverySettlement = Just DS_S
    } = []
_SCHED_IPFL_SWPPV
  ContractTerms
    { cycleOfInterestPayment = Nothing,
      maturityDate = Just md,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig md]
_SCHED_IPFL_SWPPV
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentSchedule ipanx ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFL_SWPPV
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentSchedule (ied <+> ipcl) ipcl {includeEndDay = True} md scheduleConfig
_SCHED_IPFL_SWPPV _ = []

_SCHED_RR_SWPPV :: ContractTerms a -> [ShiftedDay]
_SCHED_RR_SWPPV
  ContractTerms
    { cycleAnchorDateOfRateReset = Just rranx,
      cycleOfRateReset = Just rrcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentSchedule rranx rrcl {includeEndDay = False} md scheduleConfig
_SCHED_RR_SWPPV
  ContractTerms
    { cycleOfRateReset = Just rrcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentSchedule (ied <+> rrcl) rrcl {includeEndDay = False} md scheduleConfig
_SCHED_RR_SWPPV
  ContractTerms
    { cycleOfRateReset = Nothing,
      cycleAnchorDateOfRateReset = Just rranx,
      scheduleConfig
    } = [applyBDCWithCfg scheduleConfig rranx]
_SCHED_RR_SWPPV _ = []

_SCHED_XD_CEG :: ContractTerms a -> [ShiftedDay]
_SCHED_XD_CEG _ = []

_SCHED_MD_CEG :: ActusFrac a => ContractTerms a -> [ShiftedDay]
_SCHED_MD_CEG
  ct@ContractTerms
    { maturityDate = md
    } =
    let refs = maximum <$> mapM maturityDate (mapMaybe termsFromStructure (filter (\cs -> referenceRole cs == COVE) $ contractStructure ct))
     in case md <|> maturity ct <|> refs of
          Just m  -> [mkShiftedDay m]
          Nothing -> []

_SCHED_FP_CEG ::
  ActusFrac a =>
  ContractTerms a ->
  [ShiftedDay]
_SCHED_FP_CEG
  ct@ContractTerms
    { cycleAnchorDateOfFee = Just feanx,
      cycleOfFee = Just fecl,
      maturityDate = md,
      scheduleConfig
    } =
    let refs = maximum <$> mapM maturityDate (mapMaybe termsFromStructure (filter (\cs -> referenceRole cs == COVE) $ contractStructure ct))
     in case md <|> maturity ct <|> refs of
          Just m  -> generateRecurrentSchedule feanx fecl {includeEndDay = True} m scheduleConfig
          Nothing -> []
_SCHED_FP_CEG _ = []

_SCHED_MD_CEC :: ContractTerms a -> [ShiftedDay]
_SCHED_MD_CEC
  ct@ContractTerms
    {
    } = case mapM maturityDate $ mapMaybe termsFromStructure (filter (\cs -> referenceRole cs == COVE) $ contractStructure ct) of
    Just m  -> [mkShiftedDay $ maximum m]
    Nothing -> []

_SCHED_IP_CLM :: ContractTerms a -> [ShiftedDay]
_SCHED_IP_CLM
  ContractTerms
    { maturityDate = Just md,
      scheduleConfig
    } = [let d = applyBDCWithCfg scheduleConfig md in d { paymentDay = md }]
_SCHED_IP_CLM _ = []

_SCHED_IPCI_CLM :: ContractTerms a -> [ShiftedDay]
_SCHED_IPCI_CLM
  ContractTerms
    { nominalInterestRate = Nothing
    } = []
_SCHED_IPCI_CLM
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Just ipanx,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      scheduleConfig
    } = generateRecurrentSchedule ipanx ipcl {includeEndDay = False} md scheduleConfig
_SCHED_IPCI_CLM
  ContractTerms
    { cycleAnchorDateOfInterestPayment = Nothing,
      cycleOfInterestPayment = Just ipcl,
      maturityDate = Just md,
      initialExchangeDate = Just ied,
      scheduleConfig
    } = generateRecurrentSchedule (ied <+> ipcl) ipcl {includeEndDay = False} md scheduleConfig
_SCHED_IPCI_CLM _ = []
