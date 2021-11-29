{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS contract schedules

The implementation is a transliteration of the ACTUS specification v1.1

-}

module Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule
  ( schedule
  , maturity
  )
where

import           Control.Applicative                                      (Alternative ((<|>)))
import           Data.Ord                                                 (Down (..))
import           Data.Sort                                                (sortOn)
import           Data.Time                                                (LocalTime)
import           Language.Marlowe.ACTUS.Domain.BusinessEvents             (EventType (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms              (CT (..), ContractTermsPoly (..), Cycle (..),
                                                                           ScheduleConfig (..))
import           Language.Marlowe.ACTUS.Domain.Ops                        as O (ActusNum (..), ActusOps (..),
                                                                                YearFractionOps (_y))
import           Language.Marlowe.ACTUS.Domain.Schedule                   (ShiftedDay (..))
import           Language.Marlowe.ACTUS.Model.SCHED.ContractScheduleModel
import           Language.Marlowe.ACTUS.Utility.DateShift                 (applyBDCWithCfg)
import           Language.Marlowe.ACTUS.Utility.ScheduleGenerator         (applyEOMC,
                                                                           generateRecurrentScheduleWithCorrections,
                                                                           (<+>), (<->))

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
