{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS state transformation functions -}
module Actus.Model.StateTransition
  ( CtxSTF(..)
  , stateTransition
  ) where

import Actus.Domain
  ( ActusFrac(..)
  , ActusOps(..)
  , CEGE(..)
  , CT(..)
  , ContractState(..)
  , ContractTerms(..)
  , EventType(..)
  , FEB(..)
  , IPCB(..)
  , OPTP(..)
  , RiskFactors(..)
  , SCEF(..)
  , ShiftedDay(..)
  , sign
  )
import qualified Actus.Domain.ContractState as L
  ( accruedFees
  , accruedInterest
  , accruedInterestFirstLeg
  , accruedInterestSecondLeg
  , exerciseAmount
  , interestCalculationBase
  , interestScalingMultiplier
  , lastInterestPeriod
  , nextPrincipalRedemptionPayment
  , nominalInterest
  , notionalPrincipal
  , notionalScalingMultiplier
  , statusDate
  )
import Actus.Utility (annuity, inf, sup, (<+>))
import Actus.Utility.YearFraction (yearFraction)
import Control.Lens
import Control.Monad.Reader (Reader, reader)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Time.LocalTime (LocalTime)

-- |The context for state transitions provides the contract terms in addition with
-- schedules and the maturity of the contract. Furthermore a function to retrieve
-- risk factors is available.
data CtxSTF a = CtxSTF
  { -- | Contract terms
    contractTerms   :: ContractTerms a,
    -- | Fee payment schedule
    fpSchedule      :: [LocalTime],
    -- | Principal redemption schedule
    prSchedule      :: [LocalTime],
    -- | Interest payment schedule
    ipSchedule      :: [LocalTime],
    -- | Maturity
    maturity        :: Maybe LocalTime,
    -- | Riskfactors per event and time
    riskFactors     :: String -> EventType -> LocalTime -> RiskFactors a,
    -- | Cash flows from underlying contracts
    referenceStates :: [[((String, EventType, ShiftedDay), ContractState a, a)]]
  }

-- |A state transition updates the contract state based on the type of event and the time.
-- `CtxSTF` provides in particular the contract terms and risk factors.
stateTransition ::
  ActusFrac a =>
  -- | Event type
  EventType ->
  -- | Time
  LocalTime ->
  -- | Contract state
  ContractState a ->
  -- | Updated contract state
  Reader (CtxSTF a) (ContractState a)
stateTransition ev t sn = reader stateTransition'
  where
    stateTransition' CtxSTF {..} = stf ev (riskFactors (contractId contractTerms) ev t) contractTerms sn
      where
        stf AD _ ContractTerms { contractType = CSH } st        = _STF_AD_CSH st t
        stf AD _ ct st                                          = _STF_AD_ALL ct st t
        stf IED _ ct@ContractTerms { contractType = PAM} st     = _STF_IED_PAM ct st t
        stf IED _ ct@ContractTerms { contractType = CLM} st     = _STF_IED_PAM ct st t
        stf IED _ ct@ContractTerms { contractType = SWPPV} st   = _STF_IED_SWPPV ct st t
        stf IED _ ct@ContractTerms { contractType = LAM } st    = _STF_IED_LAM ct st t
        stf IED _ ct@ContractTerms { contractType = NAM } st    = _STF_IED_LAM ct st t
        stf IED _ ct@ContractTerms { contractType = ANN } st    = _STF_IED_LAM ct st t
        stf PR _ ct@ContractTerms { contractType = LAM } st     = _STF_PR_LAM ct st t
        stf PR _ ct@ContractTerms { contractType = NAM } st     = _STF_PR_NAM ct st t
        stf PR _ ct@ContractTerms { contractType = ANN } st     = _STF_PR_NAM ct st t
        stf MD _ ct st                                          = _STF_MD_ALL ct st t
        stf PP rf ct@ContractTerms { contractType = PAM } st    = _STF_PP_PAM fs rf ct st t
        stf PP rf ct@ContractTerms { contractType = LAM } st    = _STF_PP_LAM fs rf ct st t
        stf PP rf ct@ContractTerms { contractType = NAM } st    = _STF_PP_LAM fs rf ct st t
        stf PP rf ct@ContractTerms { contractType = ANN } st    = _STF_PP_LAM fs rf ct st t
        stf PY _ ct@ContractTerms { contractType = PAM } st     = _STF_PY_PAM fs ct st t
        stf PY _ ct@ContractTerms { contractType = LAM } st     = _STF_PY_LAM fs ct st t
        stf PY _ ct@ContractTerms { contractType = NAM } st     = _STF_PY_LAM fs ct st t
        stf PY _ ct@ContractTerms { contractType = ANN } st     = _STF_PY_LAM fs ct st t
        stf FP _ ct@ContractTerms { contractType = PAM } st     = _STF_FP_PAM ct st t
        stf FP _ ct@ContractTerms { contractType = LAM } st     = _STF_FP_LAM ct st t
        stf FP _ ct@ContractTerms { contractType = NAM } st     = _STF_FP_LAM ct st t
        stf FP _ ct@ContractTerms { contractType = ANN } st     = _STF_FP_LAM ct st t
        stf FP _ ct@ContractTerms { contractType = CEG } st     = _STF_FP_LAM ct st t
        stf PRD _ ct@ContractTerms { contractType = PAM } st    = _STF_PY_PAM fs ct st t
        stf PRD _ ct@ContractTerms { contractType = LAM } st    = _STF_PY_PAM fs ct st t
        stf PRD _ ct@ContractTerms { contractType = NAM } st    = _STF_PY_PAM fs ct st t
        stf PRD _ ct@ContractTerms { contractType = ANN } st    = _STF_PY_PAM fs ct st t
        stf PRD _ ct@ContractTerms { contractType = CEG } st    = _STF_PRD_CEG ct st t
        stf TD _ _ st                                           = _STF_TD_ALL st t
        stf IP _ ct@ContractTerms { contractType = SWPPV } st   = _STF_IP_SWPPV ct st t
        stf IP _ ct@ContractTerms { contractType = CLM } st     = _STF_IP_SWPPV ct st t
        stf IP _ ct st                                          = _STF_IP_PAM ct st t
        stf IPFX _ ct@ContractTerms { contractType = SWPPV } st = _STF_IPFX_SWPPV ct st t
        stf IPFL _ ct@ContractTerms { contractType = SWPPV } st = _STF_IPFL_SWPPV ct st t
        stf IPCI _ ct@ContractTerms { contractType = PAM } st   = _STF_IPCI_PAM ct st t
        stf IPCI _ ct@ContractTerms { contractType = CLM } st   = _STF_IPCI_PAM ct st t
        stf IPCI _ ct@ContractTerms { contractType = LAM } st   = _STF_IPCI_LAM ct st t
        stf IPCI _ ct@ContractTerms { contractType = NAM } st   = _STF_IPCI_LAM ct st t
        stf IPCI _ ct@ContractTerms { contractType = ANN } st   = _STF_IPCI_LAM ct st t
        stf IPCB _ ct@ContractTerms { contractType = LAM } st   = _STF_IPCB_LAM fs ct st t
        stf IPCB _ ct@ContractTerms { contractType = NAM } st   = _STF_IPCB_LAM fs ct st t
        stf IPCB _ ct@ContractTerms { contractType = ANN } st   = _STF_IPCB_LAM fs ct st t
        stf RR rf ct@ContractTerms { contractType = PAM } st    = _STF_RR_PAM fs rf ct st t
        stf RR rf ct@ContractTerms { contractType = CLM } st    = _STF_RR_PAM fs rf ct st t
        stf RR rf ct@ContractTerms { contractType = LAM } st    = _STF_RR_LAM fs rf ct st t
        stf RR rf ct@ContractTerms { contractType = NAM } st    = _STF_RR_LAM fs rf ct st t
        stf RR rf ct@ContractTerms { contractType = ANN } st    = _STF_RR_ANN ps fs rf ct st t
        stf RR rf ct@ContractTerms { contractType = SWPPV } st  = _STF_RR_SWPPV rf ct st t
        stf RRF rf ct@ContractTerms { contractType = PAM } st   = _STF_RRF_PAM fs rf ct st t
        stf RRF rf ct@ContractTerms { contractType = LAM } st   = _STF_RRF_LAM fs rf ct st t
        stf RRF rf ct@ContractTerms { contractType = NAM } st   = _STF_RRF_LAM fs rf ct st t
        stf RRF rf ct@ContractTerms { contractType = ANN } st   = _STF_RRF_ANN ps fs rf ct st t
        stf PRF _ ct@ContractTerms { contractType = ANN } st    = _STF_PRF_ANN ps fs ct st t
        stf SC rf ct@ContractTerms { contractType = PAM } st    = _STF_SC_PAM fs rf ct st t
        stf SC rf ct@ContractTerms { contractType = LAM } st    = _STF_SC_LAM fs rf ct st t
        stf SC rf ct@ContractTerms { contractType = NAM } st    = _STF_SC_LAM fs rf ct st t
        stf SC rf ct@ContractTerms { contractType = ANN } st    = _STF_SC_LAM fs rf ct st t
        stf XD rf ct@ContractTerms { contractType = OPTNS } st  = _STF_XD_OPTNS rf ct st t
        stf XD rf ct@ContractTerms { contractType = FUTUR } st  = _STF_XD_FUTUR rf ct st t
        stf XD _ ct@ContractTerms { contractType = CEG } st     = _STF_XD_CEG referenceStates ct st t
        stf XD rf ct@ContractTerms { contractType = CEC } st    = _STF_XD_CEC referenceStates rf ct st t
        stf _ _   ct@ContractTerms { contractType = SWAPS} st   = _STF_XX_SWAPS referenceStates ev ct st t

        -----------------------
        -- Credit Event (CE) --
        -----------------------
        stf CE rf ct st                                         = stf AD rf ct st
        -------------
        -- Default --
        -------------
        stf _ _ _ _                                             = sn

        fs =
          FeeSchedule
            (fromMaybe t (sup fpSchedule t))
            (fromMaybe t (inf fpSchedule t))

        ps =
          PrincipalRedemptionSchedule
            ( let principalRedemptionDates = prSchedule ++ maybeToList maturity
               in filter (> sd sn) principalRedemptionDates
            )
            (fromMaybe t (inf prSchedule t))

data FeeSchedule = FeeSchedule
  { latestFeePayment :: LocalTime,
    nextFeePayment   :: LocalTime
  }

data PrincipalRedemptionSchedule = PrincipalRedemptionSchedule
  { laterPrincipalRedemptionDates :: [LocalTime],
    nextPrincipalRedemption       :: LocalTime
  }

thisOr0 :: Num a => Maybe a -> a
thisOr0 = fromMaybe 0

---------------------
-- Monitoring (AD) --
---------------------

_STF_AD_CSH :: ContractState a -> LocalTime -> ContractState a
_STF_AD_CSH s t = s & L.statusDate .~ t

_STF_AD_ALL :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_AD_ALL
  ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in s & L.statusDate .~ t
          & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
_STF_AD_ALL _ s _ = s

----------------------------
-- Initial Exchange (IED) --
----------------------------

_STF_IED_PAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IED_PAM
  ContractTerms
    { nominalInterestRate,
      notionalPrincipal = Just nt,
      accruedInterest = Just ipac,
      contractRole
    }
  s
  t =
    s & L.notionalPrincipal .~ sign contractRole * nt
      & L.nominalInterest .~ thisOr0 nominalInterestRate
      & L.accruedInterest .~ ipac
      & L.statusDate .~ t
_STF_IED_PAM
  ContractTerms
    { nominalInterestRate = Just ipnr,
      notionalPrincipal = Just nt,
      cycleAnchorDateOfInterestPayment = Just ipanx,
      dayCountConvention = Just dcc,
      contractRole,
      maturityDate
    }
  s
  t =
    let nt' = sign contractRole * nt
        timeFromInterestPaymentAnchorToNow = yearFraction dcc ipanx t maturityDate
        timeFromInterestPaymentAnchorToNow' = yearFraction dcc ipanx t Nothing
        ipnr' = ipnr
     in s & L.notionalPrincipal .~ nt'
          & L.nominalInterest .~ ipnr'
          & L.accruedInterest .~ timeFromInterestPaymentAnchorToNow' * timeFromInterestPaymentAnchorToNow * nt' * ipnr' -- TODO: correct?
          & L.statusDate .~ t
_STF_IED_PAM
  ContractTerms
    { nominalInterestRate = Nothing,
      notionalPrincipal = Just nt,
      contractRole
    }
  s
  t =
    s & L.notionalPrincipal .~ sign contractRole * nt
      & L.accruedInterest .~ 0
      & L.statusDate .~ t
_STF_IED_PAM _ s _ = s

_STF_IED_SWPPV :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IED_SWPPV
  ContractTerms
    { notionalPrincipal = Just nt,
      nominalInterestRate2 = Just ipnr2,
      contractRole
    }
  s
  t =
    s & L.notionalPrincipal .~ sign contractRole * nt
      & L.nominalInterest .~ ipnr2
      & L.accruedInterest .~ 0
      & L.accruedInterestFirstLeg ?~ 0
      & L.accruedInterestSecondLeg ?~ 0
      & L.statusDate .~ t
_STF_IED_SWPPV _ s _ = s

_STF_XX_SWAPS :: [[((String, EventType, ShiftedDay), ContractState a, a)]] -> EventType -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
  {-
_STF_XX_SWAPS
  referenceStates
  ev
  ContractTerms
    { contractStructure = [_, _]
    }
  s
  t =
    let y = join referenceStates
        x = lookup (ev,t) (map (\(a,b,c) -> ((a,calculationDay b),c)) y)
     in traceShow ("STF", ev, refId <$> x) $ fromMaybe s x
     -}
_STF_XX_SWAPS _ _ _ s _ = s

_STF_IED_LAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IED_LAM
  ct@ContractTerms
    { notionalPrincipal = Just nt,
      nominalInterestRate = Just ipnr,
      dayCountConvention = Just dcc,
      maturityDate,
      contractRole
    }
  s
  t =
    let nt' = sign contractRole * nt
        ipcb' = interestCalculationBase' ct
          where
            interestCalculationBase' ContractTerms {interestCalculationBase = Just IPCB_NT} = nt'
            interestCalculationBase' ContractTerms {interestCalculationBaseA = Just ipcba}  = sign contractRole * ipcba
            interestCalculationBase' _                                                      = 0
        ipac' = interestAccrued' ct
          where
            interestAccrued' ContractTerms {accruedInterest = Just ipac} = sign contractRole * ipac
            interestAccrued' ContractTerms {cycleAnchorDateOfInterestPayment = Just ipanx}
              | ipanx < t =
                let timeFromInterestPaymentAnchorToNow = yearFraction dcc ipanx t maturityDate
                 in timeFromInterestPaymentAnchorToNow * nt' * ipcb'
            interestAccrued' _ = 0
     in s & L.notionalPrincipal .~ nt'
          & L.nominalInterest .~ ipnr
          & L.accruedInterest .~ ipac'
          & L.interestCalculationBase .~ ipcb'
          & L.statusDate .~ t
_STF_IED_LAM _ s _ = s

-------------------------------
-- Principal Redemption (PR) --
-------------------------------

_STF_PR_LAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PR_LAM
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      feeRate,
      contractRole,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        nt' = (s ^. L.notionalPrincipal)
                  - sign contractRole * ((s ^. L.nextPrincipalRedemptionPayment)
                  - sign contractRole * _max 0 (abs (s ^. L.nextPrincipalRedemptionPayment) - abs (s ^. L.notionalPrincipal)))
        ipcb' = interestCalculationBase' ct
          where
            interestCalculationBase' ContractTerms {interestCalculationBase = Just IPCB_NTL} = s ^. L.interestCalculationBase
            interestCalculationBase' _ = nt'
     in s & L.notionalPrincipal .~ nt'
          & L.accruedFees +~ timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 feeRate
          & L.interestCalculationBase .~ ipcb'
          & L.accruedInterest +~ (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase) * timeFromLastEvent
          & L.statusDate .~ t
_STF_PR_LAM _ s _ = s

_STF_PR_NAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PR_NAM
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      feeRate,
      contractRole,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        ipac' = (s ^. L.accruedInterest) + (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase) * timeFromLastEvent
        nt' = (s ^. L.notionalPrincipal) - sign contractRole * r
          where
            r = ra - _max 0 (ra - abs (s ^. L.notionalPrincipal))
            ra = (s ^. L.nextPrincipalRedemptionPayment) - sign contractRole * ipac'
        ipcb' = interestCalculationBase' ct
          where
            interestCalculationBase' ContractTerms {interestCalculationBase = Just IPCB_NT} = nt'
            interestCalculationBase' _ = s ^. L.interestCalculationBase
     in s & L.notionalPrincipal .~ nt'
          & L.accruedFees +~ timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 feeRate
          & L.interestCalculationBase .~ ipcb'
          & L.accruedInterest .~ ipac'
          & L.statusDate .~ t
_STF_PR_NAM _ s _ = s

-------------------
-- Maturity (MD) --
-------------------

_STF_MD_ALL :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_MD_ALL _ s t =
  s & L.notionalPrincipal .~ 0
    & L.accruedInterest .~ 0
    & L.accruedFees .~ 0
    & L.statusDate .~ t

-------------------------------
-- Principal Prepayment (PP) --
-------------------------------

_STF_PP_PAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PP_PAM
  fs
  RiskFactors
    { pp_payoff
    }
  ct
  s
  t = _STF_PY_PAM fs ct s t & L.notionalPrincipal -~ pp_payoff

_STF_PP_LAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PP_LAM
  fs
  RiskFactors
    { pp_payoff
    }
  ct@ContractTerms
    { interestCalculationBase = Just IPCB_NT
    }
  s
  t =
    _STF_PY_PAM fs ct s t & L.notionalPrincipal .~ (s ^. L.notionalPrincipal) - pp_payoff
      & L.interestCalculationBase .~ (s ^. L.notionalPrincipal)
_STF_PP_LAM
  fs
  RiskFactors
    { pp_payoff
    }
  ct
  s
  t =
    _STF_PY_PAM fs ct s t & L.notionalPrincipal .~ (s ^. L.notionalPrincipal) - pp_payoff
      & L.interestCalculationBase .~ (s ^. L.interestCalculationBase)

--------------------------
-- Penalty Payment (PY) --
--------------------------

_STF_PY_PAM :: ActusFrac a => FeeSchedule -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PY_PAM
  _
  ContractTerms
    { dayCountConvention = Just dcc,
      notionalPrincipal = Just nt',
      maturityDate,
      feeBasis = Just FEB_N,
      feeRate = Just fer
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in s & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
          & L.accruedFees +~ timeFromLastEvent * fer * nt'
          & L.statusDate .~ t
_STF_PY_PAM
  FeeSchedule {..}
  ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate,
      contractRole,
      feeRate = Just fer
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        timeFromLatestFeePayment = yearFraction dcc latestFeePayment t maturityDate
        timeFromLatestToNextFeePayment = yearFraction dcc latestFeePayment nextFeePayment maturityDate
     in s & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
          & L.accruedFees .~ _max 0 (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * fer
          & L.statusDate .~ t
_STF_PY_PAM _ _ s _ = s

_STF_PY_LAM :: ActusFrac a => FeeSchedule -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PY_LAM
  FeeSchedule {..}
  ct@ContractTerms
    { feeRate = Just fer,
      dayCountConvention = Just dcc,
      maturityDate,
      contractRole
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        feac' = feeAccrued' ct
          where
            feeAccrued' ContractTerms {feeBasis = Just FEB_N} = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * fer
            feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * fer
              where
                timeFromLatestFeePayment = yearFraction dcc latestFeePayment t maturityDate
                timeFromLatestToNextFeePayment = yearFraction dcc latestFeePayment nextFeePayment maturityDate
     in s & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
          & L.accruedFees .~ feac'
          & L.statusDate .~ t
_STF_PY_LAM
  _
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        feac' = feeAccrued' ct
          where
            feeAccrued' ContractTerms {feeBasis = Just FEB_N} = s ^. L.accruedFees
            feeAccrued' _                                     = 0
     in s & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
          & L.accruedFees .~ feac'
          & L.statusDate .~ t
_STF_PY_LAM _ _ s _ = s

----------------------
-- Fee Payment (FP) --
----------------------

_STF_FP_PAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_FP_PAM
  ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in s & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
          & L.accruedFees .~ 0
          & L.statusDate .~ t
_STF_FP_PAM _ s _ = s

_STF_FP_LAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_FP_LAM
  ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in s & L.accruedInterest +~ timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
          & L.accruedFees .~ 0
          & L.statusDate .~ t
_STF_FP_LAM _ s _ = s

--------------------
-- Purchase (PRD) --
--------------------

_STF_PRD_CEG :: ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PRD_CEG
  ContractTerms
    { feeRate = Just fer
    }
  s
  t =
    s & L.accruedFees .~ fer
      & L.statusDate .~ t
_STF_PRD_CEG
  ContractTerms
    { contractType = CEG,
      feeAccrued = Just feac
    }
  s
  t =
    s & L.accruedFees .~ feac
      & L.statusDate .~ t
_STF_PRD_CEG _ s _ = s

----------------------
-- Termination (TD) --
----------------------

_STF_TD_ALL :: ActusFrac a => ContractState a -> LocalTime -> ContractState a
_STF_TD_ALL
  s
  t =
    s & L.notionalPrincipal .~ 0
      & L.accruedInterest .~ 0
      & L.accruedFees .~ 0
      & L.nominalInterest .~ 0
      & L.statusDate .~ t

---------------------------
-- Interest Payment (IP) --
---------------------------

_STF_IP_SWPPV :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IP_SWPPV _ s t =
  s & L.accruedInterest .~ 0
    & L.statusDate .~ t

_STF_IP_PAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IP_PAM
  ContractTerms
    { dayCountConvention = Just dcc,
      feeRate,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in s & L.accruedInterest .~ 0
          & L.accruedFees .~ timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 feeRate
          & L.statusDate .~ t
_STF_IP_PAM _ s _ = s

---------------------------------------
-- Interest Payment Fixed Leg (IPFX) --
---------------------------------------

_STF_IPFX_SWPPV :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IPFX_SWPPV
  ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    s & L.lastInterestPeriod ?~ yearFraction dcc (s ^. L.statusDate) t maturityDate
      & L.accruedInterestFirstLeg ?~ 0
      & L.statusDate .~ t
_STF_IPFX_SWPPV _ s _ = s

------------------------------------------
-- Interest Payment Floating Leg (IPFL) --
------------------------------------------

_STF_IPFL_SWPPV :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IPFL_SWPPV
  ContractTerms
    { contractType = SWPPV
    }
  s
  t =
    s & L.accruedInterestSecondLeg ?~ 0
      & L.statusDate .~ t
_STF_IPFL_SWPPV _ s _ = s

------------------------------------
-- Interest Capitalization (IPCI) --
------------------------------------

_STF_IPCI_PAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IPCI_PAM
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in _STF_IP_PAM ct s t & L.notionalPrincipal .~ (s ^. L.notionalPrincipal) + (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
_STF_IPCI_PAM _ s _ = s

_STF_IPCI_LAM :: ActusFrac a => ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IPCI_LAM
          ct@ContractTerms
            { dayCountConvention = Just dcc,
              maturityDate
            }
          s t =
              let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
                  nt' = (s ^. L.notionalPrincipal) + (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
                  ipcb' = interestCalculationBase ct
                    where
                      interestCalculationBase ContractTerms {interestCalculationBase = Just IPCB_NT} = nt'
                      interestCalculationBase _                                                      = s ^. L.interestCalculationBase
               in _STF_IP_PAM ct s t & L.notionalPrincipal .~ nt'
                                     & L.interestCalculationBase  .~ ipcb'
_STF_IPCI_LAM _ s _ = s

---------------------------------------------
-- Interest Calculation Base Fixing (IPCB) --
---------------------------------------------

_STF_IPCB_LAM :: ActusFrac a => FeeSchedule -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_IPCB_LAM fs ct s t = _STF_PY_LAM fs ct s t & L.interestCalculationBase .~ (s ^. L.notionalPrincipal)

-------------------------------
-- Rate Reset (RR) --
-------------------------------

_STF_RR_PAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RR_PAM
  fs
  RiskFactors
    { o_rf_RRMO
    }
  ct@ContractTerms
    { feeBasis = Just FEB_N,
      feeRate = Just fer,
      lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp,
      dayCountConvention = Just dcc,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - (s ^. L.nominalInterest)) rrpf) rrpc
        ipnr' = _min (_max (s ^. L.nominalInterest + delta_r) rrlf) rrlc
     in _STF_PY_PAM fs ct s t
          & L.accruedInterest .~ (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
          & L.accruedFees .~ (s ^. L.accruedFees) + timeFromLastEvent * fer * (s ^. L.notionalPrincipal)
          & L.nominalInterest .~ ipnr'
          & L.statusDate .~ t
_STF_RR_PAM
  fs@FeeSchedule {..}
  RiskFactors
    { o_rf_RRMO
    }
  ct@ContractTerms
    { feeRate = Just fer,
      lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp,
      dayCountConvention = Just dcc,
      contractRole,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        timeFromLatestFeePayment = yearFraction dcc latestFeePayment t maturityDate
        timeFromLatestToNextFeePayment = yearFraction dcc latestFeePayment nextFeePayment maturityDate
        delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - (s ^. L.nominalInterest)) rrpf) rrpc
        ipnr' = _min (_max (s ^. L.nominalInterest + delta_r) rrlf) rrlc
     in _STF_PY_PAM fs ct s t
          & L.accruedInterest .~ (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
          & L.accruedFees .~ (if latestFeePayment == nextFeePayment then 0 else (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * fer)
          & L.nominalInterest .~ ipnr'
          & L.statusDate .~ t
_STF_RR_PAM _ _ _ s _ = s

_STF_RR_LAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RR_LAM
  fs
  RiskFactors
    { o_rf_RRMO
    }
  ct@ContractTerms
    { lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp
    }
  s
  t =
    let delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - (s ^. L.nominalInterest)) rrpf) rrpc
        ipnr' = _min (_max ((s ^. L.nominalInterest) + delta_r) rrlf) rrlc
     in _STF_PY_LAM fs ct s t
          & L.nominalInterest .~ ipnr'
          & L.statusDate .~ t
_STF_RR_LAM _ _ _ s _ = s

_STF_RR_ANN :: ActusFrac a => PrincipalRedemptionSchedule -> FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RR_ANN
  PrincipalRedemptionSchedule {..}
  FeeSchedule {..}
  RiskFactors
    { o_rf_RRMO
    }
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp,
      feeRate,
      contractRole,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        timeFromLatestFeePayment = yearFraction dcc latestFeePayment t maturityDate
        timeFromLatestToNextFeePayment = yearFraction dcc latestFeePayment nextFeePayment maturityDate
        ti = zipWith (\tn tm -> yearFraction dcc tn tm maturityDate) laterPrincipalRedemptionDates (tail laterPrincipalRedemptionDates)

        ipac' = (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
        feac' = feeAccrued' ct
          where
            feeAccrued' ContractTerms {feeBasis = Just FEB_N} = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 feeRate
            feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * thisOr0 feeRate

        ipnr' = _min (_max ((s ^. L.nominalInterest) + delta_r) rrlf) rrlc
          where
            delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - (s ^. L.nominalInterest)) rrpf) rrpc

        prnxt' = annuity ipnr' ti
     in s & L.accruedInterest .~ ipac'
          & L.accruedFees .~ feac'
          & L.nominalInterest .~ ipnr'
          & L.nextPrincipalRedemptionPayment .~ prnxt'
          & L.statusDate .~ t
_STF_RR_ANN _ _ _ _ s _ = s

_STF_RR_SWPPV :: ActusFrac a => RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RR_SWPPV
  RiskFactors
    { o_rf_RRMO
    }
  ContractTerms
    { dayCountConvention = Just dcc,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp,
      nominalInterestRate = Just ipnr,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
     in s & L.accruedInterest .~ timeFromLastEvent * (s ^. L.notionalPrincipal) * (ipnr - (s ^. L.nominalInterest))
          & L.accruedInterestFirstLeg ?~ timeFromLastEvent * (s ^. L.notionalPrincipal) * ipnr
          & L.accruedInterestSecondLeg ?~ timeFromLastEvent * (s ^. L.notionalPrincipal) * (s ^. L.nominalInterest)
          & L.nominalInterest .~ rrmlt * o_rf_RRMO + rrsp
          & L.statusDate .~ t
_STF_RR_SWPPV _ _ s _ = s

-----------------------------
-- Rate Reset Fixing (RRF) --
-----------------------------

_STF_RRF_PAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RRF_PAM
  fs
  _
  ct@ContractTerms
    { nextResetRate = rrnxt
    }
  s
  t =
    _STF_PY_PAM fs ct s t
      & L.nominalInterest .~ thisOr0 rrnxt

_STF_RRF_LAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RRF_LAM
  fs
  _
  ct@ContractTerms
    { nextResetRate = rrnxt
    }
  s
  t =
    _STF_PY_LAM fs ct s t
      & L.nominalInterest .~ thisOr0 rrnxt

_STF_RRF_ANN :: ActusFrac a => PrincipalRedemptionSchedule -> FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_RRF_ANN
  PrincipalRedemptionSchedule {..}
  FeeSchedule {..}
  _
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      nextResetRate = Just rrnxt,
      contractRole,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        timeFromLatestFeePayment = yearFraction dcc latestFeePayment t maturityDate
        timeFromLatestToNextFeePayment = yearFraction dcc latestFeePayment nextFeePayment maturityDate
        ti = zipWith (\tn tm -> yearFraction dcc tn tm maturityDate) laterPrincipalRedemptionDates (tail laterPrincipalRedemptionDates)

        ipac' = (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
        feac' = feeAccrued' ct
          where
            feeAccrued' ContractTerms {feeBasis = Just FEB_N} = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (feeRate ct)
            feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * thisOr0 (feeRate ct)

        ipnr' = rrnxt
        prnxt' = annuity ipnr' ti
     in s & L.accruedInterest .~ ipac'
          & L.accruedFees .~ feac'
          & L.nominalInterest .~ ipnr'
          & L.nextPrincipalRedemptionPayment .~ prnxt'
          & L.statusDate .~ t
_STF_RRF_ANN _ _ _ _ s _ = s

-------------------------------------------
-- Principal Payment Amount Fixing (PRF) --
-------------------------------------------

_STF_PRF_ANN :: ActusFrac a => PrincipalRedemptionSchedule -> FeeSchedule -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_PRF_ANN
  PrincipalRedemptionSchedule {..}
  FeeSchedule {..}
  ct@ContractTerms
    { dayCountConvention = Just dcc,
      contractRole,
      maturityDate
    }
  s
  t =
    let timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        timeFromLatestFeePayment = yearFraction dcc latestFeePayment t maturityDate
        timeFromLatestToNextFeePayment = yearFraction dcc latestFeePayment nextFeePayment maturityDate
        timeToNextPrincipalRedemption = yearFraction dcc t nextPrincipalRedemption maturityDate
        ti = zipWith (\tn tm -> yearFraction dcc tn tm maturityDate) laterPrincipalRedemptionDates (tail laterPrincipalRedemptionDates)

        ipac' = (s ^. L.accruedInterest) + timeFromLastEvent * (s ^. L.nominalInterest) * (s ^. L.interestCalculationBase)
        feac' = feeAccrued' ct
          where
            feeAccrued' ContractTerms {feeBasis = Just FEB_N} = (s ^. L.accruedFees) + timeFromLastEvent * (s ^. L.notionalPrincipal) * thisOr0 (feeRate ct)
            feeAccrued' _ = (timeFromLatestFeePayment / timeFromLatestToNextFeePayment) * sign contractRole * thisOr0 (feeRate ct)

        prnxt' = sign contractRole * frac * scale
          where
            scale = (s ^. L.notionalPrincipal) + ipac' + timeToNextPrincipalRedemption * (s ^. L.nominalInterest) * (s ^. L.notionalPrincipal)
            frac = annuity (s ^. L.nominalInterest) ti
     in s & L.accruedInterest .~ ipac'
          & L.accruedFees .~ feac'
          & L.nextPrincipalRedemptionPayment .~ prnxt'
          & L.statusDate .~ t
_STF_PRF_ANN _ _ _ s _ = s

-------------------------------
-- Scaling Index Fixing (SC) --
-------------------------------

_STF_SC_PAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_SC_PAM
  fs
  RiskFactors
    { o_rf_SCMO
    }
  ct@ContractTerms
    { scalingEffect = Just scef,
      scalingIndexAtStatusDate = Just scied
    }
  s
  t =
    let nsc' = case scef of
          SE_OOM -> s ^. L.notionalScalingMultiplier
          SE_IOO -> s ^. L.notionalScalingMultiplier
          _      -> (o_rf_SCMO - scied) / scied
        isc' = case scef of
          SE_ONO -> s ^. L.interestScalingMultiplier
          SE_OOM -> s ^. L.interestScalingMultiplier
          SE_ONM -> s ^. L.interestScalingMultiplier
          _      -> (o_rf_SCMO - scied) / scied
     in _STF_PY_PAM fs ct s t
          & L.notionalScalingMultiplier .~ nsc'
          & L.interestScalingMultiplier .~ isc'
_STF_SC_PAM _ _ _ s _ = s

_STF_SC_LAM :: ActusFrac a => FeeSchedule -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_SC_LAM
  fs
  RiskFactors
    { o_rf_SCMO
    }
  ct@ContractTerms
    { scalingIndexAtContractDealDate = Just sccdd,
      scalingEffect = Just scef
    }
  s
  t =
    _STF_PY_LAM fs ct s t
      & L.notionalScalingMultiplier .~ (if elem 'N' (show scef) then o_rf_SCMO / sccdd else s ^. L.notionalScalingMultiplier)
      & L.interestScalingMultiplier .~ (if elem 'I' (show scef) then o_rf_SCMO / sccdd else s ^. L.interestScalingMultiplier)
_STF_SC_LAM _ _ _ s _ = s

-------------------
-- Exercise (XD) --
-------------------

_STF_XD_OPTNS :: ActusFrac a => RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_XD_OPTNS
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { contractType = OPTNS,
      optionType = Just OPTP_C,
      optionStrike1 = Just ops1
    }
  s
  t =
    s & L.exerciseAmount ?~ _max (xd_payoff - ops1) 0
      & L.statusDate .~ t
_STF_XD_OPTNS
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { contractType = OPTNS,
      optionType = Just OPTP_P,
      optionStrike1 = Just ops1
    }
  s
  t =
    s & L.exerciseAmount ?~ _max (ops1 - xd_payoff) 0
      & L.statusDate .~ t
_STF_XD_OPTNS
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { contractType = OPTNS,
      optionType = Just OPTP_CP,
      optionStrike1 = Just ops1
    }
  s
  t =
    s & L.exerciseAmount ?~ _max (xd_payoff - ops1) 0 + _max (ops1 - xd_payoff) 0
      & L.statusDate .~ t
_STF_XD_OPTNS _ _ s _ = s

_STF_XD_FUTUR :: ActusFrac a => RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_XD_FUTUR
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { contractType = FUTUR,
      futuresPrice = Just pfut
    }
  s
  t =
    s & L.exerciseAmount ?~ xd_payoff - pfut
      & L.statusDate .~ t
_STF_XD_FUTUR _ _ s _ = s

_STF_XD_CEG :: ActusFrac a => [[((String, EventType, ShiftedDay), ContractState a, a)]] -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_XD_CEG
  referenceStates
  ContractTerms
    { coverageOfCreditEnhancement = Just cecv,
      guaranteedExposure = Just CEGE_NO,
      feeRate = Just fer,
      feeBasis = Just FEB_A,
      dayCountConvention = Just dcc,
      cycleOfFee = Just cef,
      maturityDate,
      contractRole
    }
  s
  t =
    let nt' = cecv * sign contractRole * (sum $ map f referenceStates)
        f cs =
          let (_, c, _) = last $ takeWhile (\((_, _, d), _, _) -> calculationDay d <= t) cs
           in nt c
        timeFromLastEvent = yearFraction dcc (s ^. L.statusDate) t maturityDate
        timeFullFeeCycle = yearFraction dcc (s ^. L.statusDate) ((s ^. L.statusDate) <+> cef) maturityDate
     in s & L.exerciseAmount ?~ nt'
          & L.notionalPrincipal .~ nt'
          & L.accruedFees +~ timeFromLastEvent / timeFullFeeCycle * fer
          & L.statusDate .~ t
_STF_XD_CEG
  referenceStates
  ContractTerms
    { coverageOfCreditEnhancement = Just cecv,
      guaranteedExposure = Just CEGE_NO,
      contractRole
    }
  s
  t =
    let nt' = cecv * sign contractRole * (sum $ map f referenceStates)
        f cs =
          let (_, c, _) = last $ takeWhile (\((_, _, d), _, _) -> calculationDay d <= t) cs
           in nt c
     in s & L.exerciseAmount ?~ nt'
          & L.notionalPrincipal .~ nt'
          & L.statusDate .~ t
_STF_XD_CEG
  referenceStates
  ContractTerms
    { coverageOfCreditEnhancement = Just cecv,
      guaranteedExposure = Just CEGE_NI,
      contractRole
    }
  s
  t =
    let nt' = cecv * sign contractRole * (sum $ map f referenceStates)
        f cs =
          let (_, c, _) = last $ takeWhile (\(_, x, _) -> sd x <= t) cs
           in nt c + ipac c
     in s & L.exerciseAmount ?~ nt'
          & L.notionalPrincipal .~ nt'
          & L.statusDate .~ t
_STF_XD_CEG
  _
  _
  s
  t =
    s & L.exerciseAmount ?~ (s ^. L.notionalPrincipal)
      & L.statusDate .~ t

_STF_XD_CEC :: ActusFrac a => [[((String, EventType, ShiftedDay), ContractState a, a)]] -> RiskFactors a -> ContractTerms a -> ContractState a -> LocalTime -> ContractState a
_STF_XD_CEC
  referenceStates
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { coverageOfCreditEnhancement = Just cecv,
      guaranteedExposure = Just CEGE_NO,
      contractRole
    }
  s
  t =
    let nt' = cecv * sign contractRole * (sum $ map f referenceStates)
        f cs =
          let (_, c, _) = last $ takeWhile (\((_, _, d), _, _) -> calculationDay d <= t) cs
           in nt c
     in s & L.exerciseAmount ?~ _min xd_payoff nt'
          & L.notionalPrincipal .~ nt'
          & L.statusDate .~ t
_STF_XD_CEC
  referenceStates
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { coverageOfCreditEnhancement = Just cecv,
      guaranteedExposure = Just CEGE_NI,
      contractRole
    }
  s
  t =
    let nt' = cecv * sign contractRole * (sum $ map f referenceStates)
        f cs =
          let (_, c, _) = last $ takeWhile (\(_, x, _) -> sd x <= t) cs
           in nt c + ipac c
     in s & L.exerciseAmount ?~ _min xd_payoff nt'
          & L.notionalPrincipal .~ nt'
          & L.statusDate .~ t
_STF_XD_CEC
  referenceStates
  RiskFactors
    { xd_payoff
    }
  ContractTerms
    { coverageOfCreditEnhancement,
       contractRole
    }
  s
  t =
    let cecv = fromMaybe 1 coverageOfCreditEnhancement
        nt' = cecv * sign contractRole * (sum $ map f referenceStates)
        f cs =
          let (_, c, _) = last $ takeWhile (\((_, _, d) , _, _) -> calculationDay d <= t) cs
           in nt c
     in s & L.exerciseAmount ?~ _min xd_payoff nt'
          & L.statusDate .~ t
