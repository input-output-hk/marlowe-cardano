{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

{-| = ACTUS state transformation functions -}
module Language.Marlowe.ACTUS.Model.StateTransition
  ( stateTransition
  , CtxSTF (..)
  )
where

import Control.Monad.Reader (Reader, reader)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Time.LocalTime (LocalTime)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTermsPoly (..), FEB (..), IPCB (..), OPTP (..),
                                                    SCEF (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusNum (..), ActusOps (..), RoleSignOps (..), YearFractionOps (_y))
import Language.Marlowe.ACTUS.Utility.ANN.Annuity (annuity)
import Language.Marlowe.ACTUS.Utility.ScheduleGenerator (inf, sup)
import Prelude hiding (Fractional, Num, (*), (+), (-), (/))

-- |The context for state transitions provides the contract terms in addition with
-- schedules and the maturity of the contract. Furthermore a function to retrieve
-- risk factors is available.
data CtxSTF a = CtxSTF
  { contractTerms :: ContractTermsPoly a                         -- ^ Contract terms
  , fpSchedule    :: [LocalTime]                                 -- ^ Fee payment schedule
  , prSchedule    :: [LocalTime]                                 -- ^ Principal redemption schedule
  , ipSchedule    :: [LocalTime]                                 -- ^ Interest payment schedule
  , maturity      :: Maybe LocalTime                             -- ^ Maturity
  , riskFactors   :: EventType -> LocalTime -> RiskFactorsPoly a -- ^ Riskfactors per event and time
  }

-- |A state transition updates the contract state based on the type of event and the time.
-- `CtxSTF` provides in particular the contract terms and risk factors.
stateTransition :: (RoleSignOps a, YearFractionOps a) =>
     EventType                               -- ^ Event type
  -> LocalTime                               -- ^ Time
  -> ContractStatePoly a                     -- ^ Contract state
  -> Reader (CtxSTF a) (ContractStatePoly a) -- ^ Updated contract state
stateTransition ev t sn = reader stateTransition'
  where
    stateTransition' CtxSTF {..} = stf ev (riskFactors ev t) contractTerms sn
      where
        ---------------------
        -- Monitoring (AD) --
        ---------------------
        -- STF_AD_CSH
        stf
          AD
          _
          ContractTermsPoly
            { contractType = CSH
            }
          st@ContractStatePoly
            {
            } =
            st
              { sd = t
              }
        -- STF_AD_*
        stf
          AD
          _
          ContractTermsPoly
            { dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { nt,
              ipac,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in st
                  { ipac = ipac + y_sd_t * ipnr * nt,
                    sd = t
                  }
        ----------------------------
        -- Initial Exchange (IED) --
        ----------------------------
        -- STF_IED_PAM
        stf
          IED
          _
          ContractTermsPoly
            { contractType = PAM,
              nominalInterestRate,
              notionalPrincipal = Just nt,
              accruedInterest = Just ipac,
              contractRole
            }
          st@ContractStatePoly
            {
            } =
            st
              { nt = _r contractRole * nt,
                ipnr = fromMaybe _zero nominalInterestRate,
                ipac = ipac,
                sd = t
              }
        stf
          IED
          _
          ContractTermsPoly
            { contractType = PAM,
              nominalInterestRate,
              notionalPrincipal = Just nt,
              cycleAnchorDateOfInterestPayment = Just ipanx,
              dayCountConvention = Just dcc,
              contractRole,
              maturityDate
            }
          st@ContractStatePoly
            {
            } =
            let nt' = _r contractRole * nt
                y_ipanx_t = _y dcc ipanx t maturityDate
                ipnr' = fromMaybe _zero nominalInterestRate
             in st
                  { nt = nt',
                    ipnr = ipnr',
                    ipac =
                      let _Y = _y dcc ipanx t Nothing
                       in _Y * y_ipanx_t * nt' * ipnr',
                    sd = t
                  }
        -- STF_IED_SWPPV
        stf
          IED
          _
          ContractTermsPoly
            { contractType = SWPPV,
              notionalPrincipal = Just nt,
              nominalInterestRate2 = Just ipnr2,
              contractRole
            }
          st =
            st
              { nt = _r contractRole * nt,
                ipnr = ipnr2,
                ipac = _zero,
                ipac1 = Just _zero,
                ipac2 = Just _zero,
                sd = t
              }
        -- STF_IED_LAM
        -- STF_IED_NAM
        -- STF_IED_ANN
        stf
          IED
          _
          ct@ContractTermsPoly
            { contractType,
              notionalPrincipal = Just nt,
              nominalInterestRate = Just ipnr,
              dayCountConvention = Just dcc,
              cycleAnchorDateOfInterestPayment = Just ipanx,
              maturityDate,
              contractRole
            }
          st@ContractStatePoly
            {
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let y_ipanx_t = _y dcc ipanx t maturityDate
                  nt' = _r contractRole * nt
                  ipcb' = interestCalculationBase' ct
                    where
                      interestCalculationBase' ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
                      interestCalculationBase' ContractTermsPoly {interestCalculationBaseA = Just ipcba} = _r contractRole * ipcba
                      interestCalculationBase' _ = _zero
                  ipac' = interestAccrued' ct
                    where
                      interestAccrued' ContractTermsPoly {accruedInterest = Just ipac} = _r contractRole * ipac
                      interestAccrued' ContractTermsPoly {cycleAnchorDateOfInterestPayment = Just ipanx'} | ipanx' < t = y_ipanx_t * nt' * ipcb'
                      interestAccrued' _ = _zero
               in st
                    { nt = nt',
                      ipnr = ipnr,
                      ipac = ipac',
                      ipcb = ipcb',
                      sd = t
                    }
        -------------------------------
        -- Principal Redemption (PR) --
        -------------------------------
        -- STF_PR_LAM
        stf
          PR
          _
          ct@ContractTermsPoly
            { contractType = LAM,
              dayCountConvention = Just dcc,
              feeRate,
              contractRole,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                nt' = nt - _r contractRole * (prnxt - _r contractRole * _max _zero (_abs prnxt - _abs nt))
                ipcb' = interestCalculationBase' ct
                  where
                    interestCalculationBase' ContractTermsPoly {interestCalculationBase = Just IPCB_NTL} = ipcb
                    interestCalculationBase' _                                                           = nt'
             in st
                  { nt = nt',
                    feac = maybe feac (\fer -> feac + y_sd_t * nt * fer) feeRate,
                    ipcb = ipcb',
                    ipac = ipac + ipnr * ipcb * y_sd_t,
                    sd = t
                  }
        -- STF_PR_NAM
        -- STF_PR_ANN
        stf
          PR
          _
          ct@ContractTermsPoly
            { contractType,
              dayCountConvention = Just dcc,
              feeRate,
              contractRole,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate
                  ipac' = ipac + ipnr * ipcb * y_sd_t
                  nt' = nt - _r contractRole * r
                    where
                      ra = prnxt - _r contractRole * ipac'
                      r = ra - _max _zero (ra - _abs nt)
                  ipcb' = interestCalculationBase' ct
                    where
                      interestCalculationBase' ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
                      interestCalculationBase' _                                                          = ipcb
               in st
                    { nt = nt',
                      feac = maybe feac (\fer -> feac + y_sd_t * nt * fer) feeRate,
                      ipcb = ipcb',
                      ipac = ipac',
                      sd = t
                    }
        -------------------
        -- Maturity (MD) --
        -------------------
        stf MD _ _ st =
          st
            { nt = _zero,
              ipac = _zero,
              feac = _zero,
              sd = t
            }
        -------------------------------
        -- Principal Prepayment (PP) --
        -------------------------------
        -- STF_PP_PAM
        stf
          PP
          rf@RiskFactorsPoly
            { pp_payoff
            }
          ct@ContractTermsPoly
            { contractType = PAM
            }
          st@ContractStatePoly
            { ..
            } =
            let st' = stf PY rf ct st
             in st'
                  { nt = nt - pp_payoff
                  }
        -- STF_PP_LAM
        -- STF_PP_NAM
        -- STF_PP_ANN
        stf
          PP
          rf@RiskFactorsPoly
            { pp_payoff
            }
          ct@ContractTermsPoly
            { contractType
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let st' = stf PY rf ct st
                  nt' = nt - pp_payoff
                  ipcb' = interestCalculationBase ct
                    where
                      interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
                      interestCalculationBase _                                                          = ipcb
               in st'
                    { nt = nt',
                      ipcb = ipcb'
                    }
        --------------------------
        -- Penalty Payment (PY) --
        --------------------------
        -- STF_PY_PAM
        stf
          PY
          _
          ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              notionalPrincipal = Just nt',
              maturityDate,
              feeBasis = Just FEB_N,
              feeRate = Just fer
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in st
                  { ipac = ipac + y_sd_t * ipnr * nt,
                    feac = feac + y_sd_t * nt' * fer,
                    sd = t
                  }
        stf
          PY
          _
          ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              maturityDate,
              contractRole,
              feeRate = Just fer
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                y_tfpminus_t = _y dcc tfp_minus t maturityDate
                y_tfpminus_tfpplus = _y dcc tfp_minus tfp_plus maturityDate
             in st
                  { ipac = ipac + y_sd_t * ipnr * nt,
                    feac = _max _zero (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fer,
                    sd = t
                  }
        -- STF_PY_LAM
        -- STF_PY_NAM
        -- STF_PY_ANN
        stf
          PY
          _
          ct@ContractTermsPoly
            { contractType,
              feeRate = Just fer,
              dayCountConvention = Just dcc,
              maturityDate,
              contractRole
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate
                  y_tfpminus_t = _y dcc tfp_minus t maturityDate
                  y_tfpminus_tfpplus = _y dcc tfp_minus tfp_plus maturityDate

                  ipac' = ipac + y_sd_t * ipnr * ipcb
                  feac' = feeAccrued' ct
                    where
                      feeAccrued' ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fer
                      feeAccrued' _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fer
               in st
                    { ipac = ipac',
                      feac = feac',
                      sd = t
                    }
        stf
          PY
          _
          ct@ContractTermsPoly
            { contractType,
              dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate

                  ipac' = ipac + y_sd_t * ipnr * ipcb
                  feac' = feeAccrued' ct
                    where
                      feeAccrued' ContractTermsPoly {feeBasis = Just FEB_N} = feac
                      feeAccrued' _                                         = _zero
               in st
                    { ipac = ipac',
                      feac = feac',
                      sd = t
                    }
        ----------------------
        -- Fee Payment (FP) --
        ----------------------
        -- STF_FP_PAM
        stf
          FP
          _
          ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in st
                  { ipac = ipac + y_sd_t * ipnr * nt,
                    feac = _zero,
                    sd = t
                  }
        -- STF_FP_LAM
        -- STF_FP_NAM
        -- STF_FP_ANN
        stf
          FP
          _
          ContractTermsPoly
            { contractType,
              dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate
               in st
                    { ipac = ipac + y_sd_t * ipnr * ipcb,
                      feac = _zero,
                      sd = t
                    }
        --------------------
        -- Purchase (PRD) --
        --------------------
        -- STF_PRD_PAM
        -- STF_PRD_LAM
        -- STF_PRD_NAM
        -- STF_PRD_ANN
        stf
          PRD
          rf
          ct@ContractTermsPoly
            { contractType
            }
          st
            | contractType `elem` [PAM, LAM, NAM, ANN] =
              stf PY rf ct st
        -- STF_PRD_CEG
        stf
          PRD
          _
          ContractTermsPoly
            { contractType = CEG,
              feeRate = Just fer
            }
          st = st
                 { feac = fer,
                   sd = t
                 }
        stf
          PRD
          _
          ContractTermsPoly
            { contractType = CEG,
              feeAccrued = Just feac
            }
          st = st
                 { feac = feac,
                   sd = t
                 }
        ----------------------
        -- Termination (TD) --
        ----------------------
        stf TD _ _ st =
          st
            { nt = _zero,
              ipac = _zero,
              feac = _zero,
              ipnr = _zero,
              sd = t
            }
        ---------------------------
        -- Interest Payment (IP) --
        ---------------------------
        -- STF_IP_SWPPV
        stf
          IP
          _
          ContractTermsPoly
            { contractType
            }
          st@ContractStatePoly
            {
            } | contractType `elem` [SWPPV, CLM] =
            st
              { ipac = _zero,
                sd = t
              }
        -- STF_IP_*
        stf
          IP
          _
          ContractTermsPoly
            { dayCountConvention = Just dcc,
              feeRate,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in st
                  { ipac = _zero,
                    feac = maybe _zero (\fer -> y_sd_t * nt * fer) feeRate,
                    sd = t
                  }
        ---------------------------------------
        -- Interest Payment Fixed Leg (IPFX) --
        ---------------------------------------
        -- STF_IPFX_SWPPV
        stf
          IPFX
          _
          ContractTermsPoly
            { contractType = SWPPV,
              dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { sd
            } =
            st
              { ipla = Just $ _y dcc sd t maturityDate,
                ipac1 = Just _zero,
                sd = t
              }
        ------------------------------------------
        -- Interest Payment Floating Leg (IPFL) --
        ------------------------------------------
        -- STF_IPFL_SWPPV
        stf
          IPFL
          _
          ContractTermsPoly
            { contractType = SWPPV
            }
          st@ContractStatePoly
            {
            } =
            st
              { ipac2 = Just _zero,
                sd = t
              }
        ------------------------------------
        -- Interest Capitalization (IPCI) --
        ------------------------------------
        -- STF_IPCI_LAM
        stf
          IPCI
          rf
          ct@ContractTermsPoly
            { contractType = PAM,
              dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                st' = stf IP rf ct st
             in st'
                  { nt = nt + ipac + y_sd_t * nt * ipnr
                  }
        -- STF_IPCI_LAM
        -- STF_IPCI_NAM
        -- STF_IPCI_ANN
        stf
          IPCI
          rf
          ct@ContractTermsPoly
            { contractType,
              dayCountConvention = Just dcc,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let y_sd_t = _y dcc sd t maturityDate
                  st' = stf IP rf ct st
                  nt' = nt + ipac + y_sd_t * ipnr * ipcb
                  ipcb' = interestCalculationBase ct
                    where
                      interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
                      interestCalculationBase _                                                          = ipcb
               in st'
                    { nt = nt',
                      ipcb = ipcb'
                    }
        ---------------------------------------------
        -- Interest Calculation Base Fixing (IPCB) --
        ---------------------------------------------
        stf
          IPCB
          rf
          ct@ContractTermsPoly
            { contractType
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let st' = stf PRD rf ct st
               in st'
                    { ipcb = nt
                    }
        -------------------------------
        -- Rate Reset (RR) --
        -------------------------------
        -- STF_RR_PAM
        stf
          RR
          rf@RiskFactorsPoly
            { o_rf_RRMO
            }
          ct@ContractTermsPoly
            { contractType = PAM,
              feeBasis = Just FEB_N,
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
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                st' = stf PRD rf ct st
                delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc
                ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
             in st'
                  { ipac = ipac + y_sd_t * ipnr * nt,
                    feac = feac + y_sd_t * nt * fer,
                    ipnr = ipnr',
                    sd = t
                  }
        stf
          RR
          rf@RiskFactorsPoly
            { o_rf_RRMO
            }
          ct@ContractTermsPoly
            { contractType = PAM,
              feeRate = Just fer,
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
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                y_tfpminus_t = _y dcc tfp_minus t maturityDate
                y_tfpminus_tfpplus = _y dcc tfp_minus tfp_plus maturityDate
                st' = stf PRD rf ct st
                delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc
                ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
             in st'
                  { ipac = ipac + y_sd_t * ipnr * nt,
                    feac = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fer,
                    ipnr = ipnr',
                    sd = t
                  }
        -- STF_RR_LAM
        -- STF_RR_NAM
        stf
          RR
          rf@RiskFactorsPoly
            { o_rf_RRMO
            }
          ct@ContractTermsPoly
            { contractType,
              lifeFloor = Just rrlf,
              lifeCap = Just rrlc,
              periodCap = Just rrpc,
              periodFloor = Just rrpf,
              rateMultiplier = Just rrmlt,
              rateSpread = Just rrsp
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM] =
              let st' = stf PRD rf ct st
                  delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc
                  ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
               in st'
                    { ipnr = ipnr'
                    }
        -- STF_RR_ANN
        stf
          RR
          RiskFactorsPoly
            { o_rf_RRMO
            }
          ct@ContractTermsPoly
            { contractType = ANN,
              dayCountConvention = Just dcc,
              lifeFloor = Just rrlf,
              lifeCap = Just rrlc,
              periodCap = Just rrpc,
              periodFloor = Just rrpf,
              rateMultiplier = Just rrmlt,
              rateSpread = Just rrsp,
              contractRole,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                y_tfpminus_t = _y dcc tfp_minus t maturityDate
                y_tfpminus_tfpplus = _y dcc tfp_minus tfp_plus maturityDate
                ti = zipWith (\tn tm -> _y dcc tn tm maturityDate) prDatesAfterSd (tail prDatesAfterSd)

                ipac' = ipac + y_sd_t * ipnr * ipcb
                feac' = feeAccrued' ct
                  where
                    feeAccrued' ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fromMaybe _zero (feeRate ct)
                    feeAccrued' _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fromMaybe _zero (feeRate ct)

                ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
                  where
                    delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc

                prnxt' = annuity ipnr' ti
             in st
                  { ipac = ipac',
                    feac = feac',
                    ipnr = ipnr',
                    prnxt = prnxt',
                    sd = t
                  }
        -- STF_RR_SWPPV
        stf
          RR
          RiskFactorsPoly
            { o_rf_RRMO
            }
          ContractTermsPoly
            { contractType = SWPPV,
              dayCountConvention = Just dcc,
              rateMultiplier = Just rrmlt,
              rateSpread = Just rrsp,
              nominalInterestRate = Just ipnr',
              maturityDate
            }
          st@ContractStatePoly
            { nt,
              ipnr,
              sd
            } =
            let y_sd_t = _y dcc sd t maturityDate
             in st
                  { ipac = y_sd_t * nt * (ipnr' - ipnr),
                    ipac1 = Just $ y_sd_t * nt * ipnr',
                    ipac2 = Just $ y_sd_t * nt * ipnr,
                    ipnr = rrmlt * o_rf_RRMO + rrsp,
                    sd = t
                  }
        -----------------------------
        -- Rate Reset Fixing (RRF) --
        -----------------------------
        -- STF_RRF_PAM
        stf
          RRF
          rf
          ct@ContractTermsPoly
            { contractType = PAM,
              nextResetRate = rrnxt
            }
          st =
            let st' = stf PRD rf ct st
             in st'
                  { ipnr = fromMaybe _zero rrnxt
                  }
        -- STF_RRF_LAM
        -- STF_RRF_NAM
        stf
          RRF
          rf
          ct@ContractTermsPoly
            { contractType,
              nextResetRate = rrnxt
            }
          st
            | contractType `elem` [LAM, NAM] =
              let st' = stf PRD rf ct st
               in st'
                    { ipnr = fromMaybe _zero rrnxt
                    }
        -- STF_RRF_ANN
        stf
          RRF
          _
          ct@ContractTermsPoly
            { contractType = ANN,
              dayCountConvention = Just dcc,
              nextResetRate = Just rrnxt,
              contractRole,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                y_tfpminus_t = _y dcc tfp_minus t maturityDate
                y_tfpminus_tfpplus = _y dcc tfp_minus tfp_plus maturityDate
                ti = zipWith (\tn tm -> _y dcc tn tm maturityDate) prDatesAfterSd (tail prDatesAfterSd)

                ipac' = ipac + y_sd_t * ipnr * ipcb
                feac' = feeAccrued' ct
                  where
                    feeAccrued' ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fromMaybe _zero (feeRate ct)
                    feeAccrued' _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fromMaybe _zero (feeRate ct)

                ipnr' = rrnxt
                prnxt' = annuity ipnr' ti
             in st
                  { ipac = ipac',
                    feac = feac',
                    ipnr = ipnr',
                    prnxt = prnxt',
                    sd = t
                  }
        -------------------------------------------
        -- Principal Payment Amount Fixing (PRF) --
        -------------------------------------------
        stf
          PRF
          _
          ct@ContractTermsPoly
            { contractType = ANN,
              dayCountConvention = Just dcc,
              contractRole,
              maturityDate
            }
          st@ContractStatePoly
            { ..
            } =
            let y_sd_t = _y dcc sd t maturityDate
                y_tfpminus_t = _y dcc tfp_minus t maturityDate
                y_tfpminus_tfpplus = _y dcc tfp_minus tfp_plus maturityDate
                y_t = _y dcc t tpr_plus maturityDate
                ti = zipWith (\tn tm -> _y dcc tn tm maturityDate) prDatesAfterSd (tail prDatesAfterSd)

                ipac' = ipac + y_sd_t * ipnr * ipcb
                feac' = feeAccrued' ct
                  where
                    feeAccrued' ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fromMaybe _zero (feeRate ct)
                    feeAccrued' _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fromMaybe _zero (feeRate ct)

                prnxt' = _r contractRole * frac * scale
                  where
                    scale = nt + ipac' + y_t * ipnr * nt
                    frac = annuity ipnr ti
             in st
                  { ipac = ipac',
                    feac = feac',
                    prnxt = prnxt',
                    sd = t
                  }
        -------------------------------
        -- Scaling Index Fixing (SC) --
        -------------------------------
        -- STF_SC_PAM
        stf
          SC
          rf@RiskFactorsPoly
            { o_rf_SCMO
            }
          ct@ContractTermsPoly
            { contractType = PAM,
              scalingEffect = Just scef,
              scalingIndexAtStatusDate = Just scied
            }
          st@ContractStatePoly
            { ..
            } =
            let st' = stf PY rf ct st
                nsc' = case scef of
                  SE_OOM -> nsc
                  SE_IOO -> nsc
                  _      -> (o_rf_SCMO - scied) / scied
                isc' = case scef of
                  SE_ONO -> isc
                  SE_OOM -> isc
                  SE_ONM -> isc
                  _      -> (o_rf_SCMO - scied) / scied
             in st'
                  { nsc = nsc',
                    isc = isc'
                  }
        -- STF_SC_LAM
        -- STF_SC_NAM
        -- STF_SC_ANN
        stf
          SC
          rf@RiskFactorsPoly
            { o_rf_SCMO
            }
          ct@ContractTermsPoly
            { contractType,
              scalingIndexAtContractDealDate = Just sccdd,
              scalingEffect = Just scef
            }
          st@ContractStatePoly
            { ..
            }
            | contractType `elem` [LAM, NAM, ANN] =
              let st' = stf PY rf ct st
               in st'
                    { nsc = if elem 'N' (show scef) then o_rf_SCMO / sccdd else nsc,
                      isc = if elem 'I' (show scef) then o_rf_SCMO / sccdd else nsc
                    }
        -------------------
        -- Exercise (XD) --
        -------------------
        -- STF_XD_OPTNS
        stf
          XD
          RiskFactorsPoly
            { xd_payoff
            }
          ContractTermsPoly
            { contractType = OPTNS,
              optionType = Just OPTP_C,
              optionStrike1 = Just ops1
            }
          st@ContractStatePoly
            {
            } =
            st
              { xa = Just $ _max (xd_payoff - ops1) _zero,
                sd = t
              }
        stf
          XD
          RiskFactorsPoly
            { xd_payoff
            }
          ContractTermsPoly
            { contractType = OPTNS,
              optionType = Just OPTP_P,
              optionStrike1 = Just ops1
            }
          st@ContractStatePoly
            {
            } =
            st
              { xa = Just $ _max (ops1 - xd_payoff) _zero,
                sd = t
              }
        stf
          XD
          RiskFactorsPoly
            { xd_payoff
            }
          ContractTermsPoly
            { contractType = OPTNS,
              optionType = Just OPTP_CP,
              optionStrike1 = Just ops1
            }
          st@ContractStatePoly
            {
            } =
            st
              { xa = Just $ _max (xd_payoff - ops1) _zero + _max (ops1 - xd_payoff) _zero,
                sd = t
              }
        -- STF_XD_FUTUR
        stf
          XD
          RiskFactorsPoly
            { xd_payoff
            }
          ContractTermsPoly
            { contractType = FUTUR,
              futuresPrice = Just pfut
            }
          st@ContractStatePoly
            {
            } =
            st
              { xa = Just $ xd_payoff - pfut,
                sd = t
              }
        -- STF_XD_CEG
        stf
          XD
          RiskFactorsPoly
            {
            }
          ContractTermsPoly
            { contractType = CEG
            }
          st@ContractStatePoly
            { nt
            } =
              st
                { xa = Just nt,
                  sd = t
                }
        -----------------------
        -- Credit Event (CE) --
        -----------------------
        stf CE rf ct st = stf AD rf ct st
        -------------
        -- Default --
        -------------
        stf _ _ _ _ = sn

        tfp_minus = fromMaybe t (sup fpSchedule t)
        tfp_plus = fromMaybe t (inf fpSchedule t)
        tpr_plus = fromMaybe t (inf prSchedule t)

        prDates = prSchedule ++ maybeToList maturity
        prDatesAfterSd = filter (> sd sn) prDates
