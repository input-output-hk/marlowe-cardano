{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.STF.StateTransitionModel where

import Data.Maybe (fromMaybe)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (ContractTermsPoly (..), FEB (..), IPCB (..), OPTP (..), SCEF (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusNum (..), ActusOps (..), DateOps (_lt), RoleSignOps (_r))
import Language.Marlowe.ACTUS.Utility.ANN.Annuity (annuity)
import Prelude hiding (Fractional, Num, (*), (+), (-), (/))

-- Principal at Maturity (PAM)

_STF_AD_PAM :: ActusNum a => ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_AD_PAM st@ContractStatePoly {..} t y_sd_t =
  st
    { ipac = ipac + y_sd_t * ipnr * nt,
      sd = t
    }

_STF_IED_PAM :: (RoleSignOps a, DateOps b a) => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_IED_PAM
  ContractTermsPoly
    { nominalInterestRate,
      notionalPrincipal = Just nt,
      accruedInterest = Just ipac,
      contractRole
    }
  st
  t
  _ =
    st
      { nt = _r contractRole * nt,
        ipnr = fromMaybe _zero nominalInterestRate,
        ipac = ipac,
        sd = t
      }
_STF_IED_PAM
  ContractTermsPoly
    { nominalInterestRate,
      notionalPrincipal = Just nt,
      cycleAnchorDateOfInterestPayment = Just ipanx,
      contractRole
    }
  st
  t
  y_ipanx_t =
    let nt' = _r contractRole * nt
        ipnr' = fromMaybe _zero nominalInterestRate
     in st
          { nt = nt',
            ipnr = ipnr',
            ipac = _lt ipanx t * y_ipanx_t * nt' * ipnr',
            sd = t
          }
_STF_IED_PAM _ st _ _ = st

_STF_MD_PAM :: ActusOps a => ContractStatePoly a b -> b -> ContractStatePoly a b
_STF_MD_PAM st t =
  st
    { nt = _zero,
      ipac = _zero,
      feac = _zero,
      sd = t
    }

_STF_PP_PAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> ContractStatePoly a b
_STF_PP_PAM ct st@ContractStatePoly {..} RiskFactorsPoly {..} t y_sd_t y_tfpminus_t y_tfpminus_tfpplus =
  let st' = _STF_PY_PAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
   in st'
        { nt = nt - pp_payoff
        }

_STF_PY_PAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_PY_PAM
  ContractTermsPoly
    { feeBasis = Just FEB_N,
      feeRate = Just fer,
      notionalPrincipal = Just nt'
    }
  st@ContractStatePoly {..}
  t
  y_sd_t
  _
  _ =
    st
      { ipac = ipac + y_sd_t * ipnr * nt,
        feac = feac + y_sd_t * nt' * fer,
        sd = t
      }
_STF_PY_PAM
  ContractTermsPoly
    { feeRate = Just fer,
      contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let
     in st
          { ipac = ipac + y_sd_t * ipnr * nt,
            feac = _max _zero (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fer,
            sd = t
          }
_STF_PY_PAM _ st _ _ _ _ = st

_STF_FP_PAM :: (ActusNum a, ActusOps a) => ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_FP_PAM st@ContractStatePoly {..} t y_sd_t =
  st
    { ipac = ipac + y_sd_t * ipnr * nt,
      feac = _zero,
      sd = t
    }

_STF_PRD_PAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_PRD_PAM = _STF_PY_PAM

_STF_TD_PAM :: ActusOps a => ContractStatePoly a b -> b -> ContractStatePoly a b
_STF_TD_PAM st t =
  st
    { nt = _zero,
      ipac = _zero,
      feac = _zero,
      ipnr = _zero,
      sd = t
    }

_STF_IP_PAM :: (ActusOps a, ActusNum a) => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_IP_PAM
  ContractTermsPoly
    { feeRate = Just fer
    }
  st@ContractStatePoly {..}
  t
  y_sd_t =
    st
      { ipac = _zero,
        feac = y_sd_t * fer * nt,
        sd = t
      }
_STF_IP_PAM
  _
  st
  t
  _ =
    st
      { ipac = _zero,
        feac = _zero,
        sd = t
      }

_STF_IPCI_PAM :: (ActusOps a, ActusNum a) => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_IPCI_PAM
  ct
  st@ContractStatePoly {..}
  t
  y_sd_t =
    let st' = _STF_IP_PAM ct st t y_sd_t
     in st'
          { nt = nt + ipac + y_sd_t * nt * ipnr
          }

_STF_RR_PAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> ContractStatePoly a b
_STF_RR_PAM
  ct@ContractTermsPoly
    { feeBasis = Just FEB_N,
      feeRate = Just fer,
      lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp
    }
  st@ContractStatePoly {..}
  RiskFactorsPoly
    { ..
    }
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PRD_PAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
        delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc
        ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
     in st'
          { ipac = ipac + y_sd_t * ipnr * nt,
            feac = feac + y_sd_t * nt * fer,
            ipnr = ipnr',
            sd = t
          }
_STF_RR_PAM
  ct@ContractTermsPoly
    { feeRate = Just fer,
      lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp,
      contractRole
    }
  st@ContractStatePoly {..}
  RiskFactorsPoly
    { o_rf_RRMO
    }
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PRD_PAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
        delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc
        ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
     in st'
          { ipac = ipac + y_sd_t * ipnr * nt,
            feac = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fer,
            ipnr = ipnr',
            sd = t
          }
_STF_RR_PAM _ st _ _ _ _ _ = st

_STF_RRF_PAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_RRF_PAM
  ct@ContractTermsPoly
    { nextResetRate = rrnxt
    }
  st
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PRD_PAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
     in st'
          { ipnr = fromMaybe _zero rrnxt
          }

_STF_SC_PAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> ContractStatePoly a b
_STF_SC_PAM
  ct@ContractTermsPoly
    { scalingEffect = Just scef,
      scalingIndexAtStatusDate = Just scied
    }
  st@ContractStatePoly {..}
  RiskFactorsPoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PY_PAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus

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
_STF_SC_PAM _ st _ _ _ _ _ = st

_STF_CE_PAM :: ActusNum a => ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_CE_PAM = _STF_AD_PAM

-- Linear Amortiser (LAM)

_STF_IED_LAM :: (RoleSignOps a, Ord b) => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_IED_LAM
  ct@ContractTermsPoly
    { notionalPrincipal = Just nt,
      nominalInterestRate = Just ipnr,
      contractRole
    }
  st
  t
  y_ipanx_t =
    let nt' = _r contractRole * nt
        ipcb' = interestCalculationBase ct
          where
            interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
            interestCalculationBase ContractTermsPoly {interestCalculationBaseA = Just ipcba}  = _r contractRole * ipcba
            interestCalculationBase _                                                          = _zero
        ipac' = interestAccrued ct
          where
            interestAccrued ContractTermsPoly {accruedInterest = Just ipac} = _r contractRole * ipac
            interestAccrued ContractTermsPoly {cycleAnchorDateOfInterestPayment = Just ipanx} | ipanx < t = y_ipanx_t * nt' * ipcb'
            interestAccrued _ = _zero
     in st
          { nt = nt',
            ipnr = ipnr,
            ipac = ipac',
            ipcb = ipcb',
            sd = t
          }
_STF_IED_LAM _ st _ _ = st

_STF_PR_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_PR_LAM
  ct@ContractTermsPoly
    { feeRate = Just fer,
      contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t =
    let nt' = nt - _r contractRole * (prnxt - _r contractRole * _max _zero (_abs prnxt - _abs nt))
        ipcb' = interestCalculationBase ct
          where
            interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NTL} = ipcb
            interestCalculationBase _                                                           = nt'
     in st
          { nt = nt',
            feac = feac + y_sd_t * nt * fer,
            ipcb = ipcb',
            ipac = ipac + ipnr * ipcb * y_sd_t,
            sd = t
          }
_STF_PR_LAM
  ct@ContractTermsPoly
    { contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t =
    let nt' = nt - _r contractRole * (prnxt - _r contractRole * _max _zero (_abs prnxt - _abs nt))
        ipcb' = interestCalculationBase ct
          where
            interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NTL} = ipcb
            interestCalculationBase _                                                           = nt'
     in st
          { nt = nt',
            feac = feac,
            ipcb = ipcb',
            ipac = ipac + ipnr * ipcb * y_sd_t,
            sd = t
          }

_STF_MD_LAM :: ActusOps a => ContractStatePoly a b -> b -> ContractStatePoly a b
_STF_MD_LAM st t =
  st
    { nt = _zero,
      ipac = _zero,
      feac = _zero,
      ipcb = _zero,
      sd = t
    }

_STF_PP_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> ContractStatePoly a b
_STF_PP_LAM
  ct
  st@ContractStatePoly {..}
  RiskFactorsPoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PY_LAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
        nt' = nt - pp_payoff
        ipcb' = interestCalculationBase ct
          where
            interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
            interestCalculationBase _                                                          = ipcb
     in st'
          { nt = nt',
            ipcb = ipcb'
          }

_STF_PY_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_PY_LAM
  ct@ContractTermsPoly
    { feeRate = Just fer,
      contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let ipac' = ipac + y_sd_t * ipnr * ipcb
        feac' = feeAccrued ct
          where
            feeAccrued ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fer
            feeAccrued _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fer
     in st
          { ipac = ipac',
            feac = feac',
            sd = t
          }
_STF_PY_LAM
  ct
  st@ContractStatePoly {..}
  t
  y_sd_t
  _
  _ =
    let ipac' = ipac + y_sd_t * ipnr * ipcb
        feac' = feeAccrued ct
          where
            feeAccrued ContractTermsPoly {feeBasis = Just FEB_N} = feac
            feeAccrued _                                         = _zero
     in st
          { ipac = ipac',
            feac = feac',
            sd = t
          }

_STF_FP_LAM :: (ActusNum a, ActusOps a) => ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_FP_LAM
  st@ContractStatePoly {..}
  t
  y_sd_t =
    st
      { ipac = ipac + y_sd_t * ipnr * ipcb,
        feac = _zero,
        sd = t
      }

_STF_PRD_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_PRD_LAM = _STF_PY_LAM

_STF_IPCI_LAM :: (ActusOps a, ActusNum a) => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_IPCI_LAM
  ct
  st@ContractStatePoly {..}
  t
  y_sd_t =
    let st' = _STF_IP_PAM ct st t y_sd_t
        nt' = nt + ipac + y_sd_t * ipnr * ipcb
        ipcb' = interestCalculationBase ct
          where
            interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
            interestCalculationBase _                                                          = ipcb
     in st'
          { nt = nt',
            ipcb = ipcb'
          }

_STF_IPCB_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_IPCB_LAM
  ct
  st@ContractStatePoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PRD_LAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
     in st'
          { ipcb = nt
          }

_STF_RR_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> ContractStatePoly a b
_STF_RR_LAM
  ct@ContractTermsPoly
    { lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp
    }
  st@ContractStatePoly {..}
  RiskFactorsPoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PRD_LAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
        delta_r = _min (_max (o_rf_RRMO * rrmlt + rrsp - ipnr) rrpf) rrpc
        ipnr' = _min (_max (ipnr + delta_r) rrlf) rrlc
     in st'
          { ipnr = ipnr'
          }
_STF_RR_LAM _ st _ _ _ _ _ = st

_STF_RRF_LAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> ContractStatePoly a b
_STF_RRF_LAM
  ct@ContractTermsPoly
    { nextResetRate = rrnxt
    }
  st
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PRD_LAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
     in st'
          { ipnr = fromMaybe _zero rrnxt
          }

_STF_SC_LAM :: (RoleSignOps a) => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> ContractStatePoly a b
_STF_SC_LAM
  ct@ContractTermsPoly
    { scalingIndexAtContractDealDate = Just sccdd,
      scalingEffect = Just scef
    }
  st@ContractStatePoly {..}
  RiskFactorsPoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus =
    let st' = _STF_PY_LAM ct st t y_sd_t y_tfpminus_t y_tfpminus_tfpplus
     in st'
          { nsc = if elem 'N' (show scef) then o_rf_SCMO / sccdd else nsc,
            isc = if elem 'I' (show scef) then o_rf_SCMO / sccdd else nsc
          }
_STF_SC_LAM _ st _ _ _ _ _ = st

-- Negative Amortizer (NAM)

_STF_PR_NAM :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> ContractStatePoly a b
_STF_PR_NAM
  ct@ContractTermsPoly
    { contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t =
    let st'@ContractStatePoly {ipac = ipac'} = _STF_PR_LAM ct st t y_sd_t
        nt' = nt - _r contractRole * r
          where
            ra = prnxt - _r contractRole * ipac'
            r = ra - _max _zero (ra - _abs nt)
        ipcb' = interestCalculationBase ct
          where
            interestCalculationBase ContractTermsPoly {interestCalculationBase = Just IPCB_NT} = nt'
            interestCalculationBase _                                                          = ipcb
     in st'
          { nt = nt',
            ipcb = ipcb'
          }

-- Annuity (ANN)

_STF_RR_ANN :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> a -> a -> a -> [a] -> ContractStatePoly a b
_STF_RR_ANN
  ct@ContractTermsPoly
    { lifeFloor = Just rrlf,
      lifeCap = Just rrlc,
      periodCap = Just rrpc,
      periodFloor = Just rrpf,
      rateMultiplier = Just rrmlt,
      rateSpread = Just rrsp,
      contractRole
    }
  st@ContractStatePoly {..}
  RiskFactorsPoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus
  ti =
    let ipac' = ipac + y_sd_t * ipnr * ipcb
        feac' = feeAccrued ct
          where
            feeAccrued ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fromMaybe _zero (feeRate ct)
            feeAccrued _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fromMaybe _zero (feeRate ct)
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
_STF_RR_ANN _ st _ _ _ _ _ _ = st

_STF_RRF_ANN :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> [a] -> ContractStatePoly a b
_STF_RRF_ANN
  ct@ContractTermsPoly
    { nextResetRate = Just rrnxt,
      contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus
  ti =
    let ipac' = ipac + y_sd_t * ipnr * ipcb
        feac' = feeAccrued ct
          where
            feeAccrued ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fromMaybe _zero (feeRate ct)
            feeAccrued _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fromMaybe _zero (feeRate ct)
        ipnr' = rrnxt
        prnxt' = annuity ipnr' ti
     in st
          { ipac = ipac',
            feac = feac',
            ipnr = ipnr',
            prnxt = prnxt',
            sd = t
          }
_STF_RRF_ANN _ st _ _ _ _ _ = st

_STF_PRF_ANN :: RoleSignOps a => ContractTermsPoly a b -> ContractStatePoly a b -> b -> a -> a -> a -> a -> [a] -> ContractStatePoly a b
_STF_PRF_ANN
  ct@ContractTermsPoly
    { contractRole
    }
  st@ContractStatePoly {..}
  t
  y_sd_t
  y_tfpminus_t
  y_tfpminus_tfpplus
  y_t
  ti =
    let ipac' = ipac + y_sd_t * ipnr * ipcb
        feac' = feeAccrued ct
          where
            feeAccrued ContractTermsPoly {feeBasis = Just FEB_N} = feac + y_sd_t * nt * fromMaybe _zero (feeRate ct)
            feeAccrued _ = (y_tfpminus_t / y_tfpminus_tfpplus) * _r contractRole * fromMaybe _zero (feeRate ct)
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

_STF_XD_OPTNS :: (ActusNum a, ActusOps a) => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> ContractStatePoly a b
_STF_XD_OPTNS
  ContractTermsPoly
    { optionType = Just OPTP_C,
      optionStrike1 = Just ops1
    }
  st
  RiskFactorsPoly {..}
  t = st
    { xa = Just $ _max (xd_payoff - ops1) _zero,
      sd = t
    }
_STF_XD_OPTNS
  ContractTermsPoly
    { optionType = Just OPTP_P,
      optionStrike1 = Just ops1
    }
  st
  RiskFactorsPoly {..}
  t = st
    { xa = Just $ _max (ops1 - xd_payoff) _zero,
      sd = t
    }
_STF_XD_OPTNS
  ContractTermsPoly
    { optionType = Just OPTP_CP,
      optionStrike1 = Just ops1
    }
  st
  RiskFactorsPoly {..}
  t = st
    { xa = Just $ _max (xd_payoff - ops1) _zero + _max (ops1 - xd_payoff) _zero,
      sd = t
    }
_STF_XD_OPTNS _ st _ t =
  st
    { sd = t
    }

_STF_XD_FUTUR :: ActusNum a => ContractTermsPoly a b -> ContractStatePoly a b -> RiskFactorsPoly a -> b -> ContractStatePoly a b
_STF_XD_FUTUR
  ContractTermsPoly
    { futuresPrice = Just pfut
    }
  st
  RiskFactorsPoly {..}
  t = st
    { xa = Just $ xd_payoff - pfut,
      sd = t
    }
_STF_XD_FUTUR _ _ _ _ = undefined

