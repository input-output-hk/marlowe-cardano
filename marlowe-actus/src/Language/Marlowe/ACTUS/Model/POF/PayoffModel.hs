module Language.Marlowe.ACTUS.Model.POF.PayoffModel where

import Language.Marlowe.ACTUS.Domain.ContractTerms (CR (..), FEB (FEB_A, FEB_N), PYTP (PYTP_A, PYTP_I, PYTP_N, PYTP_O))
import Language.Marlowe.ACTUS.Domain.Ops (ActusNum (..), ActusOps (_abs, _max, _zero), RoleSignOps (_r))
import Prelude hiding (Fractional, Num, (*), (+), (-), (/))

-- Principal at Maturity (PAM)

_POF_IED_PAM :: RoleSignOps a => a -> CR -> a -> a -> a
_POF_IED_PAM o_rf_curs cntrl nt pdied = _zero - o_rf_curs * _r cntrl * (nt + pdied)

_POF_MD_PAM :: ActusNum a => a -> a -> a -> a -> a -> a -> a
_POF_MD_PAM o_rf_curs nsc nt isct ipac feac = o_rf_curs * (nsc * nt + isct * ipac + feac)

_POF_PP_PAM :: ActusNum a => a -> a -> a
_POF_PP_PAM o_rf_curs pp_payoff = o_rf_curs * pp_payoff

_POF_PY_PAM :: RoleSignOps a => PYTP -> a -> a -> a -> CR -> a -> a -> a -> a
_POF_PY_PAM PYTP_A o_rf_curs _ pyrt cntrl _ _ _ = o_rf_curs * _r cntrl * pyrt
_POF_PY_PAM PYTP_N o_rf_curs _ pyrt cntrl nt _ y_sd_t = let c = o_rf_curs * _r cntrl * y_sd_t * nt in  c * pyrt
_POF_PY_PAM PYTP_I o_rf_curs o_rf_rrmo _ cntrl nt ipnr y_sd_t = let c = o_rf_curs * _r cntrl * y_sd_t * nt in  c * _max _zero (ipnr - o_rf_rrmo)
_POF_PY_PAM PYTP_O _ _ _ _ _ _ _ = undefined -- FIXME: Ask Nils

_POF_FP_PAM :: RoleSignOps a => FEB -> a -> a -> CR -> a -> a -> a -> a
_POF_FP_PAM FEB_A fer o_rf_curs cntrl _  _   _  = _r cntrl * o_rf_curs * fer
_POF_FP_PAM FEB_N fer o_rf_curs _ nt fac y_sd_t = o_rf_curs * fer * y_sd_t * nt * fac

_POF_PRD_PAM :: RoleSignOps a => a -> CR -> a -> a -> a -> a -> a -> a
_POF_PRD_PAM o_rf_curs cntrl pprd ipac ipnr nt y_sd_t = _zero - o_rf_curs * _r cntrl * (pprd + ipac + y_sd_t * ipnr * nt)

_POF_TD_PAM :: RoleSignOps a => a -> CR -> a -> a -> a -> a -> a -> a
_POF_TD_PAM o_rf_curs cntrl ptd ipac ipnr nt y_sd_t = o_rf_curs * _r cntrl * (ptd + ipac + y_sd_t * ipnr * nt)

_POF_IP_PAM :: ActusNum a => a -> a -> a -> a -> a -> a -> a
_POF_IP_PAM o_rf_curs isc ipac ipnr nt y_sd_t = o_rf_curs * isc * (ipac + y_sd_t * ipnr * nt)

-- Linear Amortizer (LAM)

_POF_PR_LAM :: RoleSignOps a => a -> CR -> a -> a -> a -> a
_POF_PR_LAM o_rf_curs cntrl nt nsc prnxt =
  let redemption = prnxt - _r cntrl * _max _zero (_abs prnxt - _abs nt)
   in o_rf_curs * _r cntrl * nsc * redemption

_POF_PRD_LAM :: RoleSignOps a => a -> CR -> a -> a -> a -> a -> a -> a
_POF_PRD_LAM o_rf_curs cntrl pprd ipac ipnr ipcb y_sd_t = _zero - o_rf_curs * _r cntrl * (pprd + ipac + y_sd_t * ipnr * ipcb)

_POF_TD_LAM :: RoleSignOps a => a -> CR -> a -> a -> a -> a -> a -> a
_POF_TD_LAM o_rf_curs cntrl ptd ipac ipnr ipcb y_sd_t = o_rf_curs * _r cntrl * (ptd + ipac + y_sd_t * ipnr * ipcb)

_POF_IP_LAM :: ActusNum a => a -> a -> a -> a -> a -> a -> a
_POF_IP_LAM o_rf_curs isc ipac ipnr ipcb y_sd_t = o_rf_curs * isc * (ipac + y_sd_t * ipnr * ipcb)

-- Negative Amortizer (NAM)

_POF_PR_NAM :: RoleSignOps a => a -> CR -> a -> a -> a -> a -> a -> a -> a -> a
_POF_PR_NAM o_rf_curs cntrl nsc prnxt ipac y_sd_t ipnr ipcb nt =
  let ra = prnxt - _r cntrl * (ipac + y_sd_t * ipnr * ipcb)
      r = ra - _max _zero (ra - _abs nt)
   in o_rf_curs * _r cntrl * nsc * r

-- Stock (STK)

_POF_PRD_STK :: RoleSignOps a => CR -> a -> a
_POF_PRD_STK cntrl pprd = _zero - _r cntrl * pprd

_POF_TD_STK :: RoleSignOps a => CR -> a -> a
_POF_TD_STK cntrl ptd = _r cntrl * ptd

_POF_DV_STK :: RoleSignOps a => CR -> a -> a -> a
_POF_DV_STK cntrl curs pp_payoff = curs * _r cntrl * pp_payoff

-- Option (OPTNS)

_POF_PRD_OPTNS :: RoleSignOps a => CR -> a -> a
_POF_PRD_OPTNS cntrl pprd = _zero - _r cntrl * pprd

_POF_MD_OPTNS :: RoleSignOps a => a
_POF_MD_OPTNS = _zero

_POF_STD_OPTNS :: (RoleSignOps a) => CR -> a -> a -> a
_POF_STD_OPTNS cntrl curs xa = curs * _r cntrl * xa

-- Future (FUTUR)


