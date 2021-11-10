{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.Payoff
  ( payoff )
where

import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTermsPoly (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusOps (..), RoleSignOps (..), YearFractionOps (_y))
import Language.Marlowe.ACTUS.Model.POF.PayoffModel

-- |Generic payoff function for ACTUS contracts
payoff :: (RoleSignOps a, YearFractionOps b a) =>
     EventType             -- ^ Event type
  -> RiskFactorsPoly a     -- ^ Risk factors
  -> ContractTermsPoly a b -- ^ Contract terms
  -> ContractStatePoly a b -- ^ Contract state
  -> b                     -- ^ Time
  -> a                     -- ^ Payoff amount
-- IED
payoff
  IED
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { notionalPrincipal = Just nt,
      premiumDiscountAtIED = Just pdied,
      contractRole
    }
  _
  _ = _POF_IED_PAM o_rf_CURS contractRole nt pdied
payoff
  IED
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { notionalPrincipal = Just nt,
      contractRole
    }
  _
  _ = _POF_IED_PAM o_rf_CURS contractRole nt _zero
-- PR
payoff
  PR
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType = LAM,
      contractRole
    }
  ContractStatePoly
    { nt,
      nsc,
      prnxt
    }
  _ = _POF_PR_LAM o_rf_CURS contractRole nt nsc prnxt
payoff
  PR
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType,
      dayCountConvention = Just dcc,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { nt,
      nsc,
      prnxt,
      ipac,
      ipcb,
      ipnr,
      sd
    }
  t | contractType `elem` [NAM, ANN] =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_PR_NAM o_rf_CURS contractRole nsc prnxt ipac y_sd_t ipnr ipcb nt
-- MD
payoff
  MD
  _
  ContractTermsPoly
    { contractType = OPTNS
    }
  _
  _ = _POF_MD_OPTNS
payoff
  MD
  RiskFactorsPoly
    { o_rf_CURS
    }
  _
  ContractStatePoly
    { nt,
      nsc,
      isc,
      ipac,
      feac
    } _ = _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
-- PP
payoff
  PP
  RiskFactorsPoly
    { o_rf_CURS,
      dv_payoff
    }
  _
  _
  _ = _POF_PP_PAM o_rf_CURS dv_payoff
-- PY
payoff
  PY
  RiskFactorsPoly
    { o_rf_CURS,
      o_rf_RRMO
    }
  ContractTermsPoly
    { penaltyType = Just pytp,
      penaltyRate = Just pyrt,
      dayCountConvention = Just dcc,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { nt,
      ipnr,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_PY_PAM pytp o_rf_CURS o_rf_RRMO pyrt contractRole nt ipnr y_sd_t
-- FP
payoff
  FP
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { dayCountConvention = Just dcc,
      feeBasis = Just feb,
      feeRate = Just fer,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { nt,
      feac,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_FP_PAM feb fer o_rf_CURS contractRole nt feac y_sd_t
-- PRD
payoff
  PRD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType = PAM,
      dayCountConvention = Just dcc,
      priceAtPurchaseDate = Just pprd,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { nt,
      ipac,
      ipnr,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_PRD_PAM o_rf_CURS contractRole pprd ipac ipnr nt y_sd_t
payoff
  PRD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType,
      dayCountConvention = Just dcc,
      priceAtPurchaseDate = Just pprd,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { ipac,
      ipcb,
      ipnr,
      sd
    }
  t | contractType `elem` [LAM, NAM, ANN] =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_PRD_LAM o_rf_CURS contractRole pprd ipac ipnr ipcb y_sd_t
payoff
  PRD
  _
  ContractTermsPoly
    { contractType,
      priceAtPurchaseDate = Just pprd,
      contractRole
    }
  _
  _ | contractType `elem` [STK, OPTNS, FUTUR] = _POF_PRD_STK contractRole pprd
-- TD
payoff
  TD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType = PAM,
      dayCountConvention = Just dcc,
      priceAtTerminationDate = Just ptd,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { nt,
      ipac,
      ipnr,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_TD_PAM o_rf_CURS contractRole ptd ipac ipnr nt y_sd_t
payoff
  TD
  _
  ContractTermsPoly
    { contractType = STK,
      priceAtTerminationDate = Just ptd,
      contractRole
    }
  _
  _ = _POF_TD_STK contractRole ptd
payoff
  TD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { dayCountConvention = Just dcc,
      priceAtTerminationDate = Just ptd,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { ipac,
      ipcb,
      ipnr,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_TD_LAM o_rf_CURS contractRole ptd ipac ipnr ipcb y_sd_t
-- IP
payoff
  IP
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType = PAM,
      dayCountConvention = Just dcc,
      maturityDate
    }
  ContractStatePoly
    { nt,
      isc,
      ipac,
      ipnr,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_IP_PAM o_rf_CURS isc ipac ipnr nt y_sd_t
payoff
  IP
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { dayCountConvention = Just dcc,
      maturityDate
    }
  ContractStatePoly
    { isc,
      ipac,
      ipcb,
      ipnr,
      sd
    }
  t =
    let y_sd_t = _y dcc sd t maturityDate
     in _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t
-- DV
payoff
  DV
  RiskFactorsPoly
    { o_rf_CURS,
      dv_payoff
    }
  ContractTermsPoly
    { contractType = STK,
      contractRole
    }
  _
  _ = _POF_DV_STK contractRole o_rf_CURS dv_payoff
-- STD
payoff
  STD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType,
      contractRole
    }
  ContractStatePoly
    { xa = Just exerciseAmount
    }
  _ | contractType `elem` [OPTNS, FUTUR] = _POF_STD_OPTNS contractRole o_rf_CURS exerciseAmount
payoff _ _ _ _ _ = _zero
