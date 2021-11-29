{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.Payoff
  ( payoff )
where

import           Data.Time.LocalTime                          (LocalTime)
import           Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsPoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractState  (ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms  (CT (..), ContractTermsPoly (..), FEB (..), PYTP (..))
import           Language.Marlowe.ACTUS.Domain.Ops            (ActusNum (..), ActusOps (..), RoleSignOps (..),
                                                               YearFractionOps (_y))
import           Prelude                                      hiding (Fractional, Num, (*), (+), (-), (/))

-- |Generic payoff function for ACTUS contracts
payoff :: (RoleSignOps a, YearFractionOps a) =>
     EventType           -- ^ Event type
  -> RiskFactorsPoly a   -- ^ Risk factors
  -> ContractTermsPoly a -- ^ Contract terms
  -> ContractStatePoly a -- ^ Contract state
  -> LocalTime           -- ^ Time
  -> a                   -- ^ Payoff amount
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
  _ = _negate $ o_rf_CURS * _r contractRole * (nt + pdied)
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
  _ = _negate $ o_rf_CURS * _r contractRole * nt
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
  _ = let redemption = prnxt - _r contractRole * _max _zero (_abs prnxt - _abs nt)
       in o_rf_CURS * _r contractRole * nsc * redemption
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
        ra = prnxt - _r contractRole * (ipac + y_sd_t * ipnr * ipcb)
        r = ra - _max _zero (ra - _abs nt)
     in o_rf_CURS * _r contractRole * nsc * r
-- MD
payoff
  MD
  _
  ContractTermsPoly
    { contractType = OPTNS
    }
  _
  _ = _zero
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
    }
  _ = o_rf_CURS * (nsc * nt + isc * ipac + feac)
-- PP
payoff
  PP
  RiskFactorsPoly
    { o_rf_CURS,
      pp_payoff
    }
  _
  _
  _ = o_rf_CURS * pp_payoff
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
     in
        case pytp of
          PYTP_A -> o_rf_CURS * _r contractRole * pyrt
          PYTP_N -> let c = o_rf_CURS * _r contractRole * y_sd_t * nt in c * pyrt
          PYTP_I -> let c = o_rf_CURS * _r contractRole * y_sd_t * nt in c * _max _zero (ipnr - o_rf_RRMO)
          PYTP_O -> undefined
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
     in
      case feb of
        FEB_A -> _r contractRole * o_rf_CURS * fer
        FEB_N -> o_rf_CURS * fer * y_sd_t * nt * feac
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
     in _negate $ o_rf_CURS * _r contractRole * (pprd + ipac + y_sd_t * ipnr * nt)
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
     in _negate $ o_rf_CURS * _r contractRole * (pprd + ipac + y_sd_t * ipnr * ipcb)
payoff
  PRD
  _
  ContractTermsPoly
    { contractType,
      priceAtPurchaseDate = Just pprd,
      contractRole
    }
  _
  _ | contractType `elem` [STK, OPTNS, FUTUR] = _negate $ _r contractRole * pprd
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
     in o_rf_CURS * _r contractRole * (ptd + ipac + y_sd_t * ipnr * nt)
payoff
  TD
  _
  ContractTermsPoly
    { contractType = STK,
      priceAtTerminationDate = Just ptd,
      contractRole
    }
  _
  _ = _r contractRole * ptd
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
     in o_rf_CURS * _r contractRole * (ptd + ipac + y_sd_t * ipnr * ipcb)
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
     in o_rf_CURS * isc * (ipac + y_sd_t * ipnr * nt)
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
     in o_rf_CURS * isc * (ipac + y_sd_t * ipnr * ipcb)
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
  _ = o_rf_CURS * _r contractRole * dv_payoff
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
  _ | contractType `elem` [OPTNS, FUTUR] = o_rf_CURS * _r contractRole * exerciseAmount
payoff _ _ _ _ _ = _zero
