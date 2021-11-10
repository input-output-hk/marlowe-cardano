{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.PayoffFs
  ( payoffFs )
where

import Data.Time (LocalTime)
import Language.Marlowe (Observation, Value)
import Language.Marlowe.ACTUS.Domain.BusinessEvents (EventType (..), RiskFactorsMarlowe, RiskFactorsPoly (..))
import Language.Marlowe.ACTUS.Domain.ContractState (ContractStateMarlowe, ContractStatePoly (..))
import Language.Marlowe.ACTUS.Domain.ContractTerms (CT (..), ContractTerms, ContractTermsPoly (..))
import Language.Marlowe.ACTUS.Domain.Ops (ActusNum (..), ActusOps (..), YearFractionOps (_y), marloweFixedPoint)
import Language.Marlowe.ACTUS.Generator.MarloweCompat (constnt)
import Language.Marlowe.ACTUS.Model.POF.PayoffModel
import Prelude hiding (Fractional, Num, (*), (+), (-), (/))

-- |Payoff function for ACTUS contracts with /fixed schedules/
-- This representation of the payoff function is less generic
-- than 'Language.Marlowe.ACTUS.Model.POF.Payoff.payoff' as it
-- does not off-load everythins into Marlowe
--
-- * Providing time information in the interface allows to properly
--   apply day count conventions based on a calender
--
-- * Schedules can not be observed during contract execution, all
--   event are scheduled at contact creation. Risk factors on the
--   other hand can be provided as input or by an oracle
payoffFs ::
     EventType                 -- ^ Event type
  -> RiskFactorsMarlowe        -- ^ Risk factors represented as Marlowe
  -> ContractTerms             -- ^ Contract terms
  -> ContractStateMarlowe      -- ^ Contract state represented as Marlowe
  -> LocalTime                 -- ^ Previous date
  -> LocalTime                 -- ^ Current date
  -> Maybe (Value Observation) -- ^ Payoff represented as Marlowe
payoffFs ev rf ct st sd t = scale <$> payoffFs' ev rf ct st sd t
  where
    scale x = x / (constnt $ fromIntegral marloweFixedPoint)

payoffFs' ::
     EventType                 -- ^ Event type
  -> RiskFactorsMarlowe        -- ^ Risk factors represented as Marlowe
  -> ContractTerms             -- ^ Contract terms
  -> ContractStateMarlowe      -- ^ Contract state represented as Marlowe
  -> LocalTime                 -- ^ Previous date
  -> LocalTime                 -- ^ Current date
  -> Maybe (Value Observation) -- ^ Unscaled payoff represented as Marlowe
-- IED
payoffFs'
  IED
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { notionalPrincipal = Just nt,
      premiumDiscountAtIED = Just pdied,
      contractRole
    }
  _ _ _ = Just $ _POF_IED_PAM o_rf_CURS contractRole (constnt nt) (constnt pdied)
payoffFs'
  IED
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { notionalPrincipal = Just nt,
      contractRole
    }
  _ _ _ = Just $ _POF_IED_PAM o_rf_CURS contractRole (constnt nt) _zero
-- PR
payoffFs'
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
  _ _ = Just $ _POF_PR_LAM o_rf_CURS contractRole nt nsc prnxt
payoffFs'
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
      ipnr
    }
  sd t | contractType `elem` [NAM, ANN] =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_PR_NAM o_rf_CURS contractRole nsc prnxt ipac y_sd_t ipnr ipcb nt
-- MD
payoffFs'
  MD
  _
  ContractTermsPoly
    { contractType = OPTNS
    }
  _
  _ _ = Just _POF_MD_OPTNS
payoffFs'
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
    } _ _ = Just $ _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
-- PP
payoffFs'
  PP
  RiskFactorsPoly
    { o_rf_CURS,
      pp_payoff
    }
  _
  _
  _ _ = Just $ _POF_PP_PAM o_rf_CURS pp_payoff
-- PY
payoffFs'
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
      ipnr
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $_POF_PY_PAM pytp o_rf_CURS o_rf_RRMO (constnt pyrt) contractRole nt ipnr y_sd_t
-- FP
payoffFs'
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
      feac
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_FP_PAM feb (constnt fer) o_rf_CURS contractRole nt feac y_sd_t
-- PRD
payoffFs'
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
    { nt,
      ipac,
      ipnr
    }
  sd t | contractType `elem` [PAM, OPTNS, FUTUR] =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_PRD_PAM o_rf_CURS contractRole (constnt pprd) ipac ipnr nt y_sd_t
payoffFs'
  PRD
  _
  ContractTermsPoly
    { contractType = STK,
      priceAtPurchaseDate = Just pprd,
      contractRole
    }
  _
  _ _ = Just $ _POF_PRD_STK contractRole (constnt pprd)
payoffFs'
  PRD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { dayCountConvention = Just dcc,
      priceAtPurchaseDate = Just pprd,
      maturityDate,
      contractRole
    }
  ContractStatePoly
    { ipac,
      ipcb,
      ipnr
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_PRD_LAM o_rf_CURS contractRole (constnt pprd) ipac ipnr ipcb y_sd_t
-- TD
payoffFs'
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
      ipnr
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_TD_PAM o_rf_CURS contractRole (constnt ptd) ipac ipnr nt y_sd_t
payoffFs'
  TD
  _
  ContractTermsPoly
    { contractType = STK,
      priceAtTerminationDate = Just ptd,
      contractRole
    }
  _
  _ _ = Just $ _POF_TD_STK contractRole (constnt ptd)
payoffFs'
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
      ipnr
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_TD_LAM o_rf_CURS contractRole (constnt ptd) ipac ipnr ipcb y_sd_t
-- IP
payoffFs'
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
      ipnr
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_IP_PAM o_rf_CURS isc ipac ipnr nt y_sd_t
payoffFs'
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
      ipnr
    }
  sd t =
    let y_sd_t = constnt $ _y dcc sd t maturityDate
     in Just $ _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t
-- DV
payoffFs'
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
  _ _ = Just $ _POF_DV_STK contractRole o_rf_CURS dv_payoff
-- STD
payoffFs'
  STD
  RiskFactorsPoly
    { o_rf_CURS
    }
  ContractTermsPoly
    { contractType,
      contractRole
    }
  ContractStatePoly
    { xa = Just exerciseAmount}
  _ _ | contractType `elem` [OPTNS, FUTUR] = Just $ _POF_STD_OPTNS contractRole o_rf_CURS exerciseAmount

payoffFs' _ _ _ _ _ _ = Nothing
