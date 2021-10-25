{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.ACTUS.Model.POF.PayoffFs where

import           Data.Maybe                                        (fromMaybe)
import           Data.Time                                         (LocalTime)
import           Language.Marlowe                                  (Observation, Value)
import           Language.Marlowe.ACTUS.Definitions.BusinessEvents (EventType (..), RiskFactorsMarlowe,
                                                                    RiskFactorsPoly (..))
import           Language.Marlowe.ACTUS.Definitions.ContractState  (ContractStateMarlowe, ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Definitions.ContractTerms  (CT (..), ContractTerms, ContractTermsPoly (..),
                                                                    FEB (..))
import           Language.Marlowe.ACTUS.MarloweCompat              (constnt, enum)
import           Language.Marlowe.ACTUS.Model.POF.PayoffModel
import           Language.Marlowe.ACTUS.Ops                        (ActusNum (..), YearFractionOps (_y),
                                                                    marloweFixedPoint)
import           Prelude                                           hiding (Fractional, Num, (*), (+), (-), (/))

payoffFs :: EventType -> RiskFactorsMarlowe -> ContractTerms -> ContractStateMarlowe -> LocalTime -> LocalTime -> Maybe (Value Observation)
payoffFs
  ev
  RiskFactorsPoly {..}
  ContractTermsPoly
    { notionalPrincipal = Just np,
      penaltyType = Just pytp,
      dayCountConvention = Just dcc,
      ..
    }
  ContractStatePoly {..}
  prevDate
  curDate =
    let pof = case contractType of
          PAM -> case ev of
            IED -> Just $ _POF_IED_PAM o_rf_CURS contractRole notionalPrincipal premiumDiscount
            MD  -> Just $ _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
            PP  -> Just $ _POF_PP_PAM o_rf_CURS pp_payoff
            PY  -> Just $ _POF_PY_PAM pytp o_rf_CURS o_rf_RRMO pyrt contractRole nt ipnr y_sd_t
            FP  -> Just $ _POF_FP_PAM feb fer o_rf_CURS contractRole nt feac y_sd_t
            PRD -> Just $ _POF_PRD_PAM o_rf_CURS contractRole pprd ipac ipnr nt y_sd_t
            TD  -> Just $ _POF_TD_PAM o_rf_CURS contractRole ptd ipac ipnr nt y_sd_t
            IP  -> Just $ _POF_IP_PAM o_rf_CURS isc ipac ipnr nt y_sd_t
            _   -> Nothing
          LAM -> case ev of
            IED -> Just $ _POF_IED_PAM o_rf_CURS contractRole notionalPrincipal premiumDiscount
            PR  -> Just $ _POF_PR_LAM o_rf_CURS contractRole nt nsc prnxt
            MD  -> Just $ _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
            PP  -> Just $ _POF_PP_PAM o_rf_CURS pp_payoff
            PY  -> Just $ _POF_PY_PAM pytp o_rf_CURS o_rf_RRMO pyrt contractRole nt ipnr y_sd_t
            FP  -> Just $ _POF_FP_PAM feb fer o_rf_CURS contractRole nt feac y_sd_t
            PRD -> Just $ _POF_PRD_LAM o_rf_CURS contractRole pprd ipac ipnr ipcb y_sd_t
            TD  -> Just $ _POF_TD_LAM o_rf_CURS contractRole ptd ipac ipnr ipcb y_sd_t
            IP  -> Just $ _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t
            _   -> Nothing
          NAM -> case ev of
            IED -> Just $ _POF_IED_PAM o_rf_CURS contractRole notionalPrincipal premiumDiscount
            PR  -> Just $ _POF_PR_NAM o_rf_CURS contractRole nsc prnxt ipac y_sd_t ipnr ipcb nt
            MD  -> Just $ _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
            PP  -> Just $ _POF_PP_PAM o_rf_CURS pp_payoff
            PY  -> Just $ _POF_PY_PAM pytp o_rf_CURS o_rf_RRMO pyrt contractRole nt ipnr y_sd_t
            FP  -> Just $ _POF_FP_PAM feb fer o_rf_CURS contractRole nt feac y_sd_t
            PRD -> Just $ _POF_PRD_LAM o_rf_CURS contractRole pprd ipac ipnr ipcb y_sd_t
            TD  -> Just $ _POF_TD_LAM o_rf_CURS contractRole ptd ipac ipnr ipcb y_sd_t
            IP  -> Just $ _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t
            _   -> Nothing
          ANN -> case ev of
            IED -> Just $ _POF_IED_PAM o_rf_CURS contractRole notionalPrincipal premiumDiscount
            PR  -> Just $ _POF_PR_NAM o_rf_CURS contractRole nsc prnxt ipac y_sd_t ipnr ipcb nt
            MD  -> Just $ _POF_MD_PAM o_rf_CURS nsc nt isc ipac feac
            PP  -> Just $ _POF_PP_PAM o_rf_CURS pp_payoff
            PY  -> Just $ _POF_PY_PAM penaltyType o_rf_CURS o_rf_RRMO pyrt contractRole nt ipnr y_sd_t
            FP  -> Just $ _POF_FP_PAM feb fer o_rf_CURS contractRole nt feac y_sd_t
            PRD -> Just $ _POF_PRD_LAM o_rf_CURS contractRole pprd ipac ipnr ipcb y_sd_t
            TD  -> Just $ _POF_TD_LAM o_rf_CURS contractRole ptd ipac ipnr ipcb y_sd_t
            IP  -> Just $ _POF_IP_LAM o_rf_CURS isc ipac ipnr ipcb y_sd_t
            _   -> Nothing
          STK -> case ev of
            PRD -> Just $ _POF_PRD_STK contractRole pprd
            TD  -> Just $ _POF_TD_STK contractRole ptd
            DV  -> Just $ _POF_DV_STK contractRole o_rf_CURS pp_payoff
            _   -> Nothing
          OPTNS -> case ev of
            PRD -> Just $ _POF_PRD_PAM o_rf_CURS contractRole pprd ipac ipnr nt y_sd_t
            TD  -> Just $ _POF_TD_PAM o_rf_CURS contractRole ptd ipac ipnr nt y_sd_t
            STD -> _POF_STD_OPTNS contractRole o_rf_CURS <$> xa
            _   -> Nothing
          FUTUR -> case ev of
            PRD -> Just $ _POF_PRD_PAM o_rf_CURS contractRole pprd ipac ipnr nt y_sd_t
            TD  -> Just $ _POF_TD_PAM o_rf_CURS contractRole ptd ipac ipnr nt y_sd_t
            STD -> _POF_STD_OPTNS contractRole o_rf_CURS <$> xa
            _   -> Nothing
     in (\x -> x / (constnt $ fromIntegral marloweFixedPoint)) <$> pof
    where
      notionalPrincipal = constnt np
      premiumDiscount = constnt (fromMaybe 0.0 premiumDiscountAtIED)
      penaltyType = enum pytp
      feb = enum (fromMaybe FEB_N feeBasis)
      fer = constnt (fromMaybe 0.0 feeRate)
      pprd = constnt (fromMaybe 0.0 priceAtPurchaseDate)
      ptd = constnt (fromMaybe 0.0 priceAtTerminationDate)
      pyrt = constnt (fromMaybe 0.0 penaltyRate)
      y_sd_t = constnt $ _y dcc prevDate curDate maturityDate
payoffFs _ _ _ _ _ _ = Nothing
