{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

{-| = ACTUS Generator

Given ACTUS contract terms a Marlowe contract is generated.

In 'genFsContract' risk factors are added to the Marlowe contract, i.e. will
be observed during the life time of the contract

-}

module Language.Marlowe.ACTUS.Generator.GeneratorFs
  ( genFsContract )
where

import           Control.Monad.Reader                                       (Reader, runReader)
import qualified Data.List                                                  as L (zip4, zip6)
import           Data.Maybe                                                 (maybeToList)
import           Data.Monoid                                                (Endo (Endo, appEndo))
import           Data.String                                                (IsString (fromString))
import           Data.Time                                                  (LocalTime)
import           Data.Validation                                            (Validation (..))
import           GHC.List                                                   (scanl')
import           Language.Marlowe                                           (Action (..), Bound (..), Case (..),
                                                                             ChoiceId (..), Contract (..),
                                                                             Observation (..), Party (..), Slot (..),
                                                                             Value (..), ValueId (ValueId))
import           Language.Marlowe.ACTUS.Domain.BusinessEvents               (EventType (..), RiskFactors,
                                                                             RiskFactorsMarlowe, RiskFactorsPoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractState                (ContractState, ContractStateMarlowe,
                                                                             ContractStatePoly (..))
import           Language.Marlowe.ACTUS.Domain.ContractTerms                (Assertion (..), AssertionContext (..),
                                                                             Assertions (..), ContractTerms,
                                                                             ContractTermsPoly (..),
                                                                             TermValidationError (..))
import qualified Language.Marlowe.ACTUS.Domain.Ops                          as O (ActusNum (..), YearFractionOps (_y))
import           Language.Marlowe.ACTUS.Domain.Schedule                     (CashFlow (..), ShiftedDay (..),
                                                                             calculationDay)
import           Language.Marlowe.ACTUS.Generator.Analysis                  (genProjectedCashflows)
import           Language.Marlowe.ACTUS.Generator.Generator                 (invoice)
import           Language.Marlowe.ACTUS.Generator.MarloweCompat             (constnt, marloweTime, timeToSlotNumber,
                                                                             toMarloweFixedPoint, useval)
import           Language.Marlowe.ACTUS.Model.APPL.Applicability            (validateTerms)
import           Language.Marlowe.ACTUS.Model.INIT.StateInitializationModel (initializeState)
import           Language.Marlowe.ACTUS.Model.POF.PayoffFs                  (payoffFs)
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule        (schedule)
import           Language.Marlowe.ACTUS.Model.SCHED.ContractSchedule        as S (maturity)
import           Language.Marlowe.ACTUS.Model.STF.StateTransition           (CtxSTF (..))
import           Language.Marlowe.ACTUS.Model.STF.StateTransitionFs         as FS (stateTransition)
import           Ledger.Value                                               (TokenName (TokenName))

-- |'genFsContract' validatate the applicabilty of the contract terms in order
-- to genereate a Marlowe contract with risk factors observed at a given point
-- in time
genFsContract ::
     (EventType -> LocalTime -> RiskFactors)   -- ^ Risk factors per event and time
  -> ContractTerms                             -- ^ ACTUS contract terms
  -> Validation [TermValidationError] Contract -- ^ Marlowe contract or applicabilty errors
genFsContract rf = fmap (genFsContract' rf) . validateTerms

genFsContract' ::
  (EventType -> LocalTime -> RiskFactors) ->
  ContractTerms ->
  Contract
genFsContract' rf ct =
  let gen :: (CashFlow, LocalTime, EventType, Slot, Integer, ContractStateMarlowe) -> Contract -> Contract
      gen (cf, sd, ev, date, i, st) cont =
        let ora = inquiryFs ev ("_" ++ show i) date "oracle" (context <$> constraints ct)
            pof = payoffFs ev (riskFactorAt i) ct st sd (cashCalculationDay cf)
         in ora $ maybe cont (payoff i cf date cont) pof

      stf :: Reader (CtxSTF Double LocalTime) ContractStateMarlowe -> (CashFlow, LocalTime, EventType, Integer) -> Reader (CtxSTF Double LocalTime) ContractStateMarlowe
      stf r (cf, sd, ev, i) = r >>= FS.stateTransition ev (riskFactorAt i) sd (cashCalculationDay cf)

      projectedCashflows = genProjectedCashflows rf ct
      eventTypesOfCashflows = cashEvent <$> projectedCashflows
      paymentDayCashflows = Slot . timeToSlotNumber . cashPaymentDay <$> projectedCashflows
      previousDates = statusDate ct : (cashCalculationDay <$> projectedCashflows)

      st0 = transform <$> initializeState
      states = runReader (sequence $ scanl' stf st0 $ L.zip4 projectedCashflows previousDates eventTypesOfCashflows [1 ..]) initCtx

   in foldr gen (postProcess Close) $ L.zip6 projectedCashflows previousDates eventTypesOfCashflows paymentDayCashflows [1 ..] states
  where
    fpSchedule, prSchedule, ipSchedule :: [LocalTime]
    fpSchedule = calculationDay <$> schedule FP ct
    prSchedule = calculationDay <$> schedule PR ct
    ipSchedule = calculationDay <$> schedule IP ct

    initCtx :: CtxSTF Double LocalTime
    initCtx = CtxSTF ct fpSchedule prSchedule ipSchedule (S.maturity ct)

    transform :: ContractState -> ContractStateMarlowe
    transform st =
      ContractStatePoly
        { nsc = constnt $ nsc st,
          nt = constnt $ nt st,
          isc = constnt $ isc st,
          ipac = constnt $ ipac st,
          feac = constnt $ feac st,
          ipnr = constnt $ ipnr st,
          ipcb = constnt $ ipcb st,
          xa = constnt <$> xa st,
          xd = marloweTime <$> xd st,
          prnxt = constnt $ prnxt st,
          tmd = marloweTime <$> tmd st,
          prf = prf st,
          sd = marloweTime $ sd st
        }

    riskFactorAt :: Integer -> RiskFactorsMarlowe
    riskFactorAt i =
      RiskFactorsPoly
        { o_rf_CURS = useval "o_rf_CURS" i,
          o_rf_RRMO = useval "o_rf_RRMO" i,
          o_rf_SCMO = useval "o_rf_SCMO" i,
          pp_payoff = useval "pp_payoff" i,
          xd_payoff = useval "xd_payoff" i,
          dv_payoff = useval "dv_payoff" i
        }

    payoffAt :: Show a => a -> ValueId
    payoffAt t = ValueId $ fromString $ "payoff_" ++ show t

    postProcess :: Contract -> Contract
    postProcess cont =
      let ctr = constraints ct
          toAssert = genZeroRiskAssertions ct <$> (assertions =<< maybeToList ctr)
          compose = appEndo . mconcat . map Endo
       in compose toAssert cont

    payoff i cf date cont p =
      Let (payoffAt i) p $
        if amount cf > 0.0
          then
            invoice
              "party"
              "counterparty"
              (UseValue $ payoffAt i)
              date
              cont
          else
            if amount cf < 0.0
              then
                invoice
                  "counterparty"
                  "party"
                  (NegValue $ UseValue $ payoffAt i)
                  date
                  cont
              else cont

    inquiryFs :: EventType -> String -> Slot -> String -> Maybe AssertionContext -> Contract -> Contract
    inquiryFs ev timePostfix date oracle context continue =
      let oracleRole = Role $ TokenName $ fromString oracle

          letTemplate inputChoiceId inputOwner cont =
            Let
              (ValueId inputChoiceId)
              (ChoiceValue (ChoiceId inputChoiceId inputOwner))
              cont

          inputTemplate inputChoiceId inputOwner inputBound cont =
            When
              [ Case (Choice (ChoiceId inputChoiceId inputOwner) inputBound) $
                  letTemplate inputChoiceId inputOwner cont
              ]
              date
              Close

          inferBounds name ctx = case (name, ctx) of
            ("o_rf_RRMO", Just AssertionContext {..}) ->
              [Bound (toMarloweFixedPoint rrmoMin) (toMarloweFixedPoint rrmoMax)]
            _ -> [Bound 0 maxPseudoDecimalValue]

          riskFactorInquiry name =
            inputTemplate
              (fromString (name ++ timePostfix))
              oracleRole
              (inferBounds name context)

          riskFactorsInquiryEv AD = id
          riskFactorsInquiryEv SC = riskFactorInquiry "o_rf_SCMO"
          riskFactorsInquiryEv RR = riskFactorInquiry "o_rf_RRMO"
          riskFactorsInquiryEv DV = riskFactorInquiry "o_rf_CURS" . riskFactorInquiry "dv_payoff"
          riskFactorsInquiryEv PP = riskFactorInquiry "o_rf_CURS" . riskFactorInquiry "pp_payoff"
          riskFactorsInquiryEv STD = riskFactorInquiry "o_rf_CURS" . riskFactorInquiry "xd_payoff"
          riskFactorsInquiryEv _ =
            if enableSettlement ct
              then riskFactorInquiry "o_rf_CURS"
              else Let (ValueId (fromString ("o_rf_CURS" ++ timePostfix))) (constnt 1.0)
       in riskFactorsInquiryEv ev continue

    maxPseudoDecimalValue :: Integer
    maxPseudoDecimalValue = 100000000000000

    genZeroRiskAssertions :: ContractTerms -> Assertion -> Contract -> Contract
    genZeroRiskAssertions terms@ContractTermsPoly {dayCountConvention = Just dcc, ..} NpvAssertionAgainstZeroRiskBond {..} continue =
      let cfs = genProjectedCashflows rf terms

          dateToYearFraction :: LocalTime -> Double
          dateToYearFraction dt = O._y dcc statusDate dt maturityDate

          dateToDiscountFactor dt = (1 O.- zeroRiskInterest) ** dateToYearFraction dt

          accumulateAndDiscount :: Value Observation -> (CashFlow, Integer) -> Value Observation
          accumulateAndDiscount acc (cf, t) =
            let discountFactor = dateToDiscountFactor $ cashCalculationDay cf
                sign x = if amount cf < 0.0 then NegValue x else x
             in constnt discountFactor O.* (sign $ useval "payoff" t) O.+ acc

          npv = foldl accumulateAndDiscount (constnt 0) (zip cfs [1 ..])
       in Assert (ValueLT (constnt expectedNpv) npv) continue
    genZeroRiskAssertions _ _ c = c
