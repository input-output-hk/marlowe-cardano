{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TupleSections    #-}

-- | = Generator for ACTUS contracts
-- Given ACTUS contract terms a Marlowe contract is generated.
module Actus.Marlowe
  ( constant,
    letval,
    useval,
    genContract,
    genContract',
    ContractTermsMarlowe,
    RiskFactorsMarlowe
  )
where

import Actus.Core (genProjectedCashflows)
import Actus.Domain (ActusOps (..), CashFlow (..), EventType (..), TermValidationError (..), marloweFixedPoint)
import Actus.Marlowe.Instance (ContractTermsMarlowe, RiskFactorsMarlowe, reduceContract, reduceValue)
import Actus.Model (validateTerms)
import Data.List as L (foldl')
import Data.String (IsString (fromString))
import Data.Time (LocalTime (..), UTCTime (UTCTime), timeOfDayToTime)
import Data.Time.Clock.System (SystemTime (MkSystemTime), utcToSystemTime)
import Data.Validation (Validation (..))
import Language.Marlowe (Action (..), Case (..), Contract (..), Observation (..), POSIXTime (..), Party (..),
                         Payee (..), Value (..), ValueId (ValueId), ada)
import Ledger.Value (TokenName (TokenName))

-- | 'genContract' validatates the applicabilty of the contract terms in order
--  to genereate a Marlowe contract with risk factors observed at a given point
--  in time
genContract ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  -- | ACTUS contract terms
  ContractTermsMarlowe ->
  -- | Marlowe contract or applicabilty errors
  Validation [TermValidationError] Contract
genContract rf = fmap (genContract' rf) . validateTerms

-- | Same as 'getContract', but does not validate the applicabilty of the contract
-- terms.
genContract' ::
  -- | Risk factors per event and time
  (EventType -> LocalTime -> RiskFactorsMarlowe) ->
  -- | ACTUS contract terms
  ContractTermsMarlowe ->
  -- | Marlowe contract
  Contract
genContract' rf ct =
  let cfs = genProjectedCashflows rf ct
   in foldl' (flip gen) Close $ reverse cfs
  where
    gen :: CashFlow (Value Observation) -> Contract -> Contract
    gen CashFlow {..} cont =
      let t = POSIXTime $ timeToSlotNumber cashPaymentDay
          a = reduceValue $ DivValue amount (Constant marloweFixedPoint)
       in reduceContract $
            If
              (_zero `ValueLT` a)
              ( invoice
                  "party"
                  "counterparty"
                  a
                  t
                  cont
              )
              ( If
                  (a `ValueLT` _zero)
                  ( invoice
                      "counterparty"
                      "party"
                      (NegValue a)
                      t
                      cont
                  )
                  cont
              )
    invoice :: String -> String -> Value Observation -> POSIXTime -> Contract -> Contract
    invoice from to amount timeout continue =
      let party = Role $ TokenName $ fromString from
          counterparty = Role $ TokenName $ fromString to
       in When
            [ Case
                (Deposit party party ada amount)
                ( Pay
                    party
                    (Party counterparty)
                    ada
                    amount
                    continue
                )
            ]
            timeout
            Close

useval :: String -> Integer -> Value Observation
useval name t = UseValue $ ValueId $ fromString $ name ++ "_" ++ show t

letval :: String -> Integer -> Value Observation -> Contract -> Contract
letval name t = Let $ ValueId $ fromString $ name ++ "_" ++ show t

toMarloweFixedPoint :: Double -> Integer
toMarloweFixedPoint = round <$> (fromIntegral marloweFixedPoint *)

constant :: Double -> Value Observation
constant = Constant . toMarloweFixedPoint

cardanoEpochStart :: Integer
cardanoEpochStart = 100

timeToSlotNumber :: LocalTime -> Integer
timeToSlotNumber LocalTime {..} =
  let (MkSystemTime secs _) = utcToSystemTime (UTCTime localDay (timeOfDayToTime localTimeOfDay))
   in fromIntegral secs - cardanoEpochStart
