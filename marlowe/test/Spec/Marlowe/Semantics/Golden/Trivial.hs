-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Reference golden output for the Trivial contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Semantics.Golden.Trivial (
-- * Contracts
  contract
-- * Test cases
, valids
, invalids
) where


import Data.String (IsString(..))
import Language.Marlowe.Core.V1.Semantics (Payment(Payment), TransactionInput(..), TransactionOutput(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( AccountId
  , Action(Deposit, Notify)
  , Case(Case)
  , Contract(Close, Pay, When)
  , Input(NormalInput)
  , InputContent(IDeposit, INotify)
  , Observation(TrueObs)
  , Party(PK, Role)
  , Payee(Party)
  , State(State, accounts, boundValues, choices, minTime)
  , Token(Token)
  , Value(Constant)
  )
import Plutus.V2.Ledger.Api (POSIXTime(..), Value(..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


-- | The Pangram contract.
contract :: Contract
contract =
  let
    party = PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3"
    ada = Token "" ""
    deposit = 40_000_000
    withdrawal = 10_000_000
    timeout = POSIXTime 4000
  in
  When
    [
      Case (Deposit party party ada (Constant deposit))
        $ When
          [
            Case (Notify TrueObs)
              $ Pay party (Party party) ada (Constant withdrawal)
              $ When
                [
                  Case (Notify TrueObs)
                  Close
                ]
                timeout
                Close
          ]
          (timeout - 1000)
          Close
    ]
    (timeout - 2000)
    Close


-- | A wrapper to assist parsing of test cases.
newtype Map k v = Map {unMap :: [(k, v)]}


-- | A function to assist parsing of test cases.
toAM :: Map k v -> AM.Map k v
toAM = AM.fromList . unMap


-- An orphan instance to support parsing of test cases.
instance IsString AccountId where
  fromString ('P' : 'K' : x) = PK $ fromString x
  fromString x               = Role $ fromString x


-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 3000},POSIXTime {getPOSIXTime = 3000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",40000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 3000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 4000},POSIXTime {getPOSIXTime = 4000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]


-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 1000},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000001)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 3000},POSIXTime {getPOSIXTime = 3000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",40000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 3000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 5000},POSIXTime {getPOSIXTime = 5000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") "Z" (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (PK "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]
