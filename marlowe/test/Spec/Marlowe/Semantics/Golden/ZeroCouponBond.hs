-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Reference golden output for the Zero Coupon Bond contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Spec.Marlowe.Semantics.Golden.ZeroCouponBond
  ( -- * Contracts
    contract
    -- * Test cases
  , invalids
  , valids
  ) where


import Language.Marlowe.Core.V1.Semantics (Payment(Payment), TransactionInput(..), TransactionOutput(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Deposit)
  , Case(Case)
  , Contract(Close, Pay, When)
  , Input(NormalInput)
  , InputContent(IDeposit)
  , Party(Role)
  , Payee(Party)
  , State(State, accounts, boundValues, choices, minTime)
  , Token(Token)
  , Value(AddValue, Constant)
  )
import Plutus.V2.Ledger.Api (POSIXTime(..), Value(..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


-- | The Pangram contract.
contract :: Contract
contract =
  let
    lender = Role "Lender"
    borrower = Role "Borrower"
    ada = Token "" ""
    principal' = Constant 100_000_000
    interest' = Constant 5_000_000
    lendingDeadline = 1000
    paybackDeadline = 2000
  in
    When
      [
        Case (Deposit lender lender ada principal')
          $ Pay lender (Party borrower) ada principal'
          $ When
            [
              Case (Deposit borrower borrower ada (principal' `AddValue` interest'))
                $ Pay borrower (Party lender) ada (principal' `AddValue` interest')
                Close
            ]
            paybackDeadline
            Close
      ]
      lendingDeadline
      Close


-- | A wrapper to assist parsing of test cases.
newtype Map k v = Map {unMap :: [(k, v)]}


-- | A function to assist parsing of test cases.
toAM :: Map k v -> AM.Map k v
toAM = AM.fromList . unMap


-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 1000},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Lender") (Role "Lender") (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Lender") (Party (Role "Borrower")) (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Lender") (Role "Lender") (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Borrower") (Role "Borrower") (Token "" "") 105000000)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Lender") (Party (Role "Borrower")) (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment (Role "Borrower") (Party (Role "Lender")) (Value (toAM $ Map [("", toAM $ Map [("",105000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]


-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 100},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Lender") (Role "Lender") (Token "" "") 100000001)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Lender") (Party (Role "Borrower")) (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Lender") (Role "Borrower") (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Borrower") (Role "Borrower") (Token "" "") 105000000)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Lender") (Party (Role "Borrower")) (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment (Role "Borrower") (Party (Role "Lender")) (Value (toAM $ Map [("", toAM $ Map [("",105000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]
