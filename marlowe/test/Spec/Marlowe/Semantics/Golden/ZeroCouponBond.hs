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


{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Semantics.Golden.ZeroCouponBond (
-- * Contracts
  contract
-- * Test cases
, valids
, invalids
) where


import Data.String (IsString(..))
import Language.Marlowe.Core.V1.Semantics (Payment(Payment), TransactionInput(..), TransactionOutput(..))
import Language.Marlowe.Core.V1.Semantics.Types
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


-- An orphan instance to support parsing of test cases.
instance IsString AccountId where
  fromString ('P' : 'K' : x) = PK $ fromString x
  fromString x               = Role $ fromString x


-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 1000},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Lender" "Lender" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Lender" (Party "Borrower") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Lender" "Lender" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Borrower" "Borrower" (Token "" "") 105000000)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Lender" (Party "Borrower") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Borrower" (Party "Lender") (Value (toAM $ Map [("", toAM $ Map [("",105000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]


-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 100},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Lender" "Lender" (Token "" "") 100000001)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Lender" (Party "Borrower") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Lender" "Borrower" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Borrower" "Borrower" (Token "" "") 105000000)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Lender" (Party "Borrower") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Borrower" (Party "Lender") (Value (toAM $ Map [("", toAM $ Map [("",105000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]
