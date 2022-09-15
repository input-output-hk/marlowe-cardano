-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Reference golden output for the Swap contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Spec.Marlowe.Semantics.Golden.Swap
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
  , Value(Constant)
  )
import Plutus.V2.Ledger.Api (CurrencySymbol, POSIXTime(..), Value(..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


aSymbol, bSymbol :: CurrencySymbol
aSymbol = "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8"
bSymbol = "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741"


-- | The Pangram contract.
contract :: Contract
contract =
  let
    aParty = Role "Party A"
    bParty = Role "Party B"
    aToken = Token aSymbol "Token A"
    bToken = Token bSymbol "Token B"
    aAmount = 300_000_000
    bAmount = 500_000_000
    aTimeout = 1000
    bTimeout = 2000
  in
    When
      [
        Case (Deposit aParty aParty aToken $ Constant aAmount)
          $ When
            [
              Case (Deposit bParty bParty bToken $ Constant bAmount)
              $ Pay aParty (Party bParty) aToken (Constant aAmount)
              $ Pay bParty (Party aParty) bToken (Constant bAmount)
              Close
            ]
            bTimeout
            Close
      ]
      aTimeout
      Close


-- | A wrapper to assist parsing of test cases.
newtype Map k v = Map {unMap :: [(k, v)]}


-- | A function to assist parsing of test cases.
toAM :: Map k v -> AM.Map k v
toAM = AM.fromList . unMap


-- | A list of Pangram test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 1000},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Party A") (Role "Party A") (Token "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8" "Token A") 300000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Party A") (Party (Role "Party A")) (Value(toAM $ Map [("13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8", toAM $ Map [("Token A",300000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Party A") (Role "Party A") (Token "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8" "Token A") 300000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Party B") (Role "Party B") (Token "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741" "Token B") 500000000)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Party A") (Party (Role "Party B")) (Value(toAM $ Map [("13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8", toAM $ Map [("Token A",300000000)])])),Payment (Role "Party B") (Party (Role "Party A")) (Value(toAM $ Map [("1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741", toAM $ Map [("Token B",500000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]


-- | A list of Pangram test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 900},POSIXTime {getPOSIXTime = 900}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 1000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Party A") (Role "Party A") (Token "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8" "Token A") 500000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Party A") (Party (Role "Party A")) (Value(toAM $ Map [("13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8", toAM $ Map [("Token A",300000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Party A") (Role "Party A") (Token "13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8" "Token A") 300000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (Role "Party A") (Role "Party B") (Token "1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741" "Token B") 500000000)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (Role "Party A") (Party (Role "Party B")) (Value(toAM $ Map [("13e78e78c233e131b0cbe4424225d338b7c5ac65e16df0a3e6c9d8f8", toAM $ Map [("Token A",300000000)])])),Payment (Role "Party B") (Party (Role "Party A")) (Value(toAM $ Map [("1b9af43b0eaafc42dfaefbbf4e71437af45454c7292a6b6606363741", toAM $ Map [("Token B",500000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]
