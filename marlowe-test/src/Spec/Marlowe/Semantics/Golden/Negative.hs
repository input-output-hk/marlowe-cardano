-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Reference golden output for a contract with negative deposits.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Spec.Marlowe.Semantics.Golden.Negative
  ( -- * Contracts
    contract
    -- * Test cases
  , invalids
  , valids
  ) where


import Language.Marlowe.Core.V1.Semantics
  (Payment(Payment), TransactionInput(..), TransactionOutput(..), TransactionWarning(TransactionNonPositiveDeposit))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Choice, Deposit, Notify)
  , Bound(..)
  , Case(Case)
  , ChoiceId(..)
  , Contract(Close, Let, When)
  , Input(NormalInput)
  , InputContent(IChoice, IDeposit, INotify)
  , Observation(ValueEQ, ValueGT, ValueLT)
  , Party
  , Payee(Party)
  , State(State, accounts, boundValues, choices, minTime)
  , Token(..)
  , Value(ChoiceValue, Constant)
  )
import Language.Marlowe.Util ()
import Plutus.V2.Ledger.Api (POSIXTime(..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


party :: Party
party = "Party"

counterparty :: Party
counterparty = "Counterparty"

ada :: Token
ada = Token "" ""


-- | The Zero-Coupon Bond contract.
contract :: Contract
contract =
  let
    initial = Constant 2_000_000
    choice = ChoiceId "Deposit" party
    value = ChoiceValue choice
    initialDeadline = 1000
    choiceDeadline = 2000
    notifyDeadline = 3000
    depositDeadline = 4000
  in
    When
      [
        Case (Deposit counterparty counterparty ada initial)
          $ When
              [
                Case (Choice choice [Bound (-1) 1])
                  $ When
                    [
                      Case (Deposit party counterparty ada value)
                        Close
                    , Case (Notify $ ValueGT value $ Constant 0)
                       $ Let "Positive" (Constant 1)
                       $ When
                         [
                           Case (Deposit party counterparty ada value)
                             Close
                         ]
                         depositDeadline
                         Close
                    , Case (Notify $ ValueEQ value $ Constant 0)
                       $ Let "Neutral" (Constant 1)
                       $ When
                         [
                           Case (Deposit party party ada value)
                             Close
                         ]
                         depositDeadline
                         Close
                    , Case (Notify $ ValueLT value $ Constant 0)
                       $ Let "Negative" (Constant 1)
                       $ When
                         [
                           Case (Deposit counterparty party ada value)
                             Close
                         ]
                         depositDeadline
                         Close
                    ]
                    notifyDeadline
                    Close
              ]
              choiceDeadline
              Close
      ]
      initialDeadline
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
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 3000},POSIXTime {getPOSIXTime = 3000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 3000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Party" "Counterparty" (Token "" "") 0)]}], TransactionOutput {txOutWarnings = [TransactionNonPositiveDeposit "Counterparty" "Party" (Token "" "") 0], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 4000},POSIXTime {getPOSIXTime = 4000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",1)]}, boundValues = toAM $ Map {unMap = [("Positive",1)]}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Party" "Counterparty" (Token "" "") 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000,Payment "Party" (Party "Party") (Token "" "") 1], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",1)]}, boundValues = toAM $ Map {unMap = [("Positive",1)]}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 4000},POSIXTime {getPOSIXTime = 4000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",0)]}, boundValues = toAM $ Map {unMap = [("Neutral",1)]}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Party" "Party" (Token "" "") 0)]}], TransactionOutput {txOutWarnings = [TransactionNonPositiveDeposit "Party" "Party" (Token "" "") 0], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",0)]}, boundValues = toAM $ Map {unMap = [("Neutral",1)]}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") (-1))]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 4000},POSIXTime {getPOSIXTime = 4000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",-1)]}, boundValues = toAM $ Map {unMap = [("Negative",1)]}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Counterparty" (Token "" "") 2000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Deposit" "Party") (-1))]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Counterparty" "Party" (Token "" "") (-1))]}], TransactionOutput {txOutWarnings = [TransactionNonPositiveDeposit "Party" "Counterparty" (Token "" "") (-1)], txOutPayments = [Payment "Counterparty" (Party "Counterparty") (Token "" "") 2000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Deposit" "Party",-1)]}, boundValues = toAM $ Map {unMap = [("Negative",1)]}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]


-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
  ]
