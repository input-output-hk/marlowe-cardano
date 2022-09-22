-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Reference golden output for the Escrow contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}


module Spec.Marlowe.Semantics.Golden.Escrow
  ( -- * Contracts
    contract
    -- * Test cases
  , invalids
  , valids
  ) where


import Language.Marlowe.Core.V1.Semantics (Payment(Payment), TransactionInput(..), TransactionOutput(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Choice, Deposit)
  , Bound(Bound)
  , Case(Case)
  , ChoiceId(ChoiceId)
  , Contract(Close, Pay, When)
  , Input(NormalInput)
  , InputContent(IChoice, IDeposit)
  , Party(Role)
  , Payee(Account, Party)
  , State(State, accounts, boundValues, choices, minTime)
  , Token(Token)
  , Value(Constant)
  )
import Language.Marlowe.Util ()
import Plutus.V2.Ledger.Api (POSIXTime(..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


seller :: Party
seller = "Seller"

buyer :: Party
buyer = "Buyer"

mediator :: Party
mediator = Role "Mediator"

ada :: Token
ada = Token "" ""


-- | The Escrow contract.
contract :: Contract
contract =
  let
    price = Constant 100_000_000
    paymentDeadline   = POSIXTime 10
    complaintDeadline = POSIXTime 20
    disputeDeadline   = POSIXTime 30
    mediationDeadline = POSIXTime 40
  in
    When
      [
        Case (Deposit seller buyer ada price)
          $ When
            [
              Case (Choice (ChoiceId "Everything is alright" buyer) [Bound 0 0])
                Close
            , Case (Choice (ChoiceId "Report problem" buyer) [Bound 1 1])
                $ Pay seller (Account buyer) ada price
                $ When
                  [
                    Case (Choice (ChoiceId "Confirm problem" seller) [Bound 1 1])
                      Close
                  , Case (Choice (ChoiceId "Dispute problem" seller) [Bound 0 0])
                    $ When
                      [
                        Case (Choice (ChoiceId "Dismiss claim" mediator) [Bound 0 0])
                          $ Pay buyer (Account seller) ada price
                          Close
                      , Case (Choice (ChoiceId "Confirm claim" mediator) [Bound 1 1])
                          Close
                      ]
                      mediationDeadline
                      Close
                  ]
                  disputeDeadline
                Close
            ]
            complaintDeadline
            Close
      ]
      paymentDeadline
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
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 10},POSIXTime {getPOSIXTime = 10}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 10}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 20},POSIXTime {getPOSIXTime = 20}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Party seller) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 20}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Everything is alright" buyer) 0)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Party seller) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Everything is alright" buyer,0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 30},POSIXTime {getPOSIXTime = 30}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 30}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Confirm problem" seller) 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1),(ChoiceId "Confirm problem" seller,1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" seller) 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 40},POSIXTime {getPOSIXTime = 40}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1),(ChoiceId "Dispute problem" seller,0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 40}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" seller) 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dismiss claim" mediator) 0)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Account seller) (Token "" "") 100000000,Payment seller (Party seller) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1),(ChoiceId "Dispute problem" seller,0),(ChoiceId "Dismiss claim" mediator,0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" seller) 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Confirm claim" mediator) 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1),(ChoiceId "Dispute problem" seller,0),(ChoiceId "Confirm claim" mediator,1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]

-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 10}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 10}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller seller ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 20},POSIXTime {getPOSIXTime = 20}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Party seller) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 20}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000001)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Everything is alright" buyer) 0)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Party seller) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Everything is alright" buyer,0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" seller) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 30},POSIXTime {getPOSIXTime = 30}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 30}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit seller buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Confirm problem" seller) 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1),(ChoiceId "Confirm problem" seller,1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit buyer buyer ada 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" buyer) 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" seller) 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 40},POSIXTime {getPOSIXTime = 40}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment seller (Account buyer) (Token "" "") 100000000,Payment buyer (Party buyer) (Token "" "") 100000000], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" buyer,1),(ChoiceId "Dispute problem" seller,0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 40}}, txOutContract = Close})
  ]
