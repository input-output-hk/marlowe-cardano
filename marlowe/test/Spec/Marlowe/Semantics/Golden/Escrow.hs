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


{-# LANGUAGE NumericUnderscores   #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}


module Spec.Marlowe.Semantics.Golden.Escrow (
-- * Contracts
  contract
-- * Test cases
, valids
, invalids
) where


import Data.String (IsString (..))
import Language.Marlowe.Core.V1.Semantics (Payment (Payment), TransactionInput (..), TransactionOutput (..))
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Action (Choice, Deposit), Bound (Bound), Case (Case),
                                                 ChoiceId (ChoiceId), Contract (Close, Pay, When), Input (NormalInput),
                                                 InputContent (IChoice, IDeposit), Party (PK, Role),
                                                 Payee (Account, Party),
                                                 State (State, accounts, boundValues, choices, minTime), Token (Token),
                                                 Value (Constant))
import Plutus.V1.Ledger.Api (POSIXTime (..), Value (..))

import qualified PlutusTx.AssocMap as AM (Map, fromList)


-- | The Pangram contract.
contract :: Contract
contract =
  let
    seller = Role "Seller"
    buyer = Role "Buyer"
    mediator = Role "Mediator"
    ada = Token "" ""
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


-- An orphan instance to support parsing of test cases.
instance IsString AccountId where
  fromString ('P' : 'K' : x) = PK $ fromString x
  fromString x               = Role $ fromString x


-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 10},POSIXTime {getPOSIXTime = 10}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 10}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 20},POSIXTime {getPOSIXTime = 20}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Party "Seller") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 20}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Everything is alright" "Buyer") 0)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Party "Seller") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Everything is alright" "Buyer",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 30},POSIXTime {getPOSIXTime = 30}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 30}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Confirm problem" "Seller") 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1),(ChoiceId "Confirm problem" "Seller",1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" "Seller") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 40},POSIXTime {getPOSIXTime = 40}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1),(ChoiceId "Dispute problem" "Seller",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 40}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" "Seller") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dismiss claim" "Mediator") 0)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Account "Seller") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Seller" (Party "Seller") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1),(ChoiceId "Dispute problem" "Seller",0),(ChoiceId "Dismiss claim" "Mediator",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" "Seller") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Confirm claim" "Mediator") 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1),(ChoiceId "Dispute problem" "Seller",0),(ChoiceId "Confirm claim" "Mediator",1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]

-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 10}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 10}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Seller" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 20},POSIXTime {getPOSIXTime = 20}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Party "Seller") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 20}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000001)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Everything is alright" "Buyer") 0)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Party "Seller") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Everything is alright" "Buyer",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Seller") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 30},POSIXTime {getPOSIXTime = 30}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 30}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Seller" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Confirm problem" "Seller") 1)]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1),(ChoiceId "Confirm problem" "Seller",1)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit "Buyer" "Buyer" (Token "" "") 100000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Report problem" "Buyer") 1)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IChoice (ChoiceId "Dispute problem" "Seller") 0)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 40},POSIXTime {getPOSIXTime = 40}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment "Seller" (Account "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])])),Payment "Buyer" (Party "Buyer") (Value (toAM $ Map [("", toAM $ Map [("",100000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = [(ChoiceId "Report problem" "Buyer",1),(ChoiceId "Dispute problem" "Seller",0)]}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 40}}, txOutContract = Close})
  ]
