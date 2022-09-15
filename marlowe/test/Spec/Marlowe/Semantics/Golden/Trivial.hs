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


module Spec.Marlowe.Semantics.Golden.Trivial
  ( -- * Contracts
    contract
    -- * Test cases
  , invalids
  , valids
  ) where


import Language.Marlowe.Core.V1.Semantics (Payment(Payment), TransactionInput(..), TransactionOutput(..))
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(Deposit, Notify)
  , Case(Case)
  , Contract(Close, Pay, When)
  , Input(NormalInput)
  , InputContent(IDeposit, INotify)
  , Observation(TrueObs)
  , Party(Address, Role)
  , Payee(Party)
  , State(State, accounts, boundValues, choices, minTime)
  , Token(Token)
  , Value(Constant)
  )
import Plutus.V2.Ledger.Api (Credential(PubKeyCredential), POSIXTime(..), PubKeyHash, Value(..))

import qualified Plutus.V2.Ledger.Api as Ledger (Address(..))
import qualified PlutusTx.AssocMap as AM (Map, fromList)


-- | Create a party from a public key hash.
pk :: PubKeyHash -> Party
pk = Address . flip Ledger.Address Nothing . PubKeyCredential


-- | The Pangram contract.
contract :: Contract
contract =
  let
    party = pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3"
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


-- | A list of test cases and results that should succeed, generated from `Language.Marlowe.FindInputs.getAllInputs`.
valids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
valids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 2000},POSIXTime {getPOSIXTime = 2000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 3000},POSIXTime {getPOSIXTime = 3000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",40000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 3000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 4000},POSIXTime {getPOSIXTime = 4000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]


-- | A list of test cases and results that should fail.
invalids :: [(POSIXTime, [TransactionInput], TransactionOutput)]
invalids =
  [
    (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 1000},POSIXTime {getPOSIXTime = 1000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 2000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000001)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 3000},POSIXTime {getPOSIXTime = 3000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",40000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 3000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 5000},POSIXTime {getPOSIXTime = 5000}), txInputs = []}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 4000}}, txOutContract = Close})
  , (POSIXTime {getPOSIXTime = 0}, [TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput (IDeposit (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Role "Z") (Token "" "") 40000000)]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]},TransactionInput {txInterval = (POSIXTime {getPOSIXTime = 0},POSIXTime {getPOSIXTime = 0}), txInputs = [NormalInput INotify]}], TransactionOutput {txOutWarnings = [], txOutPayments = [Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",10000000)])])),Payment (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3") (Party (pk "6f5462cc2372a8c93d99f004fcdee671d010dd2d0fcaa8c38e90b9a3")) (Value(toAM $ Map [("", toAM $ Map [("",30000000)])]))], txOutState = State {accounts = toAM $ Map {unMap = []}, choices = toAM $ Map {unMap = []}, boundValues = toAM $ Map {unMap = []}, minTime = POSIXTime {getPOSIXTime = 0}}, txOutContract = Close})
  ]
