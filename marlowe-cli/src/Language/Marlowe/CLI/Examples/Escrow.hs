-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Escrow contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}


module Language.Marlowe.CLI.Examples.Escrow (
  -- * Contract
  makeEscrowContract
) where


import           Language.Marlowe.SemanticsTypes (Action (..), Bound (..), Case (..), ChoiceId (..), Contract (..),
                                                  Party (..), Payee (..), State (..), Token (..), Value (..))
import           Ledger.Ada                      (adaSymbol, adaToken)
import           Plutus.V1.Ledger.Slot           (Slot)

import qualified PlutusTx.AssocMap               as AM (empty, singleton)


-- | An escrow contract with mediation.
makeEscrowContract :: Integer            -- ^ Lovelace in the initial state.
                   -> Integer            -- ^ Price of the item, in lovelace.
                   -> Party              -- ^ The seller.
                   -> Party              -- ^ The buyer.
                   -> Party              -- ^ The mediator.
                   -> Slot               -- ^ The deadline for the buyer to pay.
                   -> Slot               -- ^ The deadline for the buyer to complain.
                   -> Slot               -- ^ The deadline for the seller to dispute a complaint.
                   -> Slot               -- ^ The deadline for the mediator to decide.
                   -> (Contract, State)  -- ^ The escrow contract and initial state.
makeEscrowContract minAda price seller buyer mediator paymentDeadline complaintDeadline disputeDeadline mediationDeadline =
  let
    ada = Token adaSymbol adaToken
    price' = Constant price
    marloweState =
      State
      {
        accounts    = AM.singleton (mediator, ada) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minSlot     = 1
      }
    marloweContract =
      When
        [
          Case (Deposit seller buyer ada price')
            $ When
              [
                Case (Choice (ChoiceId "Everything is alright" buyer) [Bound 0 0])
                  Close
              , Case (Choice (ChoiceId "Report problem" buyer) [Bound 1 1])
                  $ Pay seller (Account buyer) ada price'
                  $ When
                    [
                      Case (Choice (ChoiceId "Confirm problem" seller) [Bound 1 1])
                        Close
                    , Case (Choice (ChoiceId "Dispute problem" seller) [Bound 0 0])
                      $ When
                        [
                          Case (Choice (ChoiceId "Dismiss claim" mediator) [Bound 0 0])
                            $ Pay buyer (Account seller) ada price'
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
  in
    (marloweContract, marloweState)
