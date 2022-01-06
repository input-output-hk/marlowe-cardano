-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Trivial contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Examples.Trivial (
  -- * Contract
  makeTrivialContract
) where


import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (Action (..), Case (..), Contract (..), Observation (TrueObs), Party (..),
                                        Payee (..), State (..), Token (..), Value (..))
import Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Slot (Slot)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | A trivial contract, for testing.
makeTrivialContract :: Party        -- ^ The party providing the min-ADA.
                    -> Integer      -- ^ Lovelace in the initial state.
                    -> Slot         -- ^ The minimum slot.
                    -> Party        -- ^ The party.
                    -> Integer      -- ^ Lovelace in the deposit.
                    -> Integer      -- ^ Lovelace in the withdrawal.
                    -> Slot         -- ^ The timeout.
                    -> MarloweData  -- ^ The escrow contract and initial state.
makeTrivialContract bystander minAda minSlot party deposit withdrawal timeout =
  let
    ada = Token adaSymbol adaToken
    marloweState =
      State
      {
        accounts    = AM.singleton (bystander, ada) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minSlot     = minSlot
      }
    marloweContract =
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
              (timeout - 1)
              Close
        ]
        (timeout - 2)
        Close
  in
    MarloweData{..}
