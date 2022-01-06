-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Swap contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Examples.Swap (
-- * Contract
  makeSwapContract
) where


import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (Action (..), Case (..), Contract (..), Party (..), Payee (..), State (..),
                                        Token (..), Value (..))
import Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Slot (Slot)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | A swap contract.
makeSwapContract :: Integer      -- ^ Lovelace that the first party contributes to the initial state.
                 -> Party        -- ^ First party.
                 -> Token        -- ^ First party's token.
                 -> Integer      -- ^ Amount of first party's token.
                 -> Slot         -- ^ Timeout for first party's deposit.
                 -> Party        -- ^ Second party.
                 -> Token        -- ^ Second party's token.
                 -> Integer      -- ^ Amount of second party's token.
                 -> Slot         -- ^ Timeout for second party's deposit
                 -> MarloweData  -- ^ Swap contract and initial state.
makeSwapContract minAda aParty aToken aAmount aTimeout bParty bToken bAmount bTimeout =
  let
    marloweState =
      State
      {
        accounts    = AM.singleton (aParty, Token adaSymbol adaToken) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minSlot     = 1
      }
    marloweContract =
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
  in
    MarloweData{..}
