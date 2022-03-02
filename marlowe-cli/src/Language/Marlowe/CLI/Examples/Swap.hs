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
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}

module Language.Marlowe.CLI.Examples.Swap (
-- * Contract
  makeSwapContract
) where


import Language.Marlowe.Extended (Contract (..), Party (..), Timeout, Value (..), toCore)
import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (State (..), Token (..))
import Ledger.Ada (adaSymbol, adaToken)
import Marlowe.Contracts.Swap (swap)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | A swap contract.
makeSwapContract :: Integer      -- ^ Lovelace that the first party contributes to the initial state.
                 -> Party        -- ^ First party.
                 -> Token        -- ^ First party's token.
                 -> Integer      -- ^ Amount of first party's token.
                 -> Timeout      -- ^ Timeout for first party's deposit.
                 -> Party        -- ^ Second party.
                 -> Token        -- ^ Second party's token.
                 -> Integer      -- ^ Amount of second party's token.
                 -> Timeout      -- ^ Timeout for second party's deposit
                 -> MarloweData  -- ^ Swap contract and initial state.
makeSwapContract minAda aParty aToken aAmount aTimeout bParty bToken bAmount bTimeout =
  let
    marloweState =
      State
      {
        accounts    = AM.singleton (aParty, Token adaSymbol adaToken) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minTime     = 1
      }
    Just marloweContract = toCore $
      swap
        aParty
        aToken
        (Constant aAmount)
        aTimeout
        bParty
        bToken
        (Constant bAmount)
        bTimeout
        Close
  in
    MarloweData{..}
