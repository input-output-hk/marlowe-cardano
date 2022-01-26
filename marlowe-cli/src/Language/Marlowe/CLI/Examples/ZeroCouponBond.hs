-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Zero-coupon bond contract.
--
-----------------------------------------------------------------------------


{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Examples.ZeroCouponBond (
-- * Contract
  makeZeroCouponBond
) where


import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (Action (..), Case (..), Contract (..), Party (..), Payee (..), State (..),
                                        Token (..), Value (..))
import Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Slot (Slot)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | A swap contract.
makeZeroCouponBond :: Integer      -- ^ Lovelace that the lender contributes to the initial state.
                   -> Party        -- ^ The lender.
                   -> Party        -- ^ The borrower.
                   -> Integer      -- ^ The principal.
                   -> Integer      -- ^ The interest.
                   -> Slot         -- ^ The lending deadline.
                   -> Slot         -- ^ The payback deadline.
                   -> MarloweData  -- ^ Swap contract and initial state.
makeZeroCouponBond minAda lender borrower principal interest lendingDeadline paybackDeadline =
  let
    ada = Token adaSymbol adaToken
    marloweState =
      State
      {
        accounts    = AM.singleton (lender, Token adaSymbol adaToken) minAda
      , choices     = AM.empty
      , boundValues = AM.empty
      , minSlot     = 1
      }
    principal' = Constant principal
    interest' = Constant interest
    marloweContract =
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
  in
    MarloweData{..}
