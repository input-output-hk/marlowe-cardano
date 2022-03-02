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
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}


module Language.Marlowe.CLI.Examples.ZeroCouponBond (
-- * Contract
  makeZeroCouponBond
) where


import Language.Marlowe.Extended (Contract (..), Party (..), Timeout, Value (..), toCore)
import Language.Marlowe.Semantics (MarloweData (..))
import Language.Marlowe.SemanticsTypes (State (..), Token (..))
import Ledger.Ada (adaSymbol, adaToken)
import Marlowe.Contracts.ZeroCouponBond (zeroCouponBond)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


-- | A swap contract.
makeZeroCouponBond :: Integer      -- ^ Lovelace that the lender contributes to the initial state.
                   -> Party        -- ^ The lender.
                   -> Party        -- ^ The borrower.
                   -> Integer      -- ^ The principal.
                   -> Integer      -- ^ The interest.
                   -> Timeout      -- ^ The lending deadline.
                   -> Timeout      -- ^ The payback deadline.
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
      , minTime     = 1
      }
    principal' = Constant principal
    interest' = Constant interest
    Just marloweContract = toCore $
      zeroCouponBond
        lender
        borrower
        lendingDeadline
        paybackDeadline
        principal'
        (principal' `AddValue` interest')
        ada
        Close
  in
    MarloweData{..}
