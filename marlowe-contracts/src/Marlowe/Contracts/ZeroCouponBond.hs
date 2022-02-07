{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.ZeroCouponBond where

import Language.Marlowe
import Marlowe.Contracts.Common

-- |"A zero-coupon bond is a debt security that does not pay interest
-- but instead trades at a deep discount, rendering a profit at maturity,
-- when the bond is redeemed for its full face value." -- investopedia
--
-- Note: Counter party risk
zeroCouponBond ::
     Timeout           -- ^ Initial Fixing
  -> Timeout           -- ^ Maturity
  -> Value Observation -- ^ Discounted
  -> Value Observation -- ^ Face value
  -> Token             -- ^ Token
  -> Party             -- ^ Investor
  -> Party             -- ^ Issuer
  -> Contract          -- ^ Zero-Coupon Bond Contract
zeroCouponBond fixing maturity discounted notional token investor issuer =
    transfer investor issuer (token, discounted) fixing Close
  $ deposit investor issuer (token, notional) maturity Close
    Close
