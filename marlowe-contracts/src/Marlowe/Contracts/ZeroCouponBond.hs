{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.ZeroCouponBond where

import Language.Marlowe.Extended
import Marlowe.Contracts.Common

-- |A zero-coupon bond is a debt security that does not pay interest.
-- The security is issued with a discount, at maturity it is redeemed
-- for its full face value.
zeroCouponBond ::
     Party    -- ^ Investor
  -> Party    -- ^ Issuer
  -> Timeout  -- ^ Initial Fixing
  -> Timeout  -- ^ Maturity
  -> Value    -- ^ Discounted value
  -> Value    -- ^ Face value
  -> Token    -- ^ Token
  -> Contract -- ^ Continuation
  -> Contract -- ^ Zero-Coupon Bond Contract
zeroCouponBond investor issuer fixing maturity discounted face token continuation =
    transfer investor issuer (token, discounted) fixing Close
  $ deposit investor issuer (token, face) maturity Close
    continuation
