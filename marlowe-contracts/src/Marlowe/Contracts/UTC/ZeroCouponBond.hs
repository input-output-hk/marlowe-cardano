module Marlowe.Contracts.UTC.ZeroCouponBond
  where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Extended.V1
import Marlowe.Contracts.UTC.Common
import Marlowe.Contracts.ZeroCouponBond as C

zeroCouponBond ::
     Party    -- ^ Investor
  -> Party    -- ^ Issuer
  -> UTCTime  -- ^ Initial Fixing
  -> UTCTime  -- ^ Maturity
  -> Value    -- ^ Discounted value
  -> Value    -- ^ Face value
  -> Token    -- ^ Token
  -> Contract -- ^ Continuation
  -> Contract -- ^ Zero-Coupon Bond Contract
zeroCouponBond investor issuer fixing maturity =
  C.zeroCouponBond investor issuer (toTimeout fixing) (toTimeout maturity)

