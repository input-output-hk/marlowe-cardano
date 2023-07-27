module Marlowe.Contracts.UTC.ZeroCouponBond where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Core.V1.Semantics.Types
import Marlowe.Contracts.UTC.Common
import Marlowe.Contracts.ZeroCouponBond as C

zeroCouponBond
  :: Party
  -- ^ Investor
  -> Party
  -- ^ Issuer
  -> UTCTime
  -- ^ Initial Fixing
  -> UTCTime
  -- ^ Maturity
  -> Value Observation
  -- ^ Discounted value
  -> Value Observation
  -- ^ Face value
  -> Token
  -- ^ Token
  -> Contract
  -- ^ Continuation
  -> Contract
  -- ^ Zero-Coupon Bond Contract
zeroCouponBond investor issuer fixing maturity =
  C.zeroCouponBond investor issuer (toTimeout fixing) (toTimeout maturity)
