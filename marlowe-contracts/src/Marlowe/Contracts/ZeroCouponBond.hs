{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.ZeroCouponBond where

import Language.Marlowe
import Marlowe.Contracts.Common

zeroCouponBond ::
     Timeout
  -> Timeout
  -> Value Observation
  -> Value Observation
  -> Party
  -> Party
  -> Contract
zeroCouponBond fixing maturity discounted notional investor issuer =
    transfer investor issuer (ada, discounted) fixing
  $ deposit issuer investor (ada, notional) maturity Close
    Close
