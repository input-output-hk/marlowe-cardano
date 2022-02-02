{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.ReverseConvertible where

import Language.Marlowe
import Marlowe.Contracts.Options
import Marlowe.Contracts.ZeroCouponBond

-- |Reverse convertible note
reverseConvertible ::
     Party
  -> Party
  -> Timeout
  -> Timeout
  -> Timeout
  -> Token
  -> Token
  -> Value Observation
  -> Value Observation
  -> Contract
reverseConvertible investor issuer fixing maturity settlement currency underlying strike ratio =
  zcb `both` shortCall
  where
    zcb =
      zeroCouponBond
        fixing
        maturity
        strike -- FIXME: discounted
        ratio -- FIXME: notional
        investor
        issuer
    shortCall =
      option
        European
        Call
        investor
        issuer
        (currency, strike)
        (underlying, ratio)
        maturity
        settlement
