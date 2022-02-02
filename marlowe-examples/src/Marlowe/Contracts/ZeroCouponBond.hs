{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.ZeroCouponBond where

import Language.Marlowe
import Marlowe.Contracts.Common

discountedPrice, notionalPrice :: Value Observation
discountedPrice = Constant 90
notionalPrice = Constant 100

initialExchange, maturityExchangeTimeout :: Timeout
initialExchange = Slot 10
maturityExchangeTimeout = 100

zeroCouponBond ::
     Party
  -> Party
  -> Contract
zeroCouponBond investor issuer =
    transfer initialExchange investor issuer discountedPrice
  $ transfer maturityExchangeTimeout issuer investor notionalPrice
    Close

