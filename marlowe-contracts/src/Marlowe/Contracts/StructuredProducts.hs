{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.StructuredProducts where

import Language.Marlowe.Extended.V1
import Marlowe.Contracts.Common
import Marlowe.Contracts.Options
import Marlowe.Contracts.ZeroCouponBond

-- = Structured Products

-- | == Reverse Convertible
--  A /Reverse Convertible/ is a coupon paying investment with payout at maturity of
--  the investment amount or an underlying instrument.
--
--  === Construction
--  A /Reverse Convertible/ is typically constructed by combining a /zero-coupon Bond/
--  with a short position of a /Put-Option/ with the same maturity and the face value
--  of the Bond is equal to the strike of the Option - i.e. the short position of the
--  Option is collateralized by the face value of the Bond. The coupon payment is the
--  premium received by shorting the Option.
--
--  === Payoff
--  There are two possible scenarios at maturity, depending if the Option is exercised
--  or not.
--
--  ==== Option is not exercised
--  Example:
--
--  @
--                 Investor         Bond           Option
--                                 Provider      Counterparty
--  Initial            | -----------> | 1000         |
--  Fixing:        100 | <----------- + ------------ |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--  Maturity:     1100 | <----------- |              |
--
--  @
--  At maturity the investor collects the face value of the bond together with premium
--  from selling the option, in the example: @1200@
--
--  ==== Option is exercised
--  Example:
--
--  @
--                 Investor         Bond           Option
--                                 Provider      Counterparty
--  Initial            | -----------> | 1000         |
--  Fixing:        100 | <----------- + ------------ |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--                     |              |              |
--  Maturity:     1100 | <----------- |              |
--                     |              |              |*Exercise!
--                     | ------------ + -----------> | 1100
--          Underlying | <----------- + ------------ |
--
--  @
--  At maturity the investor gets the Underlying together with the premium
--  from selling the option, in the example: @Underlying + 100@
reverseConvertible
  :: Party
  -- ^ Investor
  -> Timeout
  -- ^ Initial fixing
  -> Timeout
  -- ^ Maturity
  -> Timeout
  -- ^ Settlement date
  -> Maybe ChoiceId
  -- ^ Price feed for the underlying
  -> Token
  -- ^ Currency
  -> Token
  -- ^ Underlying
  -> Value
  -- ^ Strike
  -> Value
  -- ^ Ratio
  -> Value
  -- ^ Issue Price
  -> Contract
  -- ^ Reverse Convertible Contract
reverseConvertible investor fixing maturity settlement priceFeed currency underlying strike ratio issuePrice =
  zcb `both` shortPut
  where
    zcb =
      zeroCouponBond'
        investor
        (Role "BondProvider")
        fixing
        maturity
        issuePrice
        strike
        currency
        Close
    shortPut =
      option
        European
        Put
        (Role "OptionCounterparty")
        investor
        priceFeed
        (underlying, ratio)
        (currency, strike)
        maturity
        settlement

-- | Similar to `reverseConvertible` constructed from a Bond in combination with a Down-And-In Put barrier option
barrierReverseConvertible
  :: Party
  -- ^ Investor
  -> Timeout
  -- ^ Initial fixing
  -> Timeout
  -- ^ Maturity
  -> Timeout
  -- ^ Settlement date
  -> [Timeout]
  -- ^ Barrier observation dates
  -> Maybe ChoiceId
  -- ^ Price feed for the underlying
  -> Token
  -- ^ Currency
  -> Token
  -- ^ Underlying
  -> Value
  -- ^ Strike
  -> Value
  -- ^ Barrier
  -> Value
  -- ^ Ratio
  -> Value
  -- ^ Issue Price
  -> Contract
  -- ^ Reverse Convertible Contract
barrierReverseConvertible investor fixing maturity settlement observationDates priceFeed currency underlying strike barrier ratio issuePrice =
  zcb `both` shortPut
  where
    zcb =
      zeroCouponBond'
        investor
        (Role "BondProvider")
        fixing
        maturity
        issuePrice
        strike
        currency
        Close
    shortPut =
      barrierOption
        European
        Put
        DownAndIn
        (Role "OptionCounterparty")
        investor
        priceFeed
        currency
        underlying
        strike
        ratio
        barrier
        observationDates
        maturity
        settlement
