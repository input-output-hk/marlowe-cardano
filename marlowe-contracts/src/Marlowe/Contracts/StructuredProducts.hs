{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.StructuredProducts where

import Language.Marlowe.Extended
import Marlowe.Contracts.Common
import Marlowe.Contracts.Options
import Marlowe.Contracts.ZeroCouponBond

-- = Structured Products

-- |== Reverse Convertible
-- A /Reverse Convertible/ is a coupon paying investment with payout at maturity of
-- the investment amount or an underlying instrument.
--
-- === Construction
-- A /Reverse Convertible/ is typically constructed by combining a /zero-coupon Bond/
-- with a short position of an /Option/ with the same maturity and the face value
-- of the Bond is equal to the strike of the Option - i.e. the short position of the
-- Option is collateralized by the face value of the Bond. The coupon payment is the
-- premium received by shorting the Option.
--
-- === Payoff
-- There are two possible scenarios at maturity, depending if the Option is exercised
-- or not.
--
-- ==== Option is not exercised
-- Example:
--
-- @
--                Investor         Bond           Option
--                                Provider      Counterparty
-- Initial            | -----------> | 1000         |
-- Fixing:        100 | <----------- + ------------ |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
-- Maturity:     1100 | <----------- |              |
--
-- @
-- At maturity the investor collects the face value of the bond together with premium
-- from selling the option, in the example: @1200@
--
-- ==== Option is exercised
-- Example:
--
-- @
--                Investor         Bond           Option
--                                Provider      Counterparty
-- Initial            | -----------> | 1000         |
-- Fixing:        100 | <----------- + ------------ |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
--                    |              |              |
-- Maturity:     1100 | <----------- |              |
--                    |              |              |*Exercise!
--                    | ------------ + -----------> | 1100
--         Underlying | <----------- + ------------ |
--
-- @
-- At maturity the investor gets the Underlying together with the premium
-- from selling the option, in the example: @Underlying + 100@
--
reverseConvertible ::
     Party   -- ^ Investor
  -> Timeout -- ^ Initial fixing
  -> Timeout -- ^ Maturity
  -> Timeout -- ^ Settlement date
  -> Token   -- ^ Currency
  -> Token   -- ^ Underlying
  -> Value   -- ^ Strike
  -> Value   -- ^ Ratio
  -> Value   -- ^ Issue Price
  -> Contract          -- ^ Reverse Convertible Contract
reverseConvertible investor fixing maturity settlement currency underlying strike ratio issuePrice =
  zcb `both` shortCall
  where
    zcb =
      zeroCouponBond
        investor
        (Role "BondProvider")
        fixing
        maturity
        issuePrice
        strike
        currency
        Close
    shortCall =
      option
        European
        Call
        (Role "OptionCounterparty")
        investor
        (currency, strike)
        (underlying, ratio)
        maturity
        settlement
