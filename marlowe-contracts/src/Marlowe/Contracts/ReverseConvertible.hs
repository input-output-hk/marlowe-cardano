{-# LANGUAGE OverloadedStrings #-}
module Marlowe.Contracts.ReverseConvertible where

import Language.Marlowe
import Marlowe.Contracts.Options
import Marlowe.Contracts.ZeroCouponBond

-- = Structured Products

-- |A Reverse Convertible is a coupon paying investment with payout at maturity of
-- the investment amount or an underlying instrument.
--
-- A Reverse Convertible is typically constructed by combining a zero-coupon Bond
-- with a short position of an Option with the same maturity and the face value
-- of the bond is equal to the strike of the Option - i.e. the short position of the
-- Option is collateralized by the face value of the Bond. The coupon payment is the
-- premium received by shorting the Option.
--
-- Scenario: Option is not exercised
-- =================================
--
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
-- At maturity the investor collects the face value of the bond together with premium
-- from selling the option, in the example: 1200
--
-- Secenario: Option is exercised
-- ==============================
--
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
-- At maturity the investor gets the Underlying together with the premium
-- from selling the option, in the example: Underlying + 100
--
reverseConvertible ::
     Party             -- ^ Investor
  -> Timeout           -- ^ Initial fixing
  -> Timeout           -- ^ Maturity
  -> Timeout           -- ^ Settlement date
  -> Token             -- ^ Currency
  -> Token             -- ^ Underlying
  -> Value Observation -- ^ Strike
  -> Value Observation -- ^ Ratio
  -> Value Observation -- ^ Issue Price
  -> Contract          -- ^ Reverse Convertible Contract
reverseConvertible investor fixing maturity settlement currency underlying strike ratio issuePrice =
  zcb `both` shortCall
  where
    zcb =
      zeroCouponBond
        fixing
        maturity
        issuePrice
        strike
        currency
        investor
        (Role "BondProvider")
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
