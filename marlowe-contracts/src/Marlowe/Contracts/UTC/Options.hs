module Marlowe.Contracts.UTC.Options
  ( -- * Options
    C.BarrierType(..)
  , C.ExerciseType(..)
  , C.OptionType(..)
  , option
    -- * Option Strategies
    -- ** Fully collateralized
  , coveredCall
    -- ** Partially collateralized
  , callSpread
    -- ** Not collateralized
  , barrierOption
  , straddle
  , strangle
  ) where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Extended.V1
import qualified Marlowe.Contracts.Options as C
import Marlowe.Contracts.UTC.Common

option ::
     C.ExerciseType -- ^ Exercise Type
  -> C.OptionType   -- ^ Type of Option
  -> Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> (Token, Value) -- ^ Underlying
  -> (Token, Value) -- ^ Strike
  -> UTCTime        -- ^ Expiry
  -> UTCTime        -- ^ Settlement date
  -> Contract       -- ^ Option Contract
option exerciseType optionType buyer seller priceFeed asset strike expiry settlement =
  C.option exerciseType optionType buyer seller priceFeed asset strike (toTimeout expiry) (toTimeout settlement)

coveredCall ::
     Party          -- ^ Issuer of the covered Call
  -> Party          -- ^ Counter-party
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency)
  -> Value          -- ^ Amount of underlying tokens per contract
  -> UTCTime        -- ^ Issue date
  -> UTCTime        -- ^ Maturity
  -> UTCTime        -- ^ Settlement date
  -> Contract       -- ^ Covered Call Contract
coveredCall issuer counterparty priceFeed currency underlying strike ratio issue maturity settlement =
  C.coveredCall issuer counterparty priceFeed currency underlying strike ratio (toTimeout issue) (toTimeout maturity) (toTimeout settlement)

callSpread ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency) for the long position
  -> Value          -- ^ Strike price (in currency) for the short position
  -> Value          -- ^ Amount of underlying tokens per contract
  -> UTCTime        -- ^ Maturity
  -> UTCTime        -- ^ Settlement date
  -> Contract       -- ^ Call Spread Contract
callSpread buyer seller priceFeed currency underlying strike1 strike2 ratio maturity settlement =
  C.callSpread buyer seller priceFeed currency underlying strike1 strike2 ratio (toTimeout maturity) (toTimeout settlement)

straddle ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Ratio
  -> Value          -- ^ Strike
  -> UTCTime        -- ^ Maturity
  -> UTCTime        -- ^ Settlement date
  -> Contract       -- ^ Straddle Contract
straddle buyer seller priceFeed currency underlying ratio strike maturity settlement =
  C.straddle buyer seller priceFeed currency underlying ratio strike (toTimeout maturity) (toTimeout settlement)

strangle ::
     Party          -- ^ Buyer
  -> Party          -- ^ Seller
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Ratio
  -> Value          -- ^ Lower Strike
  -> Value          -- ^ Upper Strike
  -> UTCTime        -- ^ Maturity
  -> UTCTime        -- ^ Settlement date
  -> Contract       -- ^ Straddle Contract
strangle buyer seller priceFeed currency underlying ratio strike1 strike2 maturity settlement =
  C.strangle buyer seller priceFeed currency underlying ratio strike1 strike2 (toTimeout maturity) (toTimeout settlement)

barrierOption ::
     C.ExerciseType -- ^ Exercise Type
  -> C.OptionType   -- ^ Type of Option
  -> C.BarrierType  -- ^ Barrier Type
  -> Party          -- ^ Issuer of the covered Call
  -> Party          -- ^ Counter-party
  -> Maybe ChoiceId -- ^ Price feed for the underlying
  -> Token          -- ^ Currency
  -> Token          -- ^ Underlying
  -> Value          -- ^ Strike price (in currency)
  -> Value          -- ^ Amount of underlying tokens per contract
  -> Value          -- ^ Barrier
  -> [UTCTime]      -- ^ Barrier observation dates
  -> UTCTime        -- ^ Maturity
  -> UTCTime        -- ^ Settlement date
  -> Contract       -- ^ Covered Call Contract
barrierOption exerciseType optionType barrierType issuer counterparty priceFeed currency underlying strike ratio barrier observationDates maturity settlement =
  C.barrierOption exerciseType optionType barrierType issuer counterparty priceFeed currency underlying strike ratio barrier (map toTimeout observationDates) (toTimeout maturity) (toTimeout settlement)
