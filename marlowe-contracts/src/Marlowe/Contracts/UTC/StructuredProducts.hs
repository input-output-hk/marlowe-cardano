module Marlowe.Contracts.UTC.StructuredProducts where

import Data.Time.Clock (UTCTime)
import Language.Marlowe.Core.V1.Semantics.Types
import Marlowe.Contracts.StructuredProducts as C
import Marlowe.Contracts.UTC.Common

reverseConvertible
  :: Party
  -- ^ Investor
  -> UTCTime
  -- ^ Initial fixing
  -> UTCTime
  -- ^ Maturity
  -> UTCTime
  -- ^ Settlement date
  -> Maybe ChoiceId
  -- ^ Price feed for the underlying
  -> Token
  -- ^ Currency
  -> Token
  -- ^ Underlying
  -> Value Observation
  -- ^ Strike
  -> Value Observation
  -- ^ Ratio
  -> Value Observation
  -- ^ Issue Price
  -> Contract
  -- ^ Reverse Convertible Contract
reverseConvertible investor fixing maturity settlement =
  C.reverseConvertible investor (toTimeout fixing) (toTimeout maturity) (toTimeout settlement)

barrierReverseConvertible
  :: Party
  -- ^ Investor
  -> UTCTime
  -- ^ Initial fixing
  -> UTCTime
  -- ^ Maturity
  -> UTCTime
  -- ^ Settlement date
  -> [UTCTime]
  -- ^ Barrier observation dates
  -> Maybe ChoiceId
  -- ^ Price feed for the underlying
  -> Token
  -- ^ Currency
  -> Token
  -- ^ Underlying
  -> Value Observation
  -- ^ Strike
  -> Value Observation
  -- ^ Barrier
  -> Value Observation
  -- ^ Ratio
  -> Value Observation
  -- ^ Issue Price
  -> Contract
  -- ^ Reverse Convertible Contract
barrierReverseConvertible investor fixing maturity settlement observationDates =
  C.barrierReverseConvertible
    investor
    (toTimeout fixing)
    (toTimeout maturity)
    (toTimeout settlement)
    (map toTimeout observationDates)
