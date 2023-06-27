{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Language.Marlowe.Core.V1.Next (
  Next (..),
  filterByParties,
  mkEnvironment,
  next,
) where

import Data.Aeson (FromJSON (parseJSON), KeyValue ((.=)), ToJSON (toJSON), Value (Object), object, (.:))
import Data.Aeson.Types ()
import Deriving.Aeson (Generic)
import Prelude

import Language.Marlowe.Core.V1.Semantics.Types (
  Contract,
  Environment (..),
  IntervalError (..),
  IntervalResult (..),
  Party,
  State,
 )
import Language.Marlowe.Pretty (Pretty (..))

import Data.Bifunctor (Bifunctor (first))
import Data.List.NonEmpty
import Data.Time (UTCTime)
import Language.Marlowe.Core.V1.Next.Applicables (ApplicableInputs, mkApplicables)
import qualified Language.Marlowe.Core.V1.Next.Applicables as Applicables
import Language.Marlowe.Core.V1.Next.CanReduce (CanReduce, tryReduce)
import Language.Marlowe.Core.V1.Semantics (fixInterval)
import Plutus.V1.Ledger.SlotConfig (utcTimeToPOSIXTime)

data Next = Next {canReduce :: CanReduce, applicables :: ApplicableInputs}
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Pretty)

filterByParties :: Maybe (NonEmpty Party) -> Next -> Next
filterByParties p Next{..} = Next{canReduce, applicables = Applicables.filterByParties p applicables}

-- | Describe for a given contract which inputs can be applied it can be reduced or not
next :: Environment -> State -> Contract -> Either IntervalError Next
next environment state contract =
  do
    case fixInterval (timeInterval environment) state of
      IntervalTrimmed environmentTrimmed adjustedState -> do
        (canReduce, reducedState, reducedContract) <-
          first
            (const . InvalidInterval . timeInterval $ environment)
            (tryReduce environmentTrimmed adjustedState contract)
        Right $
          Next
            canReduce
            (mkApplicables environment reducedState reducedContract)
      IntervalError e -> Left e

instance FromJSON Next where
  parseJSON (Object v) = Next <$> v .: "can_reduce" <*> v .: "applicable_inputs"
  parseJSON _ = fail "Next must be an object with 2 fields \"can_reduce\" and \"applicable_inputs\""

instance ToJSON Next where
  toJSON Next{..} = object ["can_reduce" .= canReduce, "applicable_inputs" .= applicables]

mkEnvironment :: UTCTime -> UTCTime -> Environment
mkEnvironment start end = Environment (utcTimeToPOSIXTime start, utcTimeToPOSIXTime end)
