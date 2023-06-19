
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Core.V1.Semantics.Next
  ( Next(..)
  , next
  ) where

import Data.Aeson (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.Aeson.Types ()
import Deriving.Aeson (Generic)
import Prelude

import Language.Marlowe.Core.V1.Semantics.Types (Contract, Environment, State)
import Language.Marlowe.Pretty (Pretty(..))

import Language.Marlowe.Core.V1.Semantics.Next.Applicables (ApplicableInputs, mkApplicables)
import Language.Marlowe.Core.V1.Semantics.Next.CanReduce (AmbiguousIntervalProvided, CanReduce, tryReduce)

data Next = Next { canReduce :: CanReduce, applicables :: ApplicableInputs}
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)


-- | Describe for a given contract which inputs can be applied it can be reduced or not
next :: Environment ->  State  ->  Contract -> Either AmbiguousIntervalProvided Next
next environment state contract
  = do
    (canReduce,reducedState,reducedContract) <- tryReduce environment state contract
    Right
      $ Next
        canReduce
        (mkApplicables environment reducedState reducedContract)

instance FromJSON Next where
  parseJSON (Object v) = Next <$> v .: "can_reduce" <*> v .: "applicable_inputs"
  parseJSON _ = fail "Next must be an object with 2 fields \"can_reduce\" and \"applicable_inputs\""

instance ToJSON Next where
  toJSON Next {..} = object [ "can_reduce" .= canReduce, "applicable_inputs" .= applicables]

