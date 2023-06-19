{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Core.V1.Semantics.Next.CanReduce
  ( AmbiguousIntervalProvided(..)
  , CanReduce(..)
  , tryReduce
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Core.V1.Semantics
  (ReduceResult(ContractQuiescent, RRAmbiguousTimeIntervalError), reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types (Contract(Close), Environment, State)
import Language.Marlowe.Pretty (Pretty(..))


import Prelude

data AmbiguousIntervalProvided = AmbiguousIntervalProvided
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)

newtype CanReduce = CanReduce { unCanReduce :: Bool}
    deriving stock (Show,Eq,Ord,Generic)
    deriving newtype (FromJSON,ToJSON,Pretty)


tryReduce :: Environment -> State -> Contract -> Either AmbiguousIntervalProvided (CanReduce,State,Contract)
tryReduce environment state
  = (\case
      ContractQuiescent _ _ _ newState Close -> Right (CanReduce True ,newState,Close)  -- Todo : Add an extra notion of Terminate (N.H)
      ContractQuiescent isReduced _ _ newState newContract -> Right (CanReduce isReduced ,newState,newContract)
      RRAmbiguousTimeIntervalError -> Left AmbiguousIntervalProvided)
    . reduceContractUntilQuiescent environment state

