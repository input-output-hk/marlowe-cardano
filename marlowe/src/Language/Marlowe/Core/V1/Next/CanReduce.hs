{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Core.V1.Next.CanReduce
  ( CanReduce(..)
  , tryReduce
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..))
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Core.V1.Semantics
  (ReduceResult(ContractQuiescent, RRAmbiguousTimeIntervalError), reduceContractUntilQuiescent)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Environment, State)
import Language.Marlowe.Pretty (Pretty(..))
import Prelude

newtype CanReduce = CanReduce { unCanReduce :: Bool}
    deriving stock (Show,Eq,Ord,Generic)
    deriving newtype (FromJSON,ToJSON,Pretty)


tryReduce :: Environment -> State -> Contract -> Either () (CanReduce,State,Contract)
tryReduce environment state
  = (\case
      ContractQuiescent isReduced _ _ newState newContract -> Right (CanReduce isReduced ,newState,newContract)
      RRAmbiguousTimeIntervalError -> Left ())
    . reduceContractUntilQuiescent environment state

