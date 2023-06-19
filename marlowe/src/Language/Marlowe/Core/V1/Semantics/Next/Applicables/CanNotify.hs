{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}


module Language.Marlowe.Core.V1.Semantics.Next.Applicables.CanNotify
  ( CanNotify(..)
  ) where

import Data.Aeson (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Pretty (Pretty(..))

import Language.Marlowe.Core.V1.Semantics.Next.Indexed (CaseIndex(CaseIndex), Indexed(..))
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Prelude


newtype CanNotify = CanNotify IsMerkleizedContinuation
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)


instance FromJSON (Indexed CanNotify) where
  parseJSON (Object v) =  Indexed <$> (CaseIndex <$> v .: "case_index")
                                  <*> (CanNotify <$> v .: "is_merkleized_continuation")
  parseJSON _ = fail "CanDeposit must be an object with 2 fields \"case_index\" and \"is_merkleized_continuation\""

instance ToJSON (Indexed CanNotify) where
  toJSON (Indexed caseIndex (CanNotify isMerkleizedContinuation))
    = object
      [ "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation]
