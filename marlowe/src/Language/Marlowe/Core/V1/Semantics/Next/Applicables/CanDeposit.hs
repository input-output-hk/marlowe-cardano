
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Core.V1.Semantics.Next.Applicables.CanDeposit
  ( CanDeposit(..)
  ) where

import Data.Aeson (FromJSON(parseJSON), KeyValue((.=)), ToJSON(toJSON), Value(Object), object, (.:))
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Party, Token)
import Language.Marlowe.Pretty (Pretty(..))


import Language.Marlowe.Core.V1.Semantics.Next.Indexed (CaseIndex(CaseIndex), Indexed(..))
import Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation (IsMerkleizedContinuation)
import Prelude

data CanDeposit = CanDeposit Party AccountId Token Integer IsMerkleizedContinuation
  deriving stock (Show,Eq,Ord,Generic)
  deriving anyclass (Pretty)

instance FromJSON (Indexed CanDeposit) where
  parseJSON (Object v)
    =  Indexed
         <$>  (CaseIndex <$> v .: "case_index")
         <*>  (CanDeposit
                <$> v .: "party"
                <*> v .: "into_account"
                <*> v .: "of_token"
                <*> v .: "can_deposit"
                <*> v .: "is_merkleized_continuation")
  parseJSON _ = fail "CanDeposit must be either an object"

instance ToJSON (Indexed CanDeposit) where
  toJSON (Indexed caseIndex (CanDeposit party accountId token quantity isMerkleizedContinuation)) = object
      [ "party" .= party
      , "can_deposit" .= quantity
      , "of_token" .= token
      , "into_account" .= accountId
      , "case_index" .= caseIndex
      , "is_merkleized_continuation" .= isMerkleizedContinuation
      ]
