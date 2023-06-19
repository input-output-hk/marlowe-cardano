
{-# LANGUAGE DataKinds #-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}


{-# LANGUAGE MultiParamTypeClasses #-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

{-# LANGUAGE RankNTypes #-}


module Language.Marlowe.Core.V1.Semantics.Next.Indexed
  ( CaseIndex(..)
  , Indexed(..)
  , caseIndexed
  , getCaseIndex
  , getIndexedValue
  , sameIndexedValue
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types ()

import Data.Bifunctor (Bifunctor(..))
import Data.List.Index (indexed)
import Deriving.Aeson (Generic)
import Language.Marlowe.Pretty (Pretty(..))
import Prelude


-- | Index of an applicable input matching its derived action index in a When [Case Action]
newtype CaseIndex = CaseIndex {unIndex ::Integer}
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)
    deriving newtype (FromJSON,ToJSON)

data Indexed a
  = Indexed CaseIndex a
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)

getCaseIndex :: Indexed a -> CaseIndex
getCaseIndex (Indexed c _) = c

getIndexedValue :: Indexed a -> a
getIndexedValue (Indexed _ a) = a

sameIndexedValue :: Eq a => Indexed a -> Indexed a -> Bool
sameIndexedValue a b = getIndexedValue a == getIndexedValue b

caseIndexed :: [a] -> [(CaseIndex,a)]
caseIndexed xs = first (CaseIndex . fromIntegral) <$> indexed  xs
