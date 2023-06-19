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


module Language.Marlowe.Core.V1.Semantics.Next.IsMerkleizedContinuation
  ( IsMerkleizedContinuation(..)
  ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Types ()

import Deriving.Aeson (Generic)
import Language.Marlowe.Pretty (Pretty(..))
import Prelude

newtype IsMerkleizedContinuation = IsMerkleizedContinuation { unIsMerkleizedContinuation :: Bool}
    deriving stock (Show,Eq,Ord,Generic)
    deriving anyclass (Pretty)
    deriving newtype (FromJSON,ToJSON)
