{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Helper to work with contracts in the testing context.
--
-----------------------------------------------------------------------------

module Language.Marlowe.CLI.Test.Contract
  where

import Data.Aeson (FromJSON, ToJSON)
import Data.String (IsString)
import GHC.Exts (IsString(fromString))
import GHC.Generics (Generic)

newtype ContractNickname = ContractNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
instance IsString ContractNickname where fromString = ContractNickname

