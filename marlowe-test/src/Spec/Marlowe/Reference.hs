-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Function to generate all valid transactions for contracts in JSON files.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Spec.Marlowe.Reference
  ( -- * Types
    ReferencePath(..)
  , ReferenceTransaction(..)
    -- * Testing
  , processContract
  ) where


import Control.Monad.Except (ExceptT(..), lift, throwError)
import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict, encodeFile)
import Data.Bifunctor (first)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics (TransactionInput, TransactionOutput(..), computeTransaction)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Party(Role), State(..), Token(..))
import Language.Marlowe.FindInputs (getAllInputs)
import Plutus.V2.Ledger.Api (POSIXTime)

import qualified PlutusTx.AssocMap as AM (empty, singleton)


data ReferencePath =
  ReferencePath
  {
    contract :: Contract
  , state :: State
  , transactions :: [ReferenceTransaction]
  }
    deriving (Generic, Show)

instance FromJSON ReferencePath

instance ToJSON ReferencePath


data ReferenceTransaction =
  ReferenceTransaction
  {
    input :: TransactionInput
  , output :: TransactionOutput
  }
    deriving (Generic, Show)

instance FromJSON ReferenceTransaction

instance ToJSON ReferenceTransaction


processContract
  :: FilePath
  -> FilePath
  -> ExceptT String IO ()
processContract contractFile pathsFile =
  do
    contract <- ExceptT $ first show <$> eitherDecodeFileStrict contractFile
    traces <- ExceptT $ first show <$> getAllInputs contract
    paths <- runTransactions contract `mapM` traces
    lift $ encodeFile pathsFile paths


runTransactions
  :: Contract
  -> (POSIXTime, [TransactionInput])
  -> ExceptT String IO ReferencePath
runTransactions contract (startTime, inputs) =
  do
    let
      state = makeState startTime
    transactions <- runTransaction contract state inputs
    pure ReferencePath{..}


runTransaction
  :: Contract
  -> State
  -> [TransactionInput]
  -> ExceptT String IO [ReferenceTransaction]
runTransaction _ _ [] = pure []
runTransaction contract state (input : inputs) =
  case computeTransaction input state contract of
    Error err -> throwError $ show err
    output@TransactionOutput{..} -> (ReferenceTransaction{..} :) <$> runTransaction txOutContract txOutState inputs


makeState
  :: POSIXTime
  -> State
makeState minTime =
  let
    accounts = AM.singleton (Role "", Token "" "") 30_000_000  -- Note that 30 ada exceeds min-UTxO for current protocol parameters.
    choices = AM.empty
    boundValues = AM.empty
  in
    State{..}
