-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for testing Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}


module Language.Marlowe.CLI.Test.Types (
-- * Type
  MarloweTests(..)
, ScriptTest(..)
, ScriptOperation(..)
, TransactionNickname
, ScriptContract(..)

-- * Lenses
, psFaucetKey
, psFaucetAddress
, psBurnAddress
, psPassphrase
, psWallets
, psAppInstances
, psFollowerInstances
, psCompanionInstances
, prComparison
, prRetry
, comparisonJSON
) where


import Cardano.Api (AddressInEra, NetworkId)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types (AccountId, Contract, Input)
import Ledger.Orphans ()
import Plutus.V1.Ledger.Time (POSIXTime)


-- | Configuration for a set of Marlowe tests.
data MarloweTests era a =
    -- | Test contracts on-chain.
    ScriptTests
    {
      network       :: NetworkId   -- ^ The network ID, if any.
    , socketPath    :: FilePath    -- ^ The path to the node socket.
    , faucetFile    :: FilePath    -- ^ The file containing the faucet's signing key.
    , faucetAddress :: AddressInEra era  -- ^ The faucet address.
    , burnAddress   :: AddressInEra era -- ^ The address to which to send unneeded native tokens.
    , tests         :: [a]         -- ^ Input for the tests.
    }
    deriving stock (Eq, Generic, Show)

type TransactionNickname = String

-- | An on-chain test of the Marlowe contract and payout validators.
data ScriptTest =
  ScriptTest
  {
    stTestName         :: String             -- ^ The name of the test.
  -- , stSlotLength       :: Integer            -- ^ The slot length, in milliseconds.
  -- , stSlotZeroOffset   :: Integer            -- ^ The effective POSIX time of slot zero, in milliseconds.
  -- , stInitialContract  :: Contract           -- ^ The contract.
  -- , stInitialState     :: State              -- ^ The the contract's initial state.
  , stScriptOperations :: [ScriptOperation]  -- ^ The sequence of test operations.
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | An on- and off-chain test of the Marlowe contracts, via the Marlowe PAB.
data PabTest =
  PabTest
  {
    ptTestName      :: String          -- ^ The name of the test.
  , ptPabOperations :: [PabOperation]  -- ^ The sequence of test operations.
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


type TransactionNickname = String

data ScriptContract = InlineContract Contract | TemplateContract TemplateCommand
    deriving stock (Eq, Generic, Show)

instance ToJSON ScriptContract where
    toJSON (InlineContract c)                 = Aeson.object [("inline", toJSON c)]
    toJSON (TemplateContract templateCommand) = Aeson.object [("template", toJSON templateCommand)]

instance FromJSON ScriptContract where
    parseJSON json = case json of
      Aeson.Object (Data.HashMap.Strict.toList -> [("inline", contractJson)]) -> do
        parsedContract <- parseJSON contractJson
        pure $ InlineContract parsedContract
      Aeson.Object (Data.HashMap.Strict.toList -> [("template", templateCommandJson)]) -> do
        parsedTemplateCommand <- parseJSON templateCommandJson
        pure $ TemplateContract parsedTemplateCommand
      _ -> fail "Expected object with a single field of either `inline` or `template`"


-- | On-chain test operations for the Marlowe contract and payout validators.
data ScriptOperation =
  Initialize
    {
      soOwner        :: AccountId             -- ^ The name of the wallet's owner.
    , soMinAda       :: Integer
    , soTransaction  :: TransactionNickname   -- ^ The name of the wallet's owner.
    , soRoleCurrency :: Text                  -- ^ We derive
    , soContract     :: ScriptContract        -- ^ The Marlowe contract to be created.
    -- | FIXME: No *JSON instances for this
    -- , soStake :: Maybe StakeAddressReference
    }
  | Prepare
    {
      soOwner       :: AccountId -- ^ The name of the wallet's owner.
    , soTransaction :: TransactionNickname   -- ^ The name of the wallet's owner.
    , soInputs      :: [Input]
    , soMinimumTime :: POSIXTime
    , soMaximumTime :: POSIXTime
    }
  | Fail
    {
      soFailureMessage :: String
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
