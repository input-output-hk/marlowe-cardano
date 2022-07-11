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


module Language.Marlowe.CLI.Test.Types (
-- * Type
  MarloweTests(..)
, ScriptTest(..)
, ScriptOperation(..)
) where


import Cardano.Api (AddressAny, CardanoMode, LocalNodeConnectInfo, Lovelace, NetworkId, Value)
import Cardano.Wallet.Primitive.AddressDerivation (Passphrase)
import Control.Applicative ((<|>))
import Control.Concurrent.Chan (Chan)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.:), (.=))
import GHC.Generics (Generic)
-- import Language.Marlowe.CLI.PAB (WsRunner)
import Language.Marlowe.CLI.Types (CliError, SomePaymentSigningKey)
import Language.Marlowe.Core.V1.Semantics (MarloweParams)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State, TimeInterval)
-- import Plutus.PAB.Webserver.Client (PabClient)
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Time (DiffMilliSeconds, POSIXTime)
import Servant.Client (BaseUrl, ClientM)

import qualified Cardano.Wallet.Primitive.Types as W (WalletId)
import Control.Lens.Combinators (Lens')
import Control.Lens.Lens (lens)
import qualified Data.Aeson as A (Value (..))
import qualified Data.Map.Strict as M (Map)
import Data.Maybe (fromMaybe)
import Options.Applicative (optional)


-- | Configuration for a set of Marlowe tests.
data MarloweTests a =
    -- | Test contracts on-chain.
    ScriptTests
    {
      network       :: NetworkId   -- ^ The network ID, if any.
    , socketPath    :: FilePath    -- ^ The path to the node socket.
    , faucetFile    :: FilePath    -- ^ The file containing the faucet's signing key.
    , faucetAddress :: AddressAny  -- ^ The faucet address.
    , burnAddress   :: AddressAny  -- ^ The address to which to send unneeded native tokens.
    , tests         :: [a]         -- ^ Input for the tests.
    }
    deriving stock (Eq, Generic, Show)


-- | An on-chain test of the Marlowe contract and payout validators.
data ScriptTest =
  ScriptTest
  {
    stTestName         :: String             -- ^ The name of the test.
  , stSlotLength       :: Integer            -- ^ The slot length, in milliseconds.
  , stSlotZeroOffset   :: Integer            -- ^ The effective POSIX time of slot zero, in milliseconds.
  , stInitialContract  :: Contract           -- ^ The contract.
  , stInitialState     :: State              -- ^ The the contract's initial state.
  , stScriptOperations :: [ScriptOperation]  -- ^ The sequence of test operations.
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | On-chain test operations for the Marlowe contract and payout validators.
data ScriptOperation =
  ScriptOperation
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


