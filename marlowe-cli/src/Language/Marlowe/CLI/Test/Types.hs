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
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


module Language.Marlowe.CLI.Test.Types (
-- * Type
  MarloweTests(..)
, RoleName
, InstanceNickname
, ScriptTest(..)
, PabTest(..)
, ScriptOperation(..)
, PabOperation(..)
, PabAccess(..)
, PabState(PabState)
, WalletInfo(..)
, AppInstanceInfo(..)
-- * Lenses
, psFaucetKey
, psFaucetAddress
, psBurnAddress
, psPassphrase
, psWallets
, psAppInstances
) where


import Cardano.Api (AddressAny, CardanoMode, LocalNodeConnectInfo, Lovelace, NetworkId, Value)
import Cardano.Wallet.Primitive.AddressDerivation (Passphrase)
import Control.Concurrent.Chan (Chan)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.PAB (WsRunner)
import Language.Marlowe.CLI.Types (CliError, SomePaymentSigningKey)
import Language.Marlowe.Client (MarloweClientInput, MarloweContractState)
import Language.Marlowe.Contract (MarloweContract)
import Language.Marlowe.Semantics (MarloweParams)
import Language.Marlowe.SemanticsTypes (Contract, State, TimeInterval)
import Plutus.Contract (ContractInstanceId)
import Plutus.PAB.Webserver.Client (PabClient)
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Time (DiffMilliSeconds, POSIXTime)
import Servant.Client (BaseUrl, ClientM)
import Wallet.Emulator.Wallet (WalletId)

import qualified Cardano.Wallet.Primitive.Types as W (WalletId)
import qualified Data.Map.Strict as M (Map)


-- | Configuration for a set of Marlowe tests.
data MarloweTests a =
    -- | Test contracts on-chain.
    ScriptTests
    {
      network        :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath     :: FilePath         -- ^ The path to the node socket.
    , slotLength     :: Integer          -- ^ The slot length, in milliseconds.
    , slotZeroOffset :: Integer          -- ^ The effective POSIX time of slot zero, in milliseconds.
    , faucetFile     :: FilePath         -- ^ The file containing the faucet's signing key.
    , faucetAddress  :: AddressAny       -- ^ The faucet address.
    , burnAddress    :: AddressAny       -- ^ The address to which to send unneeded native tokens.
    , tests          :: [a]              -- ^ Input for the tests.
    }
    -- | Test contracts using the Marlowe PAB.
  | PabTests
    {
      network       :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath    :: FilePath         -- ^ The path to the node socket.
    , walletUrl     :: BaseUrl          -- ^ The URL for Cardano Wallet.
    , pabUrl        :: BaseUrl          -- ^ The URL for the Marlowe PAB.
    , faucetFile    :: FilePath         -- ^ The file containing the faucet's signing key.
    , faucetAddress :: AddressAny       -- ^ The faucet address.
    , burnAddress   :: AddressAny       -- ^ The address to which to send unneeded native tokens.
    , passphrase    :: String           -- ^ The passphrase for the Marlowe PAB.
    , tests         :: [a]              -- ^ Input for the tests.
    }
    deriving stock (Eq, Generic, Show)


-- | The name of a role.
type RoleName = String


-- | A nickname for a PAB contract instance.
type InstanceNickname = String


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


-- | An on- and off-chain test of the Marlowe contracts, via the Marlowe PAB.
data PabTest =
  PabTest
  {
    ptTestName      :: String          -- ^ The name of the test.
  , ptPabOperations :: [PabOperation]  -- ^ The sequence of test operations.
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | On-chain test operations for the Marlowe contract and payout validators.
data ScriptOperation =
  ScriptOperation
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | On- and off-chain test operations for Marlowe contracts, via the Marlowe PAB.
data PabOperation =
    -- | Create a wallet.
    CreateWallet
    {
      poOwner :: RoleName  -- ^ The name of the wallet's owner.
    }
    -- | Fund a wallet.
  | FundWallet
    {
      poOwner :: RoleName  -- ^ The name of the wallet's owner.
    , poValue :: Value     -- ^ The value to add to the wallet.
    }
    -- | Return funds from a wallet.
  | ReturnFunds
    {
      poOwner     :: RoleName    -- ^ The name of the wallet's owner.
    , poInstances :: [RoleName]  -- ^ The instances for which to role tokens should be burnt.
    }
    -- | Check the funds in a wallet.
  | CheckFunds
    {
      poOwner       :: RoleName    -- ^ The name of the wallet's owner.
    , poValue       :: Value       -- ^ The value the wallet should contain.
    , poMaximumFees :: Lovelace    -- ^ The allowable maximum fee that should have been paid.
    , poInstances   :: [RoleName]  -- ^ The role tokens that should be in the wallet.
    }
  -- TODO: Also support checking funds at script addresses.
    -- | Activate the Marlowe `WalletApp` PAB contract.
  | ActivateApp
    {
      poOwner    :: RoleName          -- ^ The name of the wallet's owner.
    , poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Call the "create" endpoint of `WalletApp`.
  | CallCreate
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    , poOwners   :: [RoleName]        -- ^ The names of roles in the contract.
    , poContract :: Contract          -- ^ The Marlowe contract to be created.
    }
    -- | Wait for confirmation of a call to the "create" endpoint.
  | AwaitCreate
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Call the "apply-inputs" endpoint of `WalletApp`.
  | CallApplyInputs
    {
      poInstance :: InstanceNickname      -- ^ The nickname of the PAB contract instance.
    , poInputs   :: [MarloweClientInput]  -- ^ The inputs to the Marlowe contract.
    , poTimes    :: Maybe TimeInterval    -- ^ The time interval for the transaction.
    }
    -- | Wait for confirmation of a call to the "apply-inputs" endpoint.
  | AwaitApplyInputs
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Call the "auto" endpoint of `WalletApp`.
  | CallAuto
    {
      poInstance     :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    , poOwner        :: RoleName          -- ^ The name of the wallet's owner.
    , poAbsoluteTime :: POSIXTime         -- ^ The maximum time to operate until.
    }
    -- | Wait for confirmation of a call to the "auto" endpoint.
  | AwaitAuto
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Call the "redeem" endpoint of `WalletApp`.
  | CallRedeem
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    , poOwner    :: RoleName          -- ^ The name of the wallet's owner.
    }
    -- | Wait for confirmation of a call to the "redeem" endpoint.
  | AwaitRedeem
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Call the "close" endpoint of `WalletApp`.
  | CallClose
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Wait for confirmation of a call to the "close" endpoint.
  | AwaitClose
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Follow the same Marlowe contract as another PAB contract instance.
  | Follow
    {
      poInstance      :: InstanceNickname  -- ^ The nickname of the PAB contract instance doing the following.
    , poOtherInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance that is being followed.
    }
    -- | Stop a PAB instance.
  | Stop
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Print the state of the PAB.
  | PrintState
    -- | Print the contents of a wallet.
  | PrintWallet
    {
      poOwner :: RoleName  -- ^ The name of the wallet's owner.
    }
    -- | Print a comment.
  | Comment
    {
      poComment :: String  -- ^ The textual comment.
    }
    -- | Wait for a specified amount of time.
  | WaitFor
    {
      poRelativeTime :: DiffMilliSeconds  -- ^ The number of milliseconds to wait.
    }
    -- | Wait until a specified time.
  | WaitUntil
    {
      poAbsoluteTime :: POSIXTime  -- ^ The time until which to wait.
    }
    -- | Fail if a test operation doesn't complete in time.
  | Timeout
    {
      poTimeoutSeconds :: Int           -- ^ The number of seconds to wait.
    , poOperation      :: PabOperation  -- ^ The PAB operation to wait for.
    }
    -- | Execute test operations that should fail.
  | ShouldFail
    {
      poOperations :: [PabOperation]  -- ^ The sequence of PAB operations that should fail.
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


-- | Access to the PAB APIs.
data PabAccess =
  PabAccess
  {
    client          :: PabClient MarloweContract WalletId             -- ^ The PAB client.
  , runWallet       :: forall a. ClientM a -> IO (Either CliError a)  -- ^ The HTTP runner for the wallet.
  , runApi          :: forall a. ClientM a -> IO (Either CliError a)  -- ^ The HTTP runner for the PAB.
  , runWs           :: WsRunner IO ()                                 -- ^ The Websockets runner.
  , localConnection :: LocalNodeConnectInfo CardanoMode               -- ^ The connection to the local node.
}


-- | Wallet information.
data WalletInfo =
  WalletInfo
  {
    wiWalletId   :: W.WalletId  -- ^ One wallet identifier.
  , wiWalletId'  :: WalletId    -- ^ Another wallet identifier
  , wiAddress    :: AddressAny  -- ^ The first wallet address.
  , wiPubKeyHash :: PubKeyHash  -- ^ The public key hash of the first wallet address.
  }
    deriving (Eq, Show)


-- | PAB instance information.
data AppInstanceInfo =
  AppInstanceInfo
  {
    aiInstance :: ContractInstanceId         -- ^ The PAB contract instance identifier.
  , aiChannel  :: Chan MarloweContractState  -- ^ The channel for state changes reported by the PAB.
  , aiParams   :: Maybe MarloweParams        -- ^ The Marlowe contract parameters, if any, for the instance.
  }
    deriving (Eq)

instance Show AppInstanceInfo where
  show AppInstanceInfo{..} =  "AppInstanceInfo {aiInstance = "
                           <> show aiInstance
                           <> ", aiParams = "
                           <> show aiParams
                           <> "}"


-- | The state of the PAB test framework.
data PabState =
  PabState
  {
    _psFaucetKey     :: SomePaymentSigningKey                   -- ^ The key to the faucet.
  , _psFaucetAddress :: AddressAny                              -- ^ The address of the faucet.
  , _psBurnAddress   :: AddressAny                              -- ^ The address for burning role tokens.
  , _psPassphrase    :: Passphrase "raw"                        -- ^ The wallet passphrase.
  , _psWallets       :: M.Map RoleName WalletInfo               -- ^ The wallets being managed.
  , _psAppInstances  :: M.Map InstanceNickname AppInstanceInfo  -- ^ The PAB contract instances being managed.
  }
    deriving (Show)

makeLenses ''PabState
