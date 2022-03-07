
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}


module Language.Marlowe.CLI.Test.Types (
  MarloweTests(..)
, RoleName
, InstanceNickname
, ScriptTest(..)
, PabTest(..)
, ScriptOperation(..)
, PabOperation(..)
, PabAccess(..)
, PabState(PabState)
, psFaucetKey
, psFaucetAddress
, psBurnAddress
, psPassphrase
, psWallets
, psAppInstances
, WalletInfo(..)
, AppInstanceInfo(..)
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


type RoleName = String


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
    CreateWallet
    {
      poOwner :: RoleName
    }
  | FundWallet
    {
      poOwner :: RoleName
    , poValue :: Value
    }
  | ReturnFunds
    {
      poOwner     :: RoleName
    , poInstances :: [RoleName]
    }
  | CheckFunds
    {
      poOwner       :: RoleName
    , poValue       :: Value
    , poMaximumFees :: Lovelace
    , poInstances   :: [RoleName]
    }
  -- TODO: Also support checking funds at script addresses.
  | ActivateApp
    {
      poOwner    :: RoleName
    , poInstance :: InstanceNickname
    }
  | CallCreate
    {
      poInstance :: InstanceNickname
    , poOwners   :: [RoleName]
    , poContract :: Contract
    }
  | AwaitCreate
    {
      poInstance :: InstanceNickname
    }
  | CallApplyInputs
    {
      poInstance :: InstanceNickname
    , poInputs   :: [MarloweClientInput]
    , poTimes    :: Maybe TimeInterval
    }
  | AwaitApplyInputs
    {
      poInstance :: InstanceNickname
    }
  | CallAuto
    {
      poInstance     :: InstanceNickname
    , poOwner        :: RoleName
    , poAbsoluteTime :: POSIXTime
    }
  | AwaitAuto
    {
      poInstance :: InstanceNickname
    }
  | CallRedeem
    {
      poInstance :: InstanceNickname
    , poOwner    :: RoleName
    }
  | AwaitRedeem
    {
      poInstance :: InstanceNickname
    }
  | CallClose
    {
      poInstance :: InstanceNickname
    }
  | AwaitClose
    {
      poInstance :: InstanceNickname
    }
  | Follow
    {
      poInstance      :: InstanceNickname
    , poOtherInstance :: InstanceNickname
    }
  | Stop
    {
      poInstance :: InstanceNickname
    }
  | PrintState
  | PrintWallet
    {
      poOwner :: RoleName
    }
  | WaitFor
    {
      poRelativeTime :: DiffMilliSeconds
    }
  | WaitUntil
    {
      poAbsoluteTime :: POSIXTime
    }
  | Timeout
    {
      poTimeoutSeconds :: Int
    , poOperation      :: PabOperation
    }
  | ShouldFail
    {
      poOperations :: [PabOperation]
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


data PabAccess =
  PabAccess
  {
    client          :: PabClient MarloweContract WalletId             -- ^ The PAB client.
  , runWallet       :: forall a. ClientM a -> IO (Either CliError a)  -- ^ The HTTP runner for the wallet.
  , runApi          :: forall a. ClientM a -> IO (Either CliError a)  -- ^ The HTTP runner for the PAB.
  , runWs           :: WsRunner IO ()                                 -- ^ The Websockets runner.
  , localConnection :: LocalNodeConnectInfo CardanoMode
}


data PabState =
  PabState
  {
    _psFaucetKey     :: SomePaymentSigningKey
  , _psFaucetAddress :: AddressAny
  , _psBurnAddress   :: AddressAny
  , _psPassphrase    :: Passphrase "raw"
  , _psWallets       :: M.Map RoleName WalletInfo
  , _psAppInstances  :: M.Map InstanceNickname AppInstanceInfo
  }
    deriving (Show)


data WalletInfo =
  WalletInfo
  {
    wiWalletId   :: W.WalletId
  , wiWalletId'  :: WalletId
  , wiAddress    :: AddressAny
  , wiPubKeyHash :: PubKeyHash
  }
    deriving (Eq, Show)


data AppInstanceInfo =
  AppInstanceInfo
  {
    aiInstance :: ContractInstanceId
  , aiChannel  :: Chan MarloweContractState
  , aiParams   :: Maybe MarloweParams
  }
    deriving (Eq)

instance Show AppInstanceInfo where
  show AppInstanceInfo{..} =  "AppInstanceInfo {aiInstance = "
                           <> show aiInstance
                           <> ", aiParams = "
                           <> show aiParams
                           <> "}"


makeLenses ''PabState
