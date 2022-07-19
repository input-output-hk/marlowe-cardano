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
, FollowerInstanceInfo(..)
, PabResponseComparison(..)
, PabResponse(..)
, CompanionInstanceInfo(..)
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


import Cardano.Api (AddressAny, CardanoMode, LocalNodeConnectInfo, Lovelace, NetworkId, StakeAddressReference, Value)
import Cardano.Wallet.Primitive.AddressDerivation (Passphrase)
import Control.Applicative ((<|>))
import Control.Concurrent.Chan (Chan)
import Control.Lens (makeLenses)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.:), (.=))
import GHC.Generics (Generic)
import Language.Marlowe.CLI.PAB (WsRunner)
import Language.Marlowe.CLI.Types (CliError, SomePaymentSigningKey)
import Language.Marlowe.Client (MarloweClientInput, MarloweContractState)
import Language.Marlowe.Contract (MarloweContract)
import Language.Marlowe.Core.V1.Semantics (MarloweParams)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, State, TimeInterval)
import Plutus.Contract (ContractInstanceId)
import Plutus.PAB.Webserver.Client (PabClient)
import Plutus.V1.Ledger.Api (PubKeyHash)
import Plutus.V1.Ledger.Time (DiffMilliSeconds, POSIXTime)
import Servant.Client (BaseUrl, ClientM)
import Wallet.Emulator.Wallet (WalletId)

import qualified Cardano.Wallet.Primitive.Types as W (WalletId)
import Control.Lens.Combinators (Lens')
import Control.Lens.Lens (lens)
import qualified Data.Aeson as A (Value (..))
import qualified Data.Map.Strict as M (Map)
import Data.Maybe (fromMaybe)
import Ledger (CurrencySymbol)
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
    -- | Test contracts using the Marlowe PAB.
  | PabTests
    {
      network       :: NetworkId  -- ^ The network ID, if any.
    , socketPath    :: FilePath   -- ^ The path to the node socket.
    , walletUrl     :: BaseUrl    -- ^ The URL for Cardano Wallet.
    , pabUrl        :: BaseUrl    -- ^ The URL for the Marlowe PAB.
    , faucetFile    :: FilePath   -- ^ The file containing the faucet's signing key.
    , faucetAddress :: AddressAny -- ^ The faucet address.
    , burnAddress   :: AddressAny -- ^ The address to which to send unneeded native tokens.
    , passphrase    :: String     -- ^ The passphrase for the Marlowe PAB.
    , tests         :: [a]        -- ^ Input for the tests.
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

-- | On-chain test operations for the Marlowe contract and payout validators.
data ScriptOperation =
  Initialize
    {
      soOwner        :: RoleName              -- ^ The name of the wallet's owner.
    , soTransaction  :: TransactionNickname   -- ^ The name of the wallet's owner.
    , soRoleCurrency :: CurrencySymbol        -- ^ We derive
    , soContract     :: Contract              -- ^ The Marlowe contract to be created.
    -- | FIXME: No *JSON instances for this
    -- , soStake :: Maybe StakeAddressReference
    }
  | Prepare
    {
      soOwner :: RoleName  -- ^ The name of the wallet's owner.
    }
  | Fail
    {
      soFailureMessage :: String
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data PabResponseComparison
  = Equals A.Value
  | Matches A.Value
  deriving stock (Eq, Generic, Show)

comparisonJSON :: Lens' PabResponseComparison A.Value
comparisonJSON = lens get set
  where
    get(Matches json) = json
    get(Equals json)  = json
    set (Matches _) json = Matches json
    set (Equals _) json  = Equals json

instance ToJSON PabResponseComparison where
  toJSON (Equals json)  = object $ pure $ "equals" .= json
  toJSON (Matches json) = object $ pure $ "matches" .= json

data PabResponse = PabResponse
  { _prComparison :: PabResponseComparison
  , _prRetry      :: Bool
  }
  deriving stock (Eq, Generic, Show)

instance ToJSON PabResponse where
  toJSON PabResponse { _prComparison, _prRetry } = object
    [ toComparisonField _prComparison
    , "retry" .= toJSON _prRetry
    ]
    where
      toComparisonField (Equals json)  = "equals" .= json
      toComparisonField (Matches json) = "matches" .= json

instance FromJSON PabResponse where
  parseJSON (A.Object v) = do
    comparison <- (Equals <$> v .: "equals") <|> (Matches <$> v .: "matches")
    retry <- optional (v .: "retry")
    pure $ PabResponse comparison (fromMaybe False retry)

  parseJSON _ = fail
    "JSONPattern should be a an object: { [ equals | matches ] : json, retry : boolean }"


-- | On- and off-chain test operations for Marlowe contracts, via the Marlowe PAB.
data PabOperation =
    -- | Create a wallet.
    CreateWallet
    {
      poOwner :: RoleName  -- ^ The name of the wallet's owner.
    }
    -- | Use a pre-existing wallet.
  | UseWallet
    {
      poOwner      :: RoleName  -- ^ The name of the wallet's owner.
    , poWalletId   :: WalletId  -- ^ The wallet ID.
    , poPassphrase :: String    -- ^ The default wallet passphrase.
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
  | ActivateFollower
    {
      poOwner       :: RoleName
    , poInstance    :: InstanceNickname
    , poAppInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance that is being followed.
    }
  | CallFollow
    {
      poInstance :: InstanceNickname
    }
  | AwaitFollow
    {
      poInstance  :: InstanceNickname
    , poResponses :: [ PabResponse ]
    }
  | ActivateCompanion
    {
      poOwner    :: RoleName
    , poInstance :: InstanceNickname
    }
  | AwaitCompanion
    { poInstance  :: InstanceNickname
    , poResponses :: [ PabResponse ]
    }
    -- | Print the contents of a wallet.
  | PrintWallet
    {
      poOwner :: RoleName  -- ^ The name of the wallet's owner.
    }
    -- | Print the UTxOs at the app adddress.
  | PrintAppUTxOs
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Print the UTxOs at the role-payout adddress.
  | PrintRoleUTxOs
    {
      poInstance :: InstanceNickname  -- ^ The nickname of the PAB contract instance.
    }
    -- | Print an instance state.
  | PrintPABInstanceState
    { poInstance        :: InstanceNickname
    , poMarloweContract :: MarloweContract -- ^ 'MarloweApp' | 'MarloweFollower'
    }
    -- | Print comment.
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
      poTimeoutSeconds :: Int             -- ^ The number of seconds to wait.
    , poOperations     :: [PabOperation]  -- ^ The PAB operations to wait for.
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
    wiWalletId   :: W.WalletId        -- ^ One wallet identifier.
  , wiWalletId'  :: WalletId          -- ^ Another wallet identifier
  , wiAddress    :: AddressAny        -- ^ The first wallet address.
  , wiPubKeyHash :: PubKeyHash        -- ^ The public key hash of the first wallet address.
  , wiPassphrase :: Passphrase "raw"  -- ^ The default wallet passphrase.
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

data FollowerInstanceInfo =
  FollowerInstanceInfo
  {
    fiInstance :: ContractInstanceId
  , fiChannel  :: Chan A.Value
  , fiParams   :: MarloweParams
  }
    deriving (Eq)

instance Show FollowerInstanceInfo where
  show FollowerInstanceInfo{..} =  "FollowerInstanceInfo {fiInstance = "
                           <> show fiInstance
                           <> ", fiParams = "
                           <> show fiParams
                           <> "}"

data CompanionInstanceInfo =
  CompanionInstanceInfo
    { cmpInstance :: ContractInstanceId
    , cmpChannel  :: Chan A.Value
    }
    deriving (Eq)

instance Show CompanionInstanceInfo where
  show CompanionInstanceInfo{..} =  "CompanionInstanceInfo {cmpInstance = "
                           <> show cmpInstance
                           <> "}"

-- | The state of the PAB test framework.
data PabState =
  PabState
  {
    _psFaucetKey          :: SomePaymentSigningKey                       -- ^ The key to the faucet.
  , _psFaucetAddress      :: AddressAny                                  -- ^ The address of the faucet.
  , _psBurnAddress        :: AddressAny                                  -- ^ The address for burning role tokens.
  , _psPassphrase         :: Passphrase "raw"                            -- ^ The default wallet passphrase.
  , _psWallets            :: M.Map RoleName WalletInfo                   -- ^ The wallets being managed.
  , _psAppInstances       :: M.Map InstanceNickname AppInstanceInfo      -- ^ The PAB contract instances being managed.
  , _psFollowerInstances  :: M.Map InstanceNickname FollowerInstanceInfo -- ^ The PAB follower instances being managed.
  , _psCompanionInstances :: M.Map InstanceNickname CompanionInstanceInfo -- ^ The PAB wallet companion
  }
    deriving (Show)

makeLenses ''PabState
makeLenses ''PabResponse
