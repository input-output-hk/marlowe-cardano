-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Testing Marlowe contracts without the PAB.
--
-----------------------------------------------------------------------------


{-# LANGUAGE BlockArguments     #-}
{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TupleSections      #-}
{-# LANGUAGE TypeApplications   #-}
{-# LANGUAGE TypeOperators      #-}


module Language.Marlowe.CLI.Test.Script where --(
-- -- * Testing
--   scriptTest
-- ) where

import Cardano.Api (AddressAny (..), AddressInEra (..), AlonzoEra, AsType (AsAddress, AsScriptHash, AsShelleyAddr),
                    AssetId (..), AssetName (..), CardanoMode, LocalNodeConnectInfo (..), NetworkId, PolicyId (..),
                    Quantity (..), ShelleyEra, StakeAddressReference (NoStakeAddress), anyAddressInShelleyBasedEra,
                    deserialiseAddress, deserialiseFromRawBytes, lovelaceToValue, negateValue, quantityToLovelace,
                    selectLovelace, serialiseAddress, toAddressAny, valueFromList, valueToList)
import Cardano.Api.Shelley (shelleyPayAddrToPlutusPubKHash)
import Cardano.Mnemonic (SomeMnemonic (..), mnemonicToText)
import Cardano.Wallet.Api (GetTransaction, MigrateShelleyWallet)
import Cardano.Wallet.Api.Client (addressClient, getWallet, listAddresses, postWallet, walletClient)
import Cardano.Wallet.Api.Types (ApiMnemonicT (..), ApiT (..), ApiTransaction (..), ApiTxId (..), ApiWallet (..),
                                 ApiWalletAssetsBalance (ApiWalletAssetsBalance), ApiWalletBalance (ApiWalletBalance),
                                 ApiWalletMigrationPostData (..), WalletOrAccountPostData (..), WalletPostData (..))
import Cardano.Wallet.Gen (genMnemonic)
import Cardano.Wallet.Primitive.AddressDerivation (NetworkDiscriminant (..), Passphrase (..))
import Cardano.Wallet.Primitive.SyncProgress (SyncProgress (Ready))
import Cardano.Wallet.Primitive.Types (WalletName (..))
import Cardano.Wallet.Primitive.Types.TokenPolicy (TokenName (UnsafeTokenName), TokenPolicyId (UnsafeTokenPolicyId))
import Cardano.Wallet.Primitive.Types.TokenQuantity (TokenQuantity (TokenQuantity))
import Cardano.Wallet.Shelley.Compatibility (fromCardanoAddress)
import Control.Concurrent (forkFinally, threadDelay)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (Exception (fromException), SomeException, catch, displayException)
import Control.Lens (use, (%=), (^.))
import Control.Monad (unless, void, when)
import Control.Monad.Except (ExceptT, MonadError, MonadIO, catchError, liftIO, runExceptT, throwError)
import Control.Monad.Extra (untilJustM, whenJust)
import Control.Monad.State.Strict (MonadState, StateT, execStateT, get, lift, put)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (Object, String), (.=))
import Data.Aeson.OneLine (renderValue)
import Data.Aeson.Types (parseEither)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Proxy (Proxy (..))
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.UUID.V4 (nextRandom)
import Language.Marlowe.CLI.Export (buildAddress, buildRoleAddress)
import Language.Marlowe.CLI.IO (liftCli, liftCliIO, liftCliMaybe)
import Language.Marlowe.CLI.PAB (receiveStatus)
import Language.Marlowe.CLI.Test.Types (AppInstanceInfo (..), CompanionInstanceInfo (..), FollowerInstanceInfo (..),
                                        InstanceNickname, PabAccess (..), PabOperation (..), PabResponse,
                                        PabResponseComparison (Equals, Matches), PabState (..), PabTest (..), RoleName,
                                        ScriptOperation (..), ScriptTest (..), WalletInfo (..), comparisonJSON,
                                        prComparison, prRetry, psAppInstances, psBurnAddress, psCompanionInstances,
                                        psFaucetAddress, psFaucetKey, psFollowerInstances, psPassphrase, psWallets)
import Language.Marlowe.CLI.Transaction (buildFaucet, queryUtxos)
import Language.Marlowe.CLI.Types (CliError (..), SomePaymentSigningKey)
import Language.Marlowe.Client (ApplyInputsEndpointSchema, AutoEndpointSchema, CreateEndpointSchema,
                                EndpointResponse (..), MarloweEndpointResult (..), RedeemEndpointSchema)
import Language.Marlowe.Contract (MarloweContract (..))
-- import Language.Marlowe.Semantics (MarloweParams (rolesCurrency))
-- import Language.Marlowe.Semantics.Types (Party (Role))
import Network.WebSockets (Connection)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (InstanceClient (..), PabClient (PabClient, activateContract, instanceClient))
import Plutus.PAB.Webserver.Types (ContractActivationArgs (..), InstanceStatusToClient (..))
import Plutus.V1.Ledger.Api (CurrencySymbol (..), TokenName (..), fromBuiltin, toBuiltin)
import Plutus.V1.Ledger.Time (DiffMilliSeconds (..), POSIXTime (..))
import Servant.API ((:>))
import System.Timeout (timeout)
import Test.QuickCheck (generate)
import Wallet.Emulator.Wallet (Wallet (..), WalletId (..))

import qualified Cardano.Api as C (Value)
import qualified Cardano.Wallet.Api.Types as W (ApiWallet (id, state))
import qualified Cardano.Wallet.Primitive.Types as W (WalletId (..))
import qualified Cardano.Wallet.Primitive.Types.Hash as W (Hash (Hash))
import qualified Cardano.Wallet.Primitive.Types.TokenMap as W (AssetId (AssetId), toFlatList)
import Cardano.Wallet.Shelley.Network.Node (Log (MsgAccountDelegationAndRewards))
import Control.Arrow ((<<<))
import Control.Monad.Error (MonadError (catchError))
import Control.Monad.RWS.Class (MonadReader)
import Control.Monad.Reader.Class (MonadReader (ask))
import Control.Monad.State (modify)
import qualified Data.Aeson as A (Value (..), object)
import qualified Data.ByteArray as BA (pack)
import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Foldable (traverse_)
import Data.Foldable.Extra (for_)
import qualified Data.HashMap.Strict as H (foldrWithKey, lookup)
import Data.List.Extra ((!?))
import qualified Data.Map.Strict as M (adjust, insert, lookup)
import qualified Data.Quantity as W (Quantity (..))
import Data.Text (Text, split)
import qualified Data.Text as T (pack, unpack)
import Data.Text.Array (equal)
import qualified Data.Time.Clock.POSIX as Time (getPOSIXTime)
import qualified Data.Vector as V (all, zip)
import Language.Marlowe.CLI.Run (initializeTransactionImpl, prepareTransaction)
import Ledger.TimeSlot (SlotConfig)
import Network.HTTP.Client (HttpException)
import qualified PlutusTx.AssocMap as AM (fromList)
import qualified Servant.Client as Servant (client)

-- Script state is a placeholder
newtype ScriptState = ScriptState [String]


data ScriptContext = ScriptContext
  { networkId  :: NetworkId
  , slotConfig :: SlotConfig
  }

-- Next Tasks:
-- 1. Try to use initializeTransaction (Language.MArlowe.CLI.Run.hs) and prepareTransction
-- We'd need to change those functions to give us the result instead of storing it in a file.
--
--
-- initializeTransactionImpl :: MonadError CliError m
--                           => MonadIO m
--                           => MarloweParams          -- ^ The Marlowe contract parameters.
--                           -> SlotConfig             -- ^ The POSIXTime-to-slot configuration.
--                           -> CostModelParams        -- ^ The cost model parameters.
--                           -> NetworkId              -- ^ The network ID.
--                           -> StakeAddressReference  -- ^ The stake address.
--                           -> Contract               -- ^ The initial Marlowe contract.
--                           -> State                  -- ^ The initial Marlowe state.
--                           -> Bool                   -- ^ Whether to deeply merkleize the contract.
--                           -> Bool                   -- ^ Whether to print statistics about the validator.
--                           -> m (MarloweTransaction AlonzoEra)


interpret :: MonadError CliError m
          => MonadState ScriptState m
          => MonadReader ScriptContext m
          => MonadIO m
          => ScriptOperation
          -> m ()
interpret (Initialize _) = do
  -- x <- get
  -- modify \(ScriptState lst) -> ScriptState ("newValue" : lst)
  -- put (ScriptState [])
  -- ctx <- ask
  -- x <- get
  modify $ \(ScriptState state) -> ScriptState (state ++ ["Initialize"])
interpret (Fail message) = throwError $ CliError message
interpret (Prepare _)    = modify $ \(ScriptState state) -> ScriptState (state ++ ["Prepare"])

--runOperation :: ScriptOperation -> ()
--runOperation (Initialize {}) = initializeTransaction
--runOperation Prepare    = prepareTransaction

-- | Test a Marlowe contract.
scriptTest  :: MonadError CliError m
            => MonadIO m
            => CostModelParams
            -> NetworkId                         -- ^ The network magic.
            -> LocalNodeConnectInfo CardanoMode  -- ^ The connection to the local node.
            -> SlotConfig                        -- ^ The time and slot correspondence.
            -> ScriptTest                        -- ^ The tests to be run.
            -> m ()                              -- ^ Action for running the tests.
scriptTest _costModel _network _connection _slotConfig ScriptTest{..} =
  do
    -- putStrLn :: String -> IO ()
    -- liftIO :: IO a -> m a
    liftIO $ putStrLn ""
    liftIO . putStrLn $ "***** Test " <> show stTestName <> " *****"

    let
      interpretLoop :: MonadError CliError m
                    => MonadState ScriptState m
                    => MonadReader ScriptContext m
                    => MonadIO m
                    => m ()
      interpretLoop = for_ stScriptOperations \operation -> do
        interpret operation
    catchError
      (
        execStateT interpretLoop (ScriptState [])
        -- TODO: Add a timeout.
        -- void
        --   . execStateT (mapM_ (interpret access) ptPabOperations)
        --   $ []
          -- $ PabState
            -- faucetKey faucetAddress burnAddress
            -- (Passphrase . BA.pack $ toEnum . fromEnum <$> passphrase)
            -- mempty mempty mempty mempty
      )
      $ \e -> do
        -- TODO: Clean up wallets and instances.
        liftIO (putStrLn $ show e)
        liftIO (putStrLn "***** FAILED *****")
        throwError (e :: CliError)
    liftIO $ putStrLn "***** PASSED *****"




