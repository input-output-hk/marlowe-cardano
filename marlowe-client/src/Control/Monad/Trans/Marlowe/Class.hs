{-# LANGUAGE GADTs #-}
module Control.Monad.Trans.Marlowe.Class
  where

import Cardano.Api (BabbageEra, Tx, TxBody)
import Control.Concurrent (threadDelay)
import Control.Monad.Identity (IdentityT(..))
import Control.Monad.Trans.Marlowe
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource.Internal (ResourceT(..))
import Data.Coerce (coerce)
import Data.Time (UTCTime)
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient(..), hoistMarloweRuntimeClient)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, Lovelace, StakeCredential, TokenName, TxId)
import Language.Marlowe.Runtime.Core.Api (Contract, ContractId, Inputs, MarloweTransactionMetadata, MarloweVersion)
import Language.Marlowe.Runtime.Transaction.Api
  ( ApplyInputsError
  , ContractCreated
  , CreateError
  , InputsApplied
  , JobId(..)
  , MarloweTxCommand(..)
  , RoleTokensConfig
  , SubmitError
  , SubmitStatus
  , WalletAddresses
  , WithdrawError
  )
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Job.Client (ClientStAwait(..), ClientStInit(..), JobClient(..), liftCommand)
import qualified Network.Protocol.Job.Client as Job
import UnliftIO (MonadIO, MonadUnliftIO, liftIO, newIORef, readIORef, withRunInIO, writeIORef)

-- ^ A class for monadic contexts that provide a connection to a Marlowe
-- Runtime instance.
class Monad m => MonadMarlowe m where
  -- ^ Run a client of the Marlowe protocol.
  runMarloweRuntimeClient :: MarloweRuntimeClient m a -> m a

instance MonadUnliftIO m => MonadMarlowe (MarloweT m) where
  runMarloweRuntimeClient client = MarloweT $ ReaderT \connector -> withRunInIO \runInIO ->
    runSomeConnector connector $ hoistMarloweRuntimeClient (runInIO . flip runMarloweT connector) client

instance MonadMarlowe m => MonadMarlowe (ReaderT r m) where
  runMarloweRuntimeClient client = ReaderT \r ->
    runMarloweRuntimeClient $ hoistMarloweRuntimeClient (flip runReaderT r) client

instance MonadMarlowe m => MonadMarlowe (ResourceT m) where
  runMarloweRuntimeClient client = ResourceT \rm ->
    runMarloweRuntimeClient $ hoistMarloweRuntimeClient (flip unResourceT rm) client

instance MonadMarlowe m => MonadMarlowe (IdentityT m) where
  runMarloweRuntimeClient = coerce runMarloweRuntimeClient

-- ^ Run a MarloweSyncClient. Used to synchronize with history for a specific
-- contract.
runMarloweSyncClient :: MonadMarlowe m => MarloweSyncClient m a -> m a
runMarloweSyncClient = runMarloweRuntimeClient . RunMarloweSyncClient

-- ^ Run a MarloweHeaderSyncClient. Used to synchronize with contract creation
-- transactions.
runMarloweHeaderSyncClient :: MonadMarlowe m => MarloweHeaderSyncClient m a -> m a
runMarloweHeaderSyncClient = runMarloweRuntimeClient . RunMarloweHeaderSyncClient

-- ^ Run a MarloweQueryClient.
runMarloweQueryClient :: MonadMarlowe m => MarloweQueryClient m a -> m a
runMarloweQueryClient = runMarloweRuntimeClient . RunMarloweQueryClient

-- ^ Run a MarloweTxCommand job client.
runMarloweTxClient :: MonadMarlowe m => JobClient MarloweTxCommand m a -> m a
runMarloweTxClient = runMarloweRuntimeClient . RunTxClient

-- ^ Create a new contract.
createContract
  :: MonadMarlowe m
  => Maybe StakeCredential
  -- ^ A reference to the stake address to use for script addresses.
  -> MarloweVersion v
  -- ^ The Marlowe version to use
  -> WalletAddresses
  -- ^ The wallet addresses to use when constructing the transaction
  -> RoleTokensConfig
  -- ^ How to initialize role tokens
  -> MarloweTransactionMetadata
  -- ^ Optional metadata to attach to the transaction
  -> Lovelace
  -- ^ Min Lovelace which should be used for the contract output.
  -> Contract v
  -- ^ The contract to run
  -> m (Either (CreateError v) (ContractCreated BabbageEra v))
createContract mStakeCredential version wallet roleTokens metadata lovelace contract =
  runMarloweTxClient $ liftCommand $ Create
    mStakeCredential
    version
    wallet
    roleTokens
    metadata
    lovelace
    contract

-- ^ Apply inputs to a contract, with custom validity interval bounds.
applyInputs'
  :: MonadMarlowe m
  => MarloweVersion v
  -- ^ The Marlowe version to use
  -> WalletAddresses
  -- ^ The wallet addresses to use when constructing the transaction
  -> ContractId
  -- ^ The ID of the contract to apply the inputs to.
  -> MarloweTransactionMetadata
  -- ^ Optional metadata to attach to the transaction
  -> Maybe UTCTime
  -- ^ The "invalid before" bound of the validity interval. If omitted, this
  -- is computed from the contract.
  -> Maybe UTCTime
  -- ^ The "invalid hereafter" bound of the validity interval. If omitted, this
  -- is computed from the contract.
  -> Inputs v
  -- ^ The inputs to apply.
  -> m (Either (ApplyInputsError v) (InputsApplied BabbageEra v))
applyInputs' version wallet contractId metadata invalidBefore invalidHereafter inputs =
  runMarloweTxClient $ liftCommand $ ApplyInputs
    version
    wallet
    contractId
    metadata
    invalidBefore
    invalidHereafter
    inputs

-- ^ Apply inputs to a contract.
applyInputs
  :: MonadMarlowe m
  => MarloweVersion v
  -- ^ The Marlowe version to use
  -> WalletAddresses
  -- ^ The wallet addresses to use when constructing the transaction
  -> ContractId
  -- ^ The ID of the contract to apply the inputs to.
  -> MarloweTransactionMetadata
  -- ^ Optional metadata to attach to the transaction
  -> Inputs v
  -- ^ The inputs to apply.
  -> m (Either (ApplyInputsError v) (InputsApplied BabbageEra v))
applyInputs version wallet contractId metadata =
  applyInputs' version wallet contractId metadata Nothing Nothing

-- ^ Withdraw funds that have been paid out to a role in a contract.
withdraw
  :: MonadMarlowe m
  => MarloweVersion v
  -- ^ The Marlowe version to use
  -> WalletAddresses
  -- ^ The wallet addresses to use when constructing the transaction
  -> ContractId
  -- ^ The ID of the contract to apply the inputs to.
  -> TokenName
  -- ^ The names of the roles whose assets to withdraw.
  -> m (Either (WithdrawError v) (TxBody BabbageEra))
withdraw version wallet contractId role =
  runMarloweTxClient $ liftCommand $ Withdraw version wallet contractId role

-- Submit a signed transaction via the Marlowe Runtime. Waits for completion
-- with exponential back-off in the polling.
submitAndWait
  :: (MonadMarlowe m, MonadIO m)
  => Tx BabbageEra
  -- ^ The transaction to submit.
  -> m (Either SubmitError BlockHeader)
submitAndWait tx = do
  delayRef <- liftIO $ newIORef 1000
  submit (onAwait delayRef) (pure . Left) (pure . Right) tx
  where
    onAwait delayRef _ _ = liftIO do
      delay <- readIORef delayRef
      threadDelay delay
      writeIORef delayRef (min 1_000_000 $ delay * 10)
      pure Nothing

-- Submit a signed transaction via the Marlowe Runtime. If it does not complete
-- immediately, it will not wait for completion, and a TxId will be returned
-- which can be used to check progress later via @attachSubmit@.
submitAndDetach
  :: MonadMarlowe m
  => Tx BabbageEra
  -- ^ The transaction to submit.
  -> m (Either TxId (Either SubmitError BlockHeader))
submitAndDetach = submit (const . pure . Just . Left) (pure . Right . Left) (pure . Right . Right)

-- Submit a signed transaction via the Marlowe Runtime.
submit
  :: MonadMarlowe m
  => (TxId -> SubmitStatus -> m (Maybe a))
  -- ^ Handle being told to wait. Receives the ID of the transaction (which can
  -- be used to attach later via @attachSubmit@) and the status of the
  -- submission.
  -> (SubmitError -> m a)
  -- ^ Handle a submit failure.
  -> (BlockHeader -> m a)
  -- ^ Handle submission success. Receives the block header of the block the
  -- transaction was seen to have been published on. Note: this block could be
  -- rolled back, in which case the block header would change and the
  -- transaction may not exist anymore.
  -> Tx BabbageEra
  -- ^ The transaction to submit.
  -> m a
submit onAwait onFail onSuccess tx =
  runMarloweTxClient $ JobClient $ pure $ SendMsgExec (Submit tx) clientCmd
  where
    clientCmd = Job.ClientStCmd
      { recvMsgFail = onFail
      , recvMsgSucceed = onSuccess
      , recvMsgAwait = \status (JobIdSubmit txId) -> do
          result <- onAwait txId status
          pure case result of
            Nothing -> SendMsgPoll clientCmd
            Just a -> SendMsgDetach a
      }

-- ^ Attach to a previously launched tx submission job. Returns @Nothing@ if
-- the submission job couldn't be found.
attachSubmit
  :: MonadMarlowe m
  => (SubmitStatus -> m (Maybe a))
  -- ^ Progress callback.
  -> (SubmitError -> m a)
  -- ^ Handle a submit failure.
  -> (BlockHeader -> m a)
  -- ^ Handle submission success. Receives the block header of the block the
  -- transaction was seen to have been published on. Note: this block could be
  -- rolled back, in which case the block header would change and the
  -- transaction may not exist anymore.
  -> TxId
  -- ^ The ID of the transaction whose submission job to attach to.
  -> m (Maybe a)
attachSubmit onAwait onFail onSuccess txId =
  runMarloweTxClient $ JobClient $ pure $ SendMsgAttach (JobIdSubmit txId) Job.ClientStAttach
    { recvMsgAttachFailed = pure Nothing
    , recvMsgAttached = pure $ Just <$> clientCmd
    }
  where
    clientCmd = Job.ClientStCmd
      { recvMsgFail = onFail
      , recvMsgSucceed = onSuccess
      , recvMsgAwait = \status _ -> do
          result <- onAwait status
          pure case result of
            Nothing -> SendMsgPoll clientCmd
            Just a -> SendMsgDetach a
      }
