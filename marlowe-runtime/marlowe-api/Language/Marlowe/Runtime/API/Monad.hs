{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.API.Monad
  where

import Control.Monad (MonadPlus, (>=>))
import Control.Monad.Base (MonadBase)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Control (MonadBaseControl, liftBaseWith)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, ask, asks, local)
import Data.Void (Void)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient, hoistMarloweSyncClient)
import Language.Marlowe.Runtime.API.Env (Env(..))
import Language.Marlowe.Runtime.Discovery.Api (DiscoveryQuery)
import Language.Marlowe.Runtime.History.Api (HistoryCommand, HistoryQuery)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Job.Client (JobClient, hoistJobClient, liftCommand)
import Network.Protocol.Query.Client (QueryClient, hoistQueryClient, liftQuery)
import Options.Applicative (Alternative)
import System.Exit (die)

-- | A monad type for Marlowe Runtime CLI programs.
newtype CLI a = CLI { runCLI :: ReaderT (Env IO) IO a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadFix
    , MonadFail
    , MonadIO
    , Alternative
    , MonadPlus
    , MonadBase IO
    , MonadBaseControl IO
    )

-- | Get the environment.
askEnv :: CLI (Env IO)
askEnv = CLI ask

-- | Get a value from the environment.
asksEnv :: (Env IO -> a) -> CLI a
asksEnv = CLI . asks

-- | Run a CLI program in a locally modified environment.
localEnv :: (Env IO -> Env IO) -> CLI a -> CLI a
localEnv f = CLI . local f . runCLI

-- | Run a History Job client.
runHistoryJobClient :: JobClient HistoryCommand CLI a -> CLI a
runHistoryJobClient client = do
  Env{..} <- askEnv
  liftBaseWith \runInBase ->
    envRunHistoryJobClient $ hoistJobClient runInBase client

-- | Run a History Query client.
runHistoryQueryClient :: QueryClient HistoryQuery CLI a -> CLI a
runHistoryQueryClient client = do
  Env{..} <- askEnv
  liftBaseWith \runInBase ->
    envRunHistoryQueryClient $ hoistQueryClient runInBase client

-- | Run a Marlowe Sync client.
runHistorySyncClient :: MarloweSyncClient CLI a -> CLI a
runHistorySyncClient client = do
  Env{..} <- askEnv
  liftBaseWith \runInBase ->
    envRunHistorySyncClient $ hoistMarloweSyncClient runInBase client

-- | Run a Tx Job client.
runTxJobClient :: JobClient MarloweTxCommand CLI a -> CLI a
runTxJobClient client = do
  Env{..} <- askEnv
  liftBaseWith \runInBase ->
    envRunTxJobClient $ hoistJobClient runInBase client

-- | Run a Discovery Query client.
runDiscoveryQueryClient :: QueryClient DiscoveryQuery CLI a -> CLI a
runDiscoveryQueryClient client = do
  Env{..} <- askEnv
  liftBaseWith \runInBase ->
    envRunDiscoveryQueryClient $ hoistQueryClient runInBase client

-- | Run a simple History command.
runHistoryCommand :: HistoryCommand Void err result -> CLI (Either err result)
runHistoryCommand = runHistoryJobClient . liftCommand

-- | Run a simple History query.
queryHistory :: HistoryQuery Void err results -> CLI (Either err results)
queryHistory = runHistoryQueryClient . liftQuery

-- | Run a simple Tx command.
runTxCommand :: MarloweTxCommand Void err result -> CLI (Either err result)
runTxCommand = runTxJobClient . liftCommand

runCLIExceptT :: Show e => ExceptT e CLI a -> CLI a
runCLIExceptT = runExceptT >=> \case
  Left ex -> liftIO $ die $ show ex
  Right a -> pure a
