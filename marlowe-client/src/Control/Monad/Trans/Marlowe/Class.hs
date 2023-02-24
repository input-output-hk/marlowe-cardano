module Control.Monad.Trans.Marlowe.Class
  where

import Control.Monad.Identity (IdentityT(..))
import Control.Monad.Trans.Marlowe
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.Resource.Internal (ResourceT(..))
import Data.Coerce (coerce)
import Language.Marlowe.Protocol.Client (MarloweClient(..), hoistMarloweClient)
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import Language.Marlowe.Protocol.Query.Client (MarloweQueryClient)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand)
import Network.Protocol.Driver (runSomeConnector)
import Network.Protocol.Job.Client (JobClient)
import UnliftIO (MonadUnliftIO, withRunInIO)

class Monad m => MonadMarlowe m where
  runMarloweClient :: MarloweClient m a -> m a

instance MonadUnliftIO m => MonadMarlowe (MarloweT m) where
  runMarloweClient client = MarloweT $ ReaderT \connector -> withRunInIO \runInIO ->
    runSomeConnector connector $ hoistMarloweClient (runInIO . flip runMarloweT connector) client

instance MonadMarlowe m => MonadMarlowe (ReaderT r m) where
  runMarloweClient client = ReaderT \r ->
    runMarloweClient $ hoistMarloweClient (flip runReaderT r) client

instance MonadMarlowe m => MonadMarlowe (ResourceT m) where
  runMarloweClient client = ResourceT \rm ->
    runMarloweClient $ hoistMarloweClient (flip unResourceT rm) client

instance MonadMarlowe m => MonadMarlowe (IdentityT m) where
  runMarloweClient = coerce runMarloweClient

runMarloweSyncClient :: MonadMarlowe m => MarloweSyncClient m a -> m a
runMarloweSyncClient = runMarloweClient . RunMarloweSyncClient

runMarloweHeaderSyncClient :: MonadMarlowe m => MarloweHeaderSyncClient m a -> m a
runMarloweHeaderSyncClient = runMarloweClient . RunMarloweHeaderSyncClient

runMarloweQueryClient :: MonadMarlowe m => MarloweQueryClient m a -> m a
runMarloweQueryClient = runMarloweClient . RunMarloweQueryClient

runMarloweTxClient :: MonadMarlowe m => JobClient MarloweTxCommand m a -> m a
runMarloweTxClient = runMarloweClient . RunTxClient
