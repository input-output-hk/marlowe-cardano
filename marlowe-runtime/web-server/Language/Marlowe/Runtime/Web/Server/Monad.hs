{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Defines a custom Monad for the web server's handler functions to run in.

module Language.Marlowe.Runtime.Web.Server.Monad
  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Cleanup (MonadCleanup(..))
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.Coerce (coerce)
import Language.Marlowe.Runtime.Web.Server.ContractHeaderIndexer (LoadContractHeaders)
import Servant

newtype AppM a = AppM { runAppM :: ReaderT AppEnv Handler a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppEnv
    , MonadFail
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadBaseControl IO
    , MonadError ServerError
    , MonadBase IO
    )

instance MonadCleanup AppM where
  generalCleanup acquire release action = coerce $ generalCleanup
    (toTransformers acquire)
    (\a b -> toTransformers $ release a b)
    (\a -> toTransformers $ action a)
    where
      toTransformers :: AppM a -> ReaderT AppEnv (ExceptT ServerError IO) a
      toTransformers = coerce

newtype AppEnv = AppEnv
  { _loadContractHeaders :: LoadContractHeaders IO
  }

-- | Load a list of contract headers.
loadContractHeaders :: LoadContractHeaders AppM
loadContractHeaders startFrom limit offset order = do
  load <- asks _loadContractHeaders
  liftIO $ load startFrom limit offset order
