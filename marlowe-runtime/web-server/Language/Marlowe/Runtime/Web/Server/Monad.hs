{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Runtime.Web.Server.Monad
  where

import Control.Monad.Base (MonadBase)
import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.Catch.Pure (MonadMask)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans.Control (MonadBaseControl)
import Language.Marlowe.Runtime.Web
import Servant
import Servant.Pagination (Range)

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

newtype AppEnv = AppEnv
  { _loadContractHeaders :: Range "contractId" TxOutRef -> IO (Maybe [ContractHeader])
  }

loadContractHeaders :: Range "contractId" TxOutRef -> AppM (Maybe [ContractHeader])
loadContractHeaders range = asks  _loadContractHeaders >>= liftIO . ($ range)
