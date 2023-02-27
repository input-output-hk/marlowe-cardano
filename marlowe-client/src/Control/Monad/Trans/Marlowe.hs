{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Marlowe
  where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Cleanup (MonadCleanup(..))
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), mapReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..), MonadTransControl(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.Writer (MonadWriter)
import Language.Marlowe.Protocol.Client (MarloweClient)
import Network.Protocol.Connection (SomeClientConnector)
import UnliftIO (MonadUnliftIO)

newtype MarloweT m a = MarloweT { unMarloweT :: ReaderT (SomeClientConnector MarloweClient IO) m a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadPlus
    , MonadError e
    , MonadState s
    , MonadWriter w
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadCont
    , MonadUnliftIO
    , MonadBase b
    , MonadTransControl
    , MonadBaseControl b
    , MonadResource
    , MonadCleanup
    )

instance MonadTrans MarloweT where
  lift = MarloweT . lift

instance MonadReader r m => MonadReader r (MarloweT m) where
  ask = lift ask
  local = mapMarloweT . local

mapMarloweT :: (m a -> n b) -> MarloweT m a -> MarloweT n b
mapMarloweT f = MarloweT . mapReaderT f . unMarloweT

runMarloweT :: MarloweT m a -> SomeClientConnector MarloweClient IO -> m a
runMarloweT = runReaderT . unMarloweT
