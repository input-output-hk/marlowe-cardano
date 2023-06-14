{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Trans.Marlowe where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Allocate (MonadAllocate)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.Catch (MonadCatch, MonadMask(..), MonadThrow)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.Event.Class (MonadEvent(..))
import Control.Monad.Except (MonadError)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader(..), ReaderT(..), mapReaderT)
import Control.Monad.State (MonadState)
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Resource (MonadResource)
import Control.Monad.With (MonadWith(..))
import Control.Monad.Writer (MonadWriter)
import Data.GeneralAllocate (GeneralAllocate(..), GeneralAllocated(..))
import Language.Marlowe.Protocol.Client (MarloweRuntimeClient)
import Language.Marlowe.Protocol.Types (MarloweRuntime)
import Network.Protocol.Connection (ClientConnector, ClientConnectorTraced)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Backend (InjectSelector, hoistEventBackend)
import UnliftIO (MonadUnliftIO)

data MarloweTracedContext r s t m = MarloweTracedContext
  { injector :: InjectSelector (s (Handshake MarloweRuntime)) t
  , connector :: ClientConnectorTraced (Handshake MarloweRuntime) MarloweRuntimeClient r s m
  }

newtype MarloweTracedT r s t m a = MarloweTracedT
  { unMarloweTracedT :: ReaderT (MarloweTracedContext r s t m) m a
  }
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
    , MonadState s'
    , MonadWriter w
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadCont
    , MonadUnliftIO
    , MonadBase b
    , MonadBaseControl b
    , MonadResource
    , MonadAllocate
    )

instance MonadWith m => MonadWith (MarloweTracedT r s t m) where
  type WithException (MarloweTracedT r s t m) = WithException m
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (MarloweTracedT r s t m) (WithException m) releaseReturn b a
    -> (a -> MarloweTracedT r s t m b)
    -> MarloweTracedT r s t m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = MarloweTracedT . ReaderT $ \r -> do
    let
      allocA' :: (forall x. m x -> m x) -> m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. MarloweTracedT r s t m x -> MarloweTracedT r s t m x
          restore' mx = MarloweTracedT . ReaderT $ restore . (runReaderT . unMarloweTracedT) mx
        GeneralAllocated a releaseA <- (runReaderT . unMarloweTracedT) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unMarloweTracedT) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unMarloweTracedT) r . go)

instance MonadEvent r t m => MonadEvent r t (MarloweTracedT r s t m) where
  askBackend = MarloweTracedT $ lift $ hoistEventBackend lift <$> askBackend
  localBackend f m = MarloweTracedT $ ReaderT \r -> localBackend
    (hoistEventBackend (flip runReaderT r . unMarloweTracedT) . f . hoistEventBackend lift)
    (runReaderT (unMarloweTracedT m) r)

instance MonadTrans (MarloweTracedT r s t) where
  lift = MarloweTracedT . lift

instance MonadReader r m => MonadReader r (MarloweTracedT r s t m) where
  ask = lift ask
  local = mapMarloweTracedT . local

newtype MarloweT m a = MarloweT { unMarloweT :: ReaderT (ClientConnector (Handshake MarloweRuntime) MarloweRuntimeClient m) m a }
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
    , MonadBaseControl b
    , MonadResource
    , MonadAllocate
    )

instance MonadWith m => MonadWith (MarloweT m) where
  type WithException (MarloweT m) = WithException m
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (MarloweT m) (WithException m) releaseReturn b a
    -> (a -> MarloweT m b)
    -> MarloweT m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = MarloweT . ReaderT $ \r -> do
    let
      allocA' :: (forall x. m x -> m x) -> m (GeneralAllocated m (WithException m) releaseReturn b a)
      allocA' restore = do
        let
          restore' :: forall x. MarloweT m x -> MarloweT m x
          restore' mx = MarloweT . ReaderT $ restore . (runReaderT . unMarloweT) mx
        GeneralAllocated a releaseA <- (runReaderT . unMarloweT) (allocA restore') r
        let
          releaseA' relTy = (runReaderT . unMarloweT) (releaseA relTy) r
        pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (flip (runReaderT . unMarloweT) r . go)

instance MonadEvent r t m => MonadEvent r t (MarloweT m) where
  askBackend = MarloweT $ lift $ hoistEventBackend lift <$> askBackend
  localBackend f m = MarloweT $ ReaderT \r -> localBackend
    (hoistEventBackend (flip runReaderT r . unMarloweT) . f . hoistEventBackend lift)
    (runReaderT (unMarloweT m) r)

instance MonadTrans MarloweT where
  lift = MarloweT . lift

instance MonadReader r m => MonadReader r (MarloweT m) where
  ask = lift ask
  local = mapMarloweT . local

mapMarloweTracedT :: (m a -> m b) -> MarloweTracedT r s t m a -> MarloweTracedT r s t m b
mapMarloweTracedT f = MarloweTracedT . mapReaderT f . unMarloweTracedT

runMarloweTracedT
  :: MarloweTracedT r s t m a
  -> InjectSelector (s (Handshake MarloweRuntime)) t
  -> ClientConnectorTraced (Handshake MarloweRuntime) MarloweRuntimeClient r s m
  -> m a
runMarloweTracedT m injector connector = runReaderT (unMarloweTracedT m) MarloweTracedContext{..}

mapMarloweT :: (m a -> m b) -> MarloweT m a -> MarloweT m b
mapMarloweT f = MarloweT . mapReaderT f . unMarloweT

runMarloweT :: MarloweT m a -> ClientConnector (Handshake MarloweRuntime) MarloweRuntimeClient m -> m a
runMarloweT = runReaderT . unMarloweT
