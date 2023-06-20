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
import Network.Protocol.Connection (Connector)
import Network.Protocol.Handshake.Types (Handshake)
import Observe.Event.Backend (InjectSelector, hoistEventBackend)
import UnliftIO (MonadUnliftIO)

data MarloweTracedContext s t m = MarloweTracedContext
  { injector :: InjectSelector (s (Handshake MarloweRuntime)) t
  , connector :: Connector MarloweRuntimeClient m
  }

newtype MarloweT m a = MarloweT { unMarloweT :: ReaderT (Connector MarloweRuntimeClient m) m a }
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

mapMarloweT :: (m a -> m b) -> MarloweT m a -> MarloweT m b
mapMarloweT f = MarloweT . mapReaderT f . unMarloweT

runMarloweT :: MarloweT m a -> Connector MarloweRuntimeClient m -> m a
runMarloweT = runReaderT . unMarloweT
