{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Event.Class
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Reader (ReaderT(..), asks)
import Control.Monad.With (MonadWithExceptable)
import Data.Functor (void)
import Observe.Event (Event, EventBackend, InjectSelector, NewEventArgs(..))
import Observe.Event.Backend
  (Event(..), hoistEventBackend, narrowEventBackend, setAncestorEventBackend, simpleNewEventArgs)
import qualified Observe.Event.Explicit as Explicit
import Unsafe.Coerce (unsafeCoerce)

class MonadWithExceptable m => MonadBackend r m | m -> r where
  localBackend :: (forall t. EventBackend m r t -> EventBackend m r t) -> m a -> m a

class MonadBackend r m => MonadEvent r s m | m -> r where
  askBackend :: m (EventBackend m r s)

instance MonadBackend r m => MonadBackend r (IdentityT m) where
  localBackend f = IdentityT . localBackend (unsafeCoerce f) . runIdentityT

instance MonadEvent r s m => MonadEvent r s (IdentityT m) where
  askBackend = unsafeCoerce $ askBackend @r @s @m

instance MonadBackend r m => MonadBackend r (ReaderT r' m) where
  localBackend f m = ReaderT \r -> localBackend
    (hoistEventBackend (flip runReaderT r) . f . hoistEventBackend lift)
    (runReaderT m r)

instance MonadEvent r s m => MonadEvent r s (ReaderT r' m) where
  askBackend = hoistEventBackend lift <$> lift askBackend

emitImmediateEventArgs_ :: MonadEvent r s m => NewEventArgs r s f -> m ()
emitImmediateEventArgs_ = void . emitImmediateEventArgs

emitImmediateEventArgs :: MonadEvent r s m => NewEventArgs r s f -> m r
emitImmediateEventArgs args = do
  backend <- askBackend
  Explicit.emitImmediateEvent backend args

emitImmediateEvent_ :: MonadEvent r s m => s f -> m ()
emitImmediateEvent_ = void . emitImmediateEvent

emitImmediateEvent :: MonadEvent r s m => s f -> m r
emitImmediateEvent = emitImmediateEventArgs . simpleNewEventArgs

withEventArgs
  :: forall r s m f a
   . MonadEvent r s m
  => NewEventArgs r s f
  -> (Event m r f -> m a)
  -> m a
withEventArgs args f = do
  backend <- askBackend
  Explicit.withEventArgs backend args \ev ->
    localBackend (setAncestorEventBackend (reference ev)) $ f ev

withEvent :: MonadEvent r s m => s f -> (Event m r f -> m a) -> m a
withEvent = withEventArgs . simpleNewEventArgs

newEventArgs :: MonadEvent r s m => NewEventArgs r s f -> m (Event m r f)
newEventArgs args = do
  backend <- askBackend
  Explicit.newEvent backend args

newEvent :: MonadEvent r s m => s f -> m (Event m r f)
newEvent = newEventArgs . simpleNewEventArgs

-- Implementation helpers

localBackendReaderT
  :: (forall x. ReaderT env m x -> n x) -- ^ Constructor
  -> (forall x. n x -> ReaderT env m x) -- ^ Destructor
  -> ((forall s. EventBackend n r s -> EventBackend n r s) -> env -> env) -- ^ Modify backend within env
  -> (forall s. EventBackend n r s -> EventBackend n r s)
  -> n a
  -> n a
localBackendReaderT wrap unwrap zoom f go =
  wrap $ ReaderT \env -> runReaderT (unwrap go) $ zoom f env

askBackendReaderT
  :: (Functor n, Monad m)
  => (forall x. ReaderT env m x -> n x) -- ^ Constructor
  -> (env -> EventBackend n r t) -- ^ Access root backend from env
  -> InjectSelector s t
  -> n (EventBackend n r s)
askBackendReaderT wrap get inject = wrap $ asks $ narrowEventBackend inject . get

composeInjectSelector :: InjectSelector t u -> InjectSelector s t -> InjectSelector s u
composeInjectSelector tu st s withInjF = st s \t injF -> tu t \u injG -> withInjF u $ injG . injF
