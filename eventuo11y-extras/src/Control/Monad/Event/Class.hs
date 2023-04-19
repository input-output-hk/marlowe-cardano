{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Event.Class
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT(..))
import Control.Monad.Trans.Reader (ReaderT(..), asks)
import Control.Monad.Trans.Resource.Internal (ResourceT(..))
import Control.Monad.With (MonadWithExceptable)
import Data.Functor (void)
import Observe.Event (Event, EventBackend, InjectSelector, NewEventArgs(..))
import Observe.Event.Backend
  (Event(..), hoistEventBackend, narrowEventBackend, setAncestorEventBackend, simpleNewEventArgs)
import qualified Observe.Event.Explicit as Explicit
import Unsafe.Coerce (unsafeCoerce)

class MonadWithExceptable m => MonadEvent r s m | m -> r s where
  askBackend :: m (EventBackend m r s)
  localBackend :: (forall t. EventBackend m r t -> EventBackend m r t) -> m a -> m a

instance MonadEvent r s m => MonadEvent r s (IdentityT m) where
  askBackend = unsafeCoerce $ askBackend @r @s @m
  localBackend f = IdentityT . localBackend (unsafeCoerce f) . runIdentityT

instance MonadEvent r s m => MonadEvent r s (ReaderT r' m) where
  askBackend = hoistEventBackend lift <$> lift askBackend
  localBackend f m = ReaderT \r -> localBackend
    (hoistEventBackend (flip runReaderT r) . f . hoistEventBackend lift)
    (runReaderT m r)

instance MonadEvent r s m => MonadEvent r s (ResourceT m) where
  askBackend = hoistEventBackend lift <$> lift askBackend
  localBackend f m = ResourceT \releaseMap -> localBackend
    (hoistEventBackend (flip unResourceT releaseMap) . f . hoistEventBackend lift)
    (unResourceT m releaseMap)

class Inject s t where
  inject :: InjectSelector s t

type MonadInjectEvent r s t m = (MonadEvent r t m, Inject s t)

emitImmediateEventArgs_ :: MonadInjectEvent r s t m => NewEventArgs r s f -> m ()
emitImmediateEventArgs_ = void . emitImmediateEventArgs

emitImmediateEventArgs :: MonadInjectEvent r s t m => NewEventArgs r s f -> m r
emitImmediateEventArgs args = do
  backend <- askBackend
  Explicit.emitImmediateEvent (narrowEventBackend inject backend) args

emitImmediateEvent_ :: MonadInjectEvent r s t m => s f -> m ()
emitImmediateEvent_ = void . emitImmediateEvent

emitImmediateEvent ::MonadInjectEvent r s t m => s f -> m r
emitImmediateEvent = emitImmediateEventArgs . simpleNewEventArgs

withEventArgs
  :: forall r s t m f a
   . MonadInjectEvent r s t m
  => NewEventArgs r s f
  -> (Event m r f -> m a)
  -> m a
withEventArgs args f = do
  backend <- askBackend
  Explicit.withEventArgs (narrowEventBackend inject backend) args \ev ->
    localBackend (setAncestorEventBackend (reference ev)) $ f ev

withEvent :: MonadInjectEvent r s t m => s f -> (Event m r f -> m a) -> m a
withEvent = withEventArgs . simpleNewEventArgs

newEventArgs :: MonadInjectEvent r s t m => NewEventArgs r s f -> m (Event m r f)
newEventArgs args = do
  backend <- askBackend
  Explicit.newEvent (narrowEventBackend inject backend) args

newEvent :: MonadInjectEvent r s t m => s f -> m (Event m r f)
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
  :: Monad m
  => (forall x. ReaderT env m x -> n x) -- ^ Constructor
  -> (env -> EventBackend n r s) -- ^ Access root backend from env
  -> n (EventBackend n r s)
askBackendReaderT wrap get = wrap $ asks get

composeInjectSelector :: InjectSelector t u -> InjectSelector s t -> InjectSelector s u
composeInjectSelector tu st s withInjF = st s \t injF -> tu t \u injG -> withInjF u $ injG . injF
