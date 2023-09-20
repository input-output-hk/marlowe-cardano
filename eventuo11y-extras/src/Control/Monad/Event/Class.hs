{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Monad.Event.Class where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Allocate (MonadAllocate)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Reader (ReaderT (..), asks)
import Control.Monad.Trans.Resource.Internal (MonadResource, ResourceT (..))
import Control.Monad.With
import Data.Functor (void)
import Data.GeneralAllocate
import Data.Kind (Type)
import Observe.Event (Event, EventBackend, InjectSelector, NewEventArgs (..))
import Observe.Event.Backend (
  Event (..),
  hoistEventBackend,
  narrowEventBackend,
  noopEventBackend,
  setAncestorEventBackend,
  simpleNewEventArgs,
 )
import qualified Observe.Event.Explicit as Explicit
import UnliftIO (MonadUnliftIO)
import Unsafe.Coerce (unsafeCoerce)

class (MonadWithExceptable m) => MonadEvent r s m | m -> r s where
  askBackend :: m (EventBackend m r s)
  localBackend :: (forall t. EventBackend m r t -> EventBackend m r t) -> m a -> m a

newtype NoopEventT r (s :: Type -> Type) m a = NoopEventT {runNoopEventT :: m a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadFail
    , MonadFix
    , MonadIO
    , MonadPlus
    , MonadUnliftIO
    , MonadResource
    , MonadAllocate
    )

instance (MonadWith m) => MonadWith (NoopEventT r s m) where
  type WithException (NoopEventT r s m) = WithException m
  stateThreadingGeneralWith
    :: forall a b releaseReturn
     . GeneralAllocate (NoopEventT r s m) (WithException m) releaseReturn b a
    -> (a -> NoopEventT r s m b)
    -> NoopEventT r s m (b, releaseReturn)
  stateThreadingGeneralWith (GeneralAllocate allocA) go = NoopEventT do
    let allocA' :: (forall x. m x -> m x) -> m (GeneralAllocated m (WithException m) releaseReturn b a)
        allocA' restore = do
          let restore' :: forall x. NoopEventT r s m x -> NoopEventT r s m x
              restore' mx = NoopEventT $ restore $ runNoopEventT mx
          GeneralAllocated a releaseA <- runNoopEventT (allocA restore')
          let releaseA' relTy = runNoopEventT (releaseA relTy)
          pure $ GeneralAllocated a releaseA'
    stateThreadingGeneralWith (GeneralAllocate allocA') (runNoopEventT . go)

instance (Monoid r, MonadWithExceptable m) => MonadEvent r s (NoopEventT r s m) where
  askBackend = pure $ noopEventBackend mempty
  localBackend = const id

instance (MonadEvent r s m) => MonadEvent r s (IdentityT m) where
  askBackend = unsafeCoerce $ askBackend @r @s @m
  localBackend f = IdentityT . localBackend (unsafeCoerce f) . runIdentityT

instance (MonadEvent r s m) => MonadEvent r s (ReaderT r' m) where
  askBackend = hoistEventBackend lift <$> lift askBackend
  localBackend f m = ReaderT \r ->
    localBackend
      (hoistEventBackend (flip runReaderT r) . f . hoistEventBackend lift)
      (runReaderT m r)

instance (MonadEvent r s m) => MonadEvent r s (ResourceT m) where
  askBackend = hoistEventBackend lift <$> lift askBackend
  localBackend f m = ResourceT \releaseMap ->
    localBackend
      (hoistEventBackend (flip unResourceT releaseMap) . f . hoistEventBackend lift)
      (unResourceT m releaseMap)

class Inject s t where
  inject :: InjectSelector s t

type MonadInjectEvent r s t m = (MonadEvent r t m, Inject s t)

emitImmediateEventFields :: (MonadInjectEvent r s t m) => s f -> [f] -> m r
emitImmediateEventFields s fs = emitImmediateEventArgs (simpleNewEventArgs s){newEventInitialFields = fs}

emitImmediateEventArgs :: (MonadInjectEvent r s t m) => NewEventArgs r s f -> m r
emitImmediateEventArgs = emitImmediateInjectEventArgs inject

emitImmediateInjectEventFields :: (MonadEvent r t m) => InjectSelector s t -> s f -> [f] -> m r
emitImmediateInjectEventFields inj s fs = emitImmediateInjectEventArgs inj (simpleNewEventArgs s){newEventInitialFields = fs}

emitImmediateInjectEventArgs :: (MonadEvent r t m) => InjectSelector s t -> NewEventArgs r s f -> m r
emitImmediateInjectEventArgs inj args = do
  backend <- askBackend
  Explicit.emitImmediateEvent (narrowEventBackend inj backend) args

emitImmediateEventFields_ :: (MonadInjectEvent r s t m) => s f -> [f] -> m ()
emitImmediateEventFields_ s fs = emitImmediateEventArgs_ (simpleNewEventArgs s){newEventInitialFields = fs}

emitImmediateEventArgs_ :: (MonadInjectEvent r s t m) => NewEventArgs r s f -> m ()
emitImmediateEventArgs_ = void . emitImmediateEventArgs

emitImmediateInjectEventFields_ :: (MonadEvent r t m) => InjectSelector s t -> s f -> [f] -> m ()
emitImmediateInjectEventFields_ inj s fs = emitImmediateInjectEventArgs_ inj (simpleNewEventArgs s){newEventInitialFields = fs}

emitImmediateInjectEventArgs_ :: (MonadEvent r t m) => InjectSelector s t -> NewEventArgs r s f -> m ()
emitImmediateInjectEventArgs_ inj = void . emitImmediateInjectEventArgs inj

emitImmediateEvent_ :: (MonadInjectEvent r s t m) => s f -> m ()
emitImmediateEvent_ = void . emitImmediateEvent

emitImmediateInjectEvent_ :: (MonadEvent r t m) => InjectSelector s t -> s f -> m ()
emitImmediateInjectEvent_ inj = void . emitImmediateInjectEvent inj

emitImmediateEvent :: (MonadInjectEvent r s t m) => s f -> m r
emitImmediateEvent = emitImmediateEventArgs . simpleNewEventArgs

emitImmediateInjectEvent :: (MonadEvent r t m) => InjectSelector s t -> s f -> m r
emitImmediateInjectEvent inj = emitImmediateInjectEventArgs inj . simpleNewEventArgs

withEventFields
  :: forall r s t m f a
   . (MonadInjectEvent r s t m)
  => s f
  -> [f]
  -> (Event m r f -> m a)
  -> m a
withEventFields s fs = withEventArgs (simpleNewEventArgs s){newEventInitialFields = fs}

withEventArgs
  :: forall r s t m f a
   . (MonadInjectEvent r s t m)
  => NewEventArgs r s f
  -> (Event m r f -> m a)
  -> m a
withEventArgs = withInjectEventArgs inject

withInjectEventFields
  :: forall r s t m f a
   . (MonadEvent r t m)
  => InjectSelector s t
  -> s f
  -> [f]
  -> (Event m r f -> m a)
  -> m a
withInjectEventFields inj s fs = withInjectEventArgs inj (simpleNewEventArgs s){newEventInitialFields = fs}

withInjectEventArgs
  :: forall r s t m f a
   . (MonadEvent r t m)
  => InjectSelector s t
  -> NewEventArgs r s f
  -> (Event m r f -> m a)
  -> m a
withInjectEventArgs inj args f = do
  backend <- askBackend
  Explicit.withEventArgs (narrowEventBackend inj backend) args \ev ->
    localBackend (setAncestorEventBackend (reference ev)) $ f ev

withEvent :: (MonadInjectEvent r s t m) => s f -> (Event m r f -> m a) -> m a
withEvent = withEventArgs . simpleNewEventArgs

withInjectEvent :: (MonadEvent r t m) => InjectSelector s t -> s f -> (Event m r f -> m a) -> m a
withInjectEvent inj = withInjectEventArgs inj . simpleNewEventArgs

newEventFields :: (MonadInjectEvent r s t m) => s f -> [f] -> m (Event m r f)
newEventFields s fs = newEventArgs (simpleNewEventArgs s){newEventInitialFields = fs}

newEventArgs :: (MonadInjectEvent r s t m) => NewEventArgs r s f -> m (Event m r f)
newEventArgs = newInjectEventArgs inject

newInjectEventFields :: (MonadEvent r t m) => InjectSelector s t -> s f -> [f] -> m (Event m r f)
newInjectEventFields inj s fs = newInjectEventArgs inj (simpleNewEventArgs s){newEventInitialFields = fs}

newInjectEventArgs :: (MonadEvent r t m) => InjectSelector s t -> NewEventArgs r s f -> m (Event m r f)
newInjectEventArgs inj args = do
  backend <- askBackend
  Explicit.newEvent (narrowEventBackend inj backend) args

newEvent :: (MonadInjectEvent r s t m) => s f -> m (Event m r f)
newEvent = newEventArgs . simpleNewEventArgs

newInjectEvent :: (MonadEvent r t m) => InjectSelector s t -> s f -> m (Event m r f)
newInjectEvent inj = newInjectEventArgs inj . simpleNewEventArgs

-- Implementation helpers

localBackendReaderT
  :: (forall x. ReaderT env m x -> n x)
  -- ^ Constructor
  -> (forall x. n x -> ReaderT env m x)
  -- ^ Destructor
  -> ((forall s. EventBackend n r s -> EventBackend n r s) -> env -> env)
  -- ^ Modify backend within env
  -> (forall s. EventBackend n r s -> EventBackend n r s)
  -> n a
  -> n a
localBackendReaderT wrap unwrap zoom f go =
  wrap $ ReaderT \env -> runReaderT (unwrap go) $ zoom f env

askBackendReaderT
  :: (Monad m)
  => (forall x. ReaderT env m x -> n x)
  -- ^ Constructor
  -> (env -> EventBackend n r s)
  -- ^ Access root backend from env
  -> n (EventBackend n r s)
askBackendReaderT wrap get = wrap $ asks get

composeInjectSelector :: InjectSelector t u -> InjectSelector s t -> InjectSelector s u
composeInjectSelector tu st s withInjF = st s \t injF -> tu t \u injG -> withInjF u $ injG . injF
