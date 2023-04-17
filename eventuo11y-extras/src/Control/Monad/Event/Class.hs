{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}

module Control.Monad.Event.Class
  where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Identity (IdentityT)
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.With (MonadWithExceptable)
import Observe.Event (Event, EventBackend, InjectSelector, NewEventArgs(..))
import Observe.Event.Backend
  (Event(..), hoistEventBackend, narrowEventBackend, setAncestorEventBackend, simpleNewEventArgs)
import qualified Observe.Event.Explicit as Explicit
import Unsafe.Coerce (unsafeCoerce)

class MonadWithExceptable m => MonadEvent r s m where
  askBackend :: m (EventBackend m r s)
  localBackend :: (EventBackend m r s -> EventBackend m r s) -> m a -> m a

instance MonadEvent r s m => MonadEvent r s (IdentityT m) where
  askBackend = unsafeCoerce $ askBackend @r @s @m
  localBackend = unsafeCoerce $ localBackend @r @s @m

instance MonadEvent r s m => MonadEvent r s (ReaderT r' m) where
  askBackend = hoistEventBackend lift <$> lift askBackend
  localBackend f m = ReaderT \r -> localBackend
    (hoistEventBackend (flip runReaderT r) . f . hoistEventBackend lift)
    (runReaderT m r)

emitImmediateEventArgs :: MonadEvent r t m => InjectSelector s t -> NewEventArgs r s f -> m r
emitImmediateEventArgs inject NewEventArgs{..} = do
  backend <- askBackend
  inject newEventSelector \sel injField -> Explicit.emitImmediateEvent backend NewEventArgs
    { newEventSelector = sel
    , newEventInitialFields = injField <$> newEventInitialFields
    , ..
    }

emitImmediateEvent :: MonadEvent r t m => InjectSelector s t -> s f -> m r
emitImmediateEvent inject = emitImmediateEventArgs inject . simpleNewEventArgs

withEventArgs
  :: forall r s t f m a
   . MonadEvent r t m
  => InjectSelector s t
  -> NewEventArgs r s f
  -> (Event m r f -> m a)
  -> m a
withEventArgs inject args f = do
  backend <- askBackend
  Explicit.withEventArgs (narrowEventBackend inject backend) args \ev ->
    localBackend @r @t (setAncestorEventBackend (reference ev)) $ f ev

withEvent :: MonadEvent r t m => InjectSelector s t -> s f -> (Event m r f -> m a) -> m a
withEvent inject = withEventArgs inject . simpleNewEventArgs

newEventArgs :: MonadEvent r t m => InjectSelector s t -> NewEventArgs r s f -> m (Event m r f)
newEventArgs inject args = do
  backend <- askBackend
  Explicit.newEvent (narrowEventBackend inject backend) args

newEvent :: MonadEvent r t m => InjectSelector s t -> s f -> m (Event m r f)
newEvent inject = newEventArgs inject . simpleNewEventArgs

composeInjectSelector :: InjectSelector b c -> InjectSelector a b -> InjectSelector a c
composeInjectSelector injBC injAB a k = injAB a \b injFG -> injBC b \c -> k c . (. injFG)
