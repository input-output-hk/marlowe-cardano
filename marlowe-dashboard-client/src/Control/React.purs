module Control.React where

import Prologue

import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Base (class MonadBase)
import Control.Monad.Cont (class MonadCont, class MonadTrans)
import Control.Monad.Error.Class (class MonadError, class MonadThrow)
import Control.Monad.Fork.Class
  ( class MonadBracket
  , class MonadFork
  , class MonadKill
  , bracket
  , kill
  , never
  , uninterruptible
  )
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT(..)
  , ask
  , local
  , mapReaderT
  , runReaderT
  , withReaderT
  )
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Unlift (class MonadUnlift)
import Control.Monad.Writer (class MonadTell, class MonadWriter)
import Data.Bifunctor (bimap)
import Data.Distributive (class Distributive)
import Data.These (These(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Unlift (class MonadUnliftAff)
import Effect.Class (class MonadEffect)
import Effect.Unlift (class MonadUnliftEffect)
import Halogen.Store.Monad (class MonadStore)
import Servant.PureScript (class MonadAjax)
import Test.Web.Monad (class MonadTest)

-- | The ReactT monad transformer enables computations in the context of
-- | changing values. Computations have read-only access to the previous and
-- | next value of a piece of state. `This s` represents a change in which there
-- | is no future state (The last change),  `That s` represents a change in
-- | which there is no previous state (the first change), and `Both s1 s2`
-- | represents a change with both a past and future state.
newtype ReactT :: forall k. Type -> (k -> Type) -> k -> Type
newtype ReactT s m a = ReactT (ReaderT (These s s) m a)

runReactT :: forall s m a. ReactT s m a -> (These s s) -> m a
runReactT (ReactT r) = runReaderT r

withReactT :: forall s t m a. (t -> s) -> ReactT s m a -> ReactT t m a
withReactT f (ReactT r) = ReactT $ withReaderT (bimap f f) r

mapReactT :: forall s m n a b. (m a -> n b) -> ReactT s m a -> ReactT s n b
mapReactT f (ReactT r) = ReactT $ mapReaderT f r

-- | Run an action only on the first change.
onInitialize
  :: forall s m a. Applicative m => Monoid a => (s -> m a) -> ReactT s m a
onInitialize action = ReactT $ ReaderT case _ of
  That s -> action s
  _ -> pure mempty

-- | Run an action only on the first change.
onFinalize
  :: forall s m a. Applicative m => Monoid a => (s -> m a) -> ReactT s m a
onFinalize action = ReactT $ ReaderT case _ of
  This s -> action s
  _ -> pure mempty

-- | Always run an action
always :: forall s m a. Applicative m => (These s s -> m a) -> ReactT s m a
always = ReactT <<< ReaderT

-- | Run an action only when the value changes. Excludes finalizers.
onChange
  :: forall s m a
   . Applicative m
  => Monoid a
  => Eq s
  => (Maybe s -> s -> m a)
  -> ReactT s m a
onChange = onChangeWith eq

-- | Run an action only when the value changes according to a custom equality
-- | check. Excludes finalizers.
onChangeWith
  :: forall s m a
   . Applicative m
  => Monoid a
  => (s -> s -> Boolean)
  -> (Maybe s -> s -> m a)
  -> ReactT s m a
onChangeWith eq' f = ReactT $ ReaderT case _ of
  That s -> f Nothing s
  Both s1 s2 | not (eq' s1 s2) -> f (Just s1) s2
  _ -> pure mempty

-- | Run an action only when part of the value changes. Excludes finalizers.
onPartialChange
  :: forall s t m a
   . Applicative m
  => Monoid a
  => Eq t
  => (s -> t)
  -> (Maybe t -> t -> m a)
  -> ReactT s m a
onPartialChange = onPartialChangeWith eq

-- | Run an action only when part of the value changes according to a custom equality
-- | check. Excludes finalizers.
onPartialChangeWith
  :: forall s t m a
   . Applicative m
  => Monoid a
  => (t -> t -> Boolean)
  -> (s -> t)
  -> (Maybe t -> t -> m a)
  -> ReactT s m a
onPartialChangeWith eq' f = withReactT f <<< onChangeWith eq'

derive newtype instance Functor m => Functor (ReactT s m)
derive newtype instance Apply m => Apply (ReactT s m)
derive newtype instance Applicative m => Applicative (ReactT s m)
derive newtype instance Bind m => Bind (ReactT s m)
derive newtype instance Monad m => Monad (ReactT s m)
derive newtype instance MonadEffect m => MonadEffect (ReactT s m)
derive newtype instance MonadUnliftEffect m => MonadUnliftEffect (ReactT s m)
derive newtype instance MonadAff m => MonadAff (ReactT s m)
derive newtype instance MonadUnliftAff m => MonadUnliftAff (ReactT s m)
derive newtype instance MonadBase b m => MonadBase b (ReactT s m)
derive newtype instance MonadUnlift b m => MonadUnlift b (ReactT s m)
derive newtype instance MonadThrow e m => MonadThrow e (ReactT s m)
derive newtype instance MonadError e m => MonadError e (ReactT s m)
derive newtype instance MonadTell s m => MonadTell s (ReactT s m)
derive newtype instance MonadWriter s m => MonadWriter s (ReactT s m)
derive newtype instance MonadState s m => MonadState s (ReactT s m)
derive newtype instance MonadCont m => MonadCont (ReactT s m)
derive newtype instance MonadRec m => MonadRec (ReactT s m)
derive newtype instance MonadFork f m => MonadFork f (ReactT s m)
derive newtype instance MonadAjax api m => MonadAjax api (ReactT s m)
derive newtype instance MonadTest m => MonadTest (ReactT s m)
derive newtype instance MonadLogger l m => MonadLogger l (ReactT s m)
derive newtype instance MonadStore a s m => MonadStore a s (ReactT s m)
derive newtype instance MonadTime m => MonadTime (ReactT s m)
derive newtype instance Distributive g => Distributive (ReactT s g)
derive newtype instance MonadTrans (ReactT s)

instance MonadKill e f m => MonadKill e f (ReactT s m) where
  kill e = lift <<< kill e

instance MonadBracket e f m => MonadBracket e f (ReactT s m) where
  bracket (ReactT acquire) release run = ReactT $ bracket
    acquire
    (\c a -> case release c a of ReactT r -> r)
    (\a -> case run a of ReactT r -> r)
  uninterruptible (ReactT r) = ReactT $ uninterruptible r
  never = lift never

instance MonadAsk r m => MonadAsk r (ReactT s m) where
  ask = lift ask

instance MonadReader r m => MonadReader r (ReactT s m) where
  local f (ReactT (ReaderT r)) = ReactT $ ReaderT $ local f <<< r
