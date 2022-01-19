-- TODO move this to its own library
module Halogen.Form.FormM where

import Prelude

import Control.Apply (lift2)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Lens (Lens', over)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

newtype FormM :: forall k. Type -> (k -> Type) -> Type -> Type
newtype FormM input m a =
  FormM (forall r. (a -> m r) -> ((input -> input) -> m r -> m r) -> m r)

update :: forall i m. i -> FormM i m Unit
update i = FormM \a up -> up (const i) (a unit)

runFormM
  :: forall input m a
   . Applicative m
  => FormM input m a
  -> ((input -> input) -> m Unit)
  -> m a
runFormM (FormM k) handleUpdate = k pure \input mr -> handleUpdate input *> mr

instance Functor m => Functor (FormM i m) where
  map f (FormM k) = FormM \b up -> k (b <<< f) up

instance Apply m => Apply (FormM i m) where
  apply (FormM kf) (FormM ka) = FormM \b up ->
    kf (\f -> ka (\a -> b (f a)) up) up

instance Applicative m => Applicative (FormM i m) where
  pure a = FormM \k _ -> k a

instance Bind m => Bind (FormM i m) where
  bind (FormM k) f = FormM \b up ->
    k (\a -> case f a of FormM k' -> k' b up) up

instance Monad m => Monad (FormM i m)

instance (Apply m, Semigroup a) => Semigroup (FormM i m a) where
  append = lift2 append

instance (Applicative m, Monoid a) => Monoid (FormM i m a) where
  mempty = pure mempty

instance MonadTrans (FormM i) where
  lift m = FormM \pure' _ -> pure' =<< m

instance MonadEffect m => MonadEffect (FormM i m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (FormM i m) where
  liftAff = lift <<< liftAff

instance MonadThrow e m => MonadThrow e (FormM i m) where
  throwError = lift <<< throwError

instance MonadTell w m => MonadTell w (FormM i m) where
  tell = lift <<< tell

instance MonadAsk r m => MonadAsk r (FormM i m) where
  ask = lift ask

instance MonadReader r m => MonadReader r (FormM i m) where
  local f = hoistFormM (local f)

instance MonadState s m => MonadState s (FormM i m) where
  state = lift <<< state

zoom :: forall i j m. Functor m => Lens' i j -> FormM j m ~> FormM i m
zoom l (FormM k) = FormM \a up -> k a $ up <<< over l

hoistFormM
  :: forall i m1 m2
   . Monad m1
  => Monad m2
  => (m1 ~> m2)
  -> FormM i m1 ~> FormM i m2
hoistFormM phi (FormM m) =
  FormM \ka up ->
    join $ phi $ m (pure <<< ka) (\i -> pure <<< up i <<< join <<< phi)
