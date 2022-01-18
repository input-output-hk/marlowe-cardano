-- TODO move this to its own library
module Halogen.Form.FormM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Free.Class (class MonadFree, wrapFree)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Newtype (class Newtype, over)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

data FormF input m a
  = Update input a
  | Lift (m a)

newtype FormM input m a = FormM (Free (FormF input m) a)

derive instance Functor m => Functor (FormF input m)

update :: forall i m. Applicative m => i -> FormM i m Unit
update i = wrapFree $ Update i $ pure unit

derive instance Newtype (FormM i m a) _
derive instance Functor m => Functor (FormM i m)
derive newtype instance Apply m => Apply (FormM i m)
derive newtype instance Applicative m => Applicative (FormM i m)
derive newtype instance Functor m => MonadFree (FormF i m) (FormM i m)

derive newtype instance Bind m => Bind (FormM i m)
instance Monad m => Monad (FormM i m)
derive newtype instance MonadRec m => MonadRec (FormM i m)

derive newtype instance (Apply m, Semigroup a) => Semigroup (FormM i m a)

derive newtype instance (Applicative m, Monoid a) => Monoid (FormM i m a)

instance MonadTrans (FormM i) where
  lift = FormM <<< liftF <<< Lift

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
  local f = over FormM $ hoistFree (hoistFormF (local f))

instance MonadState s m => MonadState s (FormM i m) where
  state = lift <<< state

mapInput
  :: forall i j m
   . Functor m
  => (i -> j)
  -> FormM i m ~> FormM j m
mapInput f = over FormM $ hoistFree case _ of
  Update j a -> Update (f j) a
  Lift m -> Lift m

hoistFormM
  :: forall i m1 m2
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> FormM i m1 ~> FormM i m2
hoistFormM a = over FormM $ hoistFree (hoistFormF a)

hoistFormF :: forall i m n. (m ~> n) -> FormF i m ~> FormF i n
hoistFormF _ (Update i a) = Update i a
hoistFormF a (Lift m) = Lift $ a m
