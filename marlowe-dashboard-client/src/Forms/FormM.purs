-- TODO move this to its own library
module Forms.FormM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree, hoistFree)
import Control.Monad.Free as Free
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Lens (Lens', over)
import Data.Newtype (class Newtype)
import Data.Profunctor.Strong ((***))
import Data.Tuple.Nested (type (/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

type StateFn state a = state -> a /\ state

data FormF state payload m a
  = Update (payload -> payload) a
  | State (StateFn state a)
  | Lift (m a)

instance Functor m => Functor (FormF state payload m) where
  map f (Update s a) = Update s (f a)
  map f (Lift m) = Lift $ map f m
  map f (State s) = State (s <#> f *** identity)

newtype FormM state payload m a = FormM (Free (FormF state payload m) a)

mapUpdateFn
  :: forall a m payload payload' state
   . ((payload -> payload) -> (payload' -> payload'))
  -> FormM state payload m a
  -> FormM state payload' m a
mapUpdateFn f (FormM h) = FormM (hoistFree go h)
  where
  go :: FormF state payload m ~> FormF state payload' m
  go = case _ of
    Update u a -> Update (f u) a
    State s -> State s
    Lift m -> Lift m

mapStateFn
  :: forall a m payload state state'
   . (forall a'. StateFn state a' -> StateFn state' a')
  -> FormM state payload m a
  -> FormM state' payload m a
mapStateFn f (FormM h) = FormM (hoistFree go h)
  where
  go :: FormF state payload m ~> FormF state' payload m
  go = case _ of
    Update u a -> Update u a
    State s -> State (f s)
    Lift m -> Lift m

runFormM
  :: forall a payload m state
   . MonadRec m
  => FormM state payload m a
  -> ((payload -> payload) -> m Unit)
  -> (forall a'. (state -> a' /\ state) -> m a')
  -> m a
runFormM (FormM f) handleUpdate handleState = foldFree interpretFormF f
  where
  interpretFormF :: (FormF state payload m) ~> m
  interpretFormF (Update s a) = handleUpdate s $> a
  interpretFormF (State s) = handleState s
  interpretFormF (Lift m) = m

liftF :: forall payload m state. FormF state payload m ~> FormM state payload m
liftF = FormM <<< Free.liftF

update :: forall payload m state. payload -> FormM state payload m Unit
update p = liftF $ Update (const p) unit

zoomFormF
  :: forall p q m s. Functor m => Lens' p q -> FormF s q m ~> FormF s p m
zoomFormF l (Update s a) = Update (over l s) a
zoomFormF _ (State s) = State s
zoomFormF _ (Lift m) = Lift m

zoom :: forall p q m s. Functor m => Lens' p q -> FormM s q m ~> FormM s p m
zoom l (FormM f) = FormM $ hoistFree (zoomFormF l) f

hoistFormF :: forall p m1 m2 s. (m1 ~> m2) -> FormF s p m1 ~> FormF s p m2
hoistFormF _ (Update s a) = Update s a
hoistFormF _ (State s) = State s
hoistFormF phi (Lift m) = Lift $ phi m

hoistFormM :: forall p m1 m2 s. (m1 ~> m2) -> FormM s p m1 ~> FormM s p m2
hoistFormM phi (FormM m) = FormM $ hoistFree (hoistFormF phi) m

derive instance Newtype (FormM s p m a) _

derive newtype instance Functor m => Functor (FormM s p m)

derive newtype instance Apply m => Apply (FormM s p m)

derive newtype instance Applicative m => Applicative (FormM s p m)

derive newtype instance Bind m => Bind (FormM s p m)

instance Monad m => Monad (FormM s p m)

derive newtype instance MonadRec (FormM s p m)

derive newtype instance (Apply m, Semigroup a) => Semigroup (FormM s p m a)

derive newtype instance (Applicative m, Monoid a) => Monoid (FormM s p m a)

instance MonadTrans (FormM s i) where
  lift = liftF <<< Lift

instance Monad m => MonadState s (FormM s p m) where
  state = liftF <<< State

instance MonadEffect m => MonadEffect (FormM s p m) where
  liftEffect = lift <<< liftEffect

instance MonadAff m => MonadAff (FormM s p m) where
  liftAff = lift <<< liftAff

instance MonadThrow e m => MonadThrow e (FormM s p m) where
  throwError = lift <<< throwError

instance MonadTell w m => MonadTell w (FormM s p m) where
  tell = lift <<< tell

instance MonadAsk r m => MonadAsk r (FormM s p m) where
  ask = lift ask

instance MonadReader r m => MonadReader r (FormM s p m) where
  local = hoistFormM <<< local

-- runFormMHalogenM
--   :: forall state state' action slots output payload m a
--    . MonadRec m
--   => FormM state payload m a
--   -> ((payload -> payload) -> HalogenM state' action slots output m Unit)
--   -> (forall a'. (state -> a' /\ state) -> HalogenM state' action slots output m a')
--   -> HalogenM state' action slots output m a
-- runFormMHalogenM (FormM f) handleUpdate handleState = HalogenM $ substFree interpretFormF f
--   where
--   interpretFormF
--     :: (FormF state payload m) ~> Free (HalogenF state' action slots output m)
--   interpretFormF (Update s a) = case handleUpdate s of
--     HalogenM freeHalogenF -> freeHalogenF $> a
--   interpretFormF (State s) = case handleState s of
--     HalogenM freeHalogenF -> freeHalogenF
--   interpretFormF (Lift m) = Free.liftF $ H.Lift m
