-- TODO move this to its own library
module Forms.FormM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree, hoistFree)
import Control.Monad.Free as Free
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, state)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Lens (Lens', over)
import Data.Newtype (class Newtype)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

data FormF state payload m a
  = UpdateFormData (payload -> payload) a
  | UpdateState (state -> state) a
  | Lift (m a)

instance Functor m => Functor (FormF state payload m) where
  map f (UpdateFormData s a) = UpdateFormData s (f a)
  map f (Lift m) = Lift $ map f m
  map f (UpdateState s a) = UpdateState s (f a)

newtype FormM state payload m a = FormM (Free (FormF state payload m) a)

mapUpdateFormDataFn
  :: forall a m payload payload' state
   . ((payload -> payload) -> (payload' -> payload'))
  -> FormM state payload m a
  -> FormM state payload' m a
mapUpdateFormDataFn f (FormM h) = FormM (hoistFree go h)
  where
  go :: FormF state payload m ~> FormF state payload' m
  go = case _ of
    UpdateFormData u a -> UpdateFormData (f u) a
    UpdateState s a -> UpdateState s a
    Lift m -> Lift m

mapUpdateStateFn
  :: forall a m payload state state'
   . ((state -> state) -> (state' -> state'))
  -> FormM state payload m a
  -> FormM state' payload m a
mapUpdateStateFn f (FormM h) = FormM (hoistFree go h)
  where
  go :: FormF state payload m ~> FormF state' payload m
  go = case _ of
    UpdateFormData u a -> UpdateFormData u a
    UpdateState s a -> UpdateState (f s) a
    Lift m -> Lift m

runFormM
  :: forall a payload m state
   . MonadRec m
  => FormM state payload m a
  -> ((payload -> payload) -> m Unit)
  -> ((state -> state) -> m Unit)
  -> m a
runFormM (FormM f) handleUpdateFormData handleUpdateState = foldFree
  interpretFormF
  f
  where
  interpretFormF :: (FormF state payload m) ~> m
  interpretFormF (UpdateFormData s a) = handleUpdateFormData s $> a
  interpretFormF (UpdateState s a) = handleUpdateState s $> a
  interpretFormF (Lift m) = m

liftF :: forall payload m state. FormF state payload m ~> FormM state payload m
liftF = FormM <<< Free.liftF

update :: forall payload m state. payload -> FormM state payload m Unit
update p = liftF $ UpdateFormData (const p) unit

zoomFormF
  :: forall a b m s t
   . Functor m
  => Lens' s a
  -> Lens' t b
  -> FormF a b m ~> FormF s t m
zoomFormF _ l (UpdateFormData s a) = UpdateFormData (over l s) a
zoomFormF l _ (UpdateState s a) = UpdateState (over l s) a
zoomFormF _ _ (Lift m) = Lift m

zoom
  :: forall a b m s t
   . Functor m
  => Lens' s a
  -> Lens' t b
  -> FormM a b m ~> FormM s t m
zoom l k (FormM f) = FormM $ hoistFree (zoomFormF l k) f

hoistFormF :: forall p m1 m2 s. (m1 ~> m2) -> FormF s p m1 ~> FormF s p m2
hoistFormF _ (UpdateFormData s a) = UpdateFormData s a
hoistFormF _ (UpdateState s a) = UpdateState s a
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

instance MonadState s' m => MonadState s' (FormM s p m) where
  state = lift <<< state

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
-- runFormMHalogenM (FormM f) handleUpdateFormData handleUpdateState = HalogenM $ substFree interpretFormF f
--   where
--   interpretFormF
--     :: (FormF state payload m) ~> Free (HalogenF state' action slots output m)
--   interpretFormF (UpdateFormData s a) = case handleUpdateFormData s of
--     HalogenM freeHalogenF -> freeHalogenF $> a
--   interpretFormF (UpdateState s) = case handleUpdateState s of
--     HalogenM freeHalogenF -> freeHalogenF
--   interpretFormF (Lift m) = Free.liftF $ H.Lift m
