-- TODO move this to its own library
module Halogen.Form.FormM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, foldFree, hoistFree, substFree)
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
import Halogen (HalogenF, HalogenM(..))
import Halogen as H

data FormF input m a
  = Update (input -> input) a
  | Lift (m a)

instance Functor m => Functor (FormF input m) where
  map f (Update s a) = Update s (f a)
  map f (Lift m) = Lift $ map f m

newtype FormM input m a = FormM (Free (FormF input m) a)

runFormMHalogenM
  :: forall state action slots output input m a
   . MonadRec m
  => FormM input m a
  -> ((input -> input) -> HalogenM state action slots output m Unit)
  -> HalogenM state action slots output m a
runFormMHalogenM (FormM f) handleUpdate = HalogenM $ substFree interpretFormF f
  where
  interpretFormF
    :: (FormF input m) ~> Free (HalogenF state action slots output m)
  interpretFormF (Update s a) = case handleUpdate s of
    HalogenM freeHalogenF -> freeHalogenF $> a
  interpretFormF (Lift m) = Free.liftF $ H.Lift m

runFormM
  :: forall input m a
   . MonadRec m
  => FormM input m a
  -> ((input -> input) -> m Unit)
  -> m a
runFormM (FormM f) handleUpdate = foldFree interpretFormF f
  where
  interpretFormF :: (FormF input m) ~> m
  interpretFormF (Update s a) = handleUpdate s $> a
  interpretFormF (Lift m) = m

liftF :: forall input m. FormF input m ~> FormM input m
liftF = FormM <<< Free.liftF

update :: forall input m. input -> FormM input m Unit
update i = liftF $ Update (const i) unit

zoomFormF :: forall i j m. Functor m => Lens' i j -> FormF j m ~> FormF i m
zoomFormF l (Update s a) = Update (over l s) a
zoomFormF _ (Lift m) = Lift m

zoom :: forall i j m. Functor m => Lens' i j -> FormM j m ~> FormM i m
zoom l (FormM f) = FormM $ hoistFree (zoomFormF l) f

hoistFormF :: forall i m1 m2. (m1 ~> m2) -> FormF i m1 ~> FormF i m2
hoistFormF _ (Update s a) = Update s a
hoistFormF phi (Lift m) = Lift $ phi m

hoistFormM :: forall i m1 m2. (m1 ~> m2) -> FormM i m1 ~> FormM i m2
hoistFormM phi (FormM m) = FormM $ hoistFree (hoistFormF phi) m

derive instance Newtype (FormM i m a) _

derive newtype instance Functor m => Functor (FormM i m)

derive newtype instance Apply m => Apply (FormM i m)

derive newtype instance Applicative m => Applicative (FormM i m)

derive newtype instance Bind m => Bind (FormM i m)

instance Monad m => Monad (FormM i m)

derive newtype instance MonadRec (FormM i m)

derive newtype instance (Apply m, Semigroup a) => Semigroup (FormM i m a)

derive newtype instance (Applicative m, Monoid a) => Monoid (FormM i m a)

instance MonadTrans (FormM i) where
  lift = liftF <<< Lift

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
  local = hoistFormM <<< local

instance MonadState s m => MonadState s (FormM input m) where
  state = lift <<< state
