-- TODO move this to its own library
module Halogen.Form.FormM where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State (class MonadState, StateT, gets, mapStateT)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (class MonadTell, tell)
import Data.Lens (appendModifying)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, over, unwrap)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)

type FormState = SemigroupMap String (Additive Int)

data FormF input m a
  = SetInput input a
  | Lift (m a)

derive instance functorFormF :: Functor m => Functor (FormF input m)

newtype FormM :: forall k. k -> Type -> (Type -> Type) -> Type -> Type
newtype FormM slots input m a =
  FormM (StateT FormState (Free (FormF input m)) a)

setInput :: forall s i m. i -> FormM s i m Unit
setInput i = FormM $ lift $ liftF $ SetInput i unit

derive instance newtypeFormM :: Newtype (FormM s i m a) _
derive instance functorFormM :: Functor m => Functor (FormM s i m)
derive newtype instance applyFormM :: Apply m => Apply (FormM s i m)
derive newtype instance applicativeFormM ::
  Applicative m =>
  Applicative (FormM s i m)

derive newtype instance bindFormM :: Bind m => Bind (FormM s i m)
instance monadFormM :: Monad m => Monad (FormM s i m)
derive newtype instance monadRecFormM :: MonadRec m => MonadRec (FormM s i m)

derive newtype instance semigroupFormM ::
  ( Apply m
  , Semigroup a
  ) =>
  Semigroup (FormM s i m a)

derive newtype instance monoidFormM ::
  ( Applicative m
  , Monoid a
  ) =>
  Monoid (FormM s i m a)

instance monadTransFormM :: MonadTrans (FormM s i) where
  lift = FormM <<< lift <<< liftF <<< Lift

instance monadEffectFormM :: MonadEffect m => MonadEffect (FormM s i m) where
  liftEffect = lift <<< liftEffect

instance monadAffFormM :: MonadAff m => MonadAff (FormM s i m) where
  liftAff = lift <<< liftAff

instance monadThrowFormM :: MonadThrow e m => MonadThrow e (FormM s i m) where
  throwError = lift <<< throwError

instance monadtellFormM :: MonadTell w m => MonadTell w (FormM s i m) where
  tell = lift <<< tell

instance monadAskFormM :: MonadAsk r m => MonadAsk r (FormM s i m) where
  ask = lift ask

instance monadReaderFormM :: MonadReader r m => MonadReader r (FormM s i m) where
  local f =
    over FormM
      $ mapStateT
      $ hoistFree (hoistFormF (local f))

derive newtype instance monadStateFormM ::
  Monad m =>
  MonadState FormState (FormM sl i m)

uniqueId :: forall s i m. Monad m => String -> FormM s i m String
uniqueId candidate = do
  count <- gets $ Map.lookup candidate <<< unwrap
  appendModifying identity (SemigroupMap (Map.singleton candidate (Additive 1)))
  pure $ candidate <> case count of
    Nothing -> ""
    Just (Additive i) -> "-" <> show i

hoistFormF :: forall i m n. (m ~> n) -> FormF i m ~> FormF i n
hoistFormF _ (SetInput i a) = SetInput i a
hoistFormF a (Lift m) = Lift $ a m
