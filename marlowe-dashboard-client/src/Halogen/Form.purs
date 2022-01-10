-- TODO move this to its own library
module Halogen.Form
  ( Form(..)
  , FormF
  , FormM(..)
  , FormState
  , FormResult
  , form
  , useForm
  , hoistForm
  , nest
  , setInput
  , split
  , subform
  , uniqueId
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free, hoistFree, liftF, substFree)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT)
import Control.Monad.Reader (class MonadAsk, class MonadReader, ask, local)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.State
  ( class MonadState
  , StateT
  , evalStateT
  , gets
  , mapStateT
  )
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Control.Monad.Writer (WriterT(..), censor, mapWriterT)
import Data.Bifunctor (bimap, lmap)
import Data.Foldable (for_)
import Data.Lens (Lens', _1, _2, appendModifying, set, view)
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (class Newtype, over, unwrap)
import Data.Profunctor.Star (Star(..))
import Data.Tuple (Tuple(..))
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Component (hoistSlot)
import Halogen.Css as HC
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (type (<>), Hook)
import Halogen.Hooks as Hooks
import Polyform (Reporter(..))
import Polyform.Reporter (R)
import Polyform.Reporter as Reporter
import Web.Event.Event (preventDefault)

type FormState = SemigroupMap String (Additive Int)

type Form slots m input a =
  Reporter
    (StateT FormState (Free (FormF input m)))
    (Array (H.ComponentHTML input slots m))
    input
    a

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

nest
  :: forall s m i a
   . Monad m
  => (Array (H.ComponentHTML i s m) -> H.ComponentHTML i s m)
  -> Form s m i a
  -> Form s m i a
nest parent = over Reporter $ over Star $ map $ censor (pure <<< parent)

form
  :: forall s m i a
   . Functor m
  => (i -> FormM s i m (R (Array (H.ComponentHTML i s m)) a))
  -> Form s m i a
form = Reporter <<< Star <<< map (MaybeT <<< WriterT <<< unwrap)

uniqueId :: forall m. MonadState FormState m => String -> m String
uniqueId candidate = do
  count <- gets $ Map.lookup candidate <<< unwrap
  appendModifying identity (SemigroupMap (Map.singleton candidate (Additive 1)))
  pure $ candidate <> case count of
    Nothing -> ""
    Just (Additive i) -> "-" <> show i

split
  :: forall s m i j a b
   . Monad m
  => Form s m i a
  -> Form s m j b
  -> Form s m (Tuple i j) (Tuple a b)
split f g = Tuple <$> subform _1 f <*> subform _2 g

subform
  :: forall s m i j a
   . Functor m
  => Lens' i j
  -> Form s m j a
  -> Form s m i a
subform lens (Reporter (Star f)) = Reporter $ Star \i ->
  let
    adaptAction j = set lens j i

    adaptFormF :: FormF j m ~> FormF i m
    adaptFormF (SetInput j a) = SetInput (set lens j i) a
    adaptFormF (Lift m) = Lift m
  in
    mapMaybeT
      ( mapWriterT
          ( mapStateT
              ( hoistFree adaptFormF <<< map
                  (lmap (map (map (bimap (map adaptAction) adaptAction))))
              )
          )
      )
      (f (view lens i))

hoistFormF :: forall i m n. (m ~> n) -> FormF i m ~> FormF i n
hoistFormF _ (SetInput i a) = SetInput i a
hoistFormF a (Lift m) = Lift $ a m

hoistForm
  :: forall i s m1 m2 a
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> Form s m1 i a
  -> Form s m2 i a
hoistForm a =
  over Reporter
    $ over Star
    $ map
    $ mapMaybeT
    $ mapWriterT
    $ mapStateT
    $ hoistFree (hoistFormF a) <<< map hoistR
  where
  hoistR = lmap $ map $ map $ lmap $ hoistSlot a

runForm
  :: forall s m i a
   . Functor m
  => Form s m i a
  -> i
  -> Free (FormF i m) (Tuple (Maybe a) (Array (H.ComponentHTML i s m)))
runForm f = flip evalStateT mempty <<< Reporter.runReporter f

type UseForm slots m input output =
  Hooks.UseState input
    <> Hooks.UseState (Maybe output)
    <> Hooks.UseState (Array (H.ComponentHTML input slots m))
    <> Hooks.UseState (Maybe (AVar Int))
    <> Hooks.UseEffect
    <> Hooks.UseEffect
    <> Hooks.Pure

type FormResult slots m a =
  { html :: Array String -> H.ComponentHTML (Hooks.HookM m Unit) slots m
  , result :: Maybe a
  }

useForm
  :: forall s m i a
   . MonadAff m
  => Eq i
  => Show i
  => Eq a
  => Form s m i a
  -> i
  -> Hook m (UseForm s m i a) (FormResult s m a)
useForm f initialInput = Hooks.do
  Tuple input inputId <- Hooks.useState initialInput
  Tuple result resultId <- Hooks.useState Nothing
  Tuple children childrenId <- Hooks.useState []
  Tuple versionAVar versionAVarId <- Hooks.useState Nothing
  Hooks.useLifecycleEffect do
    versionAVar' <- liftAff $ AVar.new 0
    Hooks.put versionAVarId $ Just versionAVar'
    pure Nothing
  handleInputChange { input, versionAVar, inputId, resultId, childrenId }
  let handleAction = Hooks.put inputId
  Hooks.pure
    { html: \classNames ->
        HH.form
          [ HC.classNames classNames
          , HE.onSubmit $ liftEffect <<< preventDefault
          ]
          $ bimap (map handleAction) handleAction <$> children
    , result
    }
  where
  eqInputDeps { versionAVar: Nothing } { versionAVar: Just _ } = false
  eqInputDeps { versionAVar: Just _ } { versionAVar: Nothing } = false
  eqInputDeps deps1 deps2 = deps1.input == deps2.input
  handleInputChange deps =
    Hooks.capturesWith eqInputDeps deps Hooks.useTickEffect do
      for_ deps.versionAVar \versionAVar -> do
        let { input, inputId, resultId, childrenId } = deps
        initialVersion <- liftAff $ AVar.read versionAVar
        nextVersionRef <- liftEffect $ Ref.new initialVersion
        let
          interpretFormF :: FormF i m ~> Hooks.HookM m
          interpretFormF (SetInput i a) = do
            withVersionCheck initialVersion 0 versionAVar do
              Hooks.put inputId i
              liftEffect $ Ref.modify_ (add 1) nextVersionRef
            pure a

          interpretFormF (Lift m) = lift m

        Tuple result children <- Hooks.HookM
          $ substFree (interpretFormF >>> case _ of Hooks.HookM free -> free)
          $ runForm f input
        nextVersion <- liftEffect $ Ref.read nextVersionRef
        withVersionCheck initialVersion (nextVersion + 1) versionAVar do
          Hooks.put childrenId children
          Hooks.put resultId result
      pure Nothing

  withVersionCheck
    :: forall n. MonadAff n => Int -> Int -> AVar Int -> n Unit -> n Unit
  withVersionCheck initialVersion increment versionAVar g = do
    version <- liftAff $ AVar.take versionAVar
    when (version == initialVersion) g
    liftAff $ AVar.put (version + increment) versionAVar
