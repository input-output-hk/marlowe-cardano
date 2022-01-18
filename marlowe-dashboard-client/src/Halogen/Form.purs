-- TODO move this to its own library
module Halogen.Form
  ( AsyncForm
  , AsyncFormSpec
  , AsyncInput(..)
  , Form(..)
  , FormHTML
  , FormResult
  , FormSpec
  , mkForm
  , mkAsyncForm
  , hoistForm
  , module FormM
  , multi
  , multiWithIndex
  , split
  , subform
  , useForm
  ) where

import Prelude

import Control.Monad.Free (Free, substFree)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT(..), mapWriterT)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either, hush)
import Data.Foldable (for_)
import Data.Lens (Lens', _1, _2, set, view)
import Data.Lens.Index (class Index, ix)
import Data.Maybe (Maybe(..))
import Data.Newtype (over, unwrap)
import Data.Profunctor.Star (Star(..))
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Component (hoistSlot)
import Halogen.Css as HC
import Halogen.Form.FormM
  ( FormF(..)
  , FormM(..)
  , hoistFormF
  , hoistFormM
  , mapInput
  , update
  ) as FormM
import Halogen.Form.FormM (FormF(..), FormM, hoistFormM, mapInput, update)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks (type (<>), Hook)
import Halogen.Hooks as Hooks
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Polyform (Reporter(..), Validator)
import Polyform.Reporter as Reporter
import Polyform.Validator (runValidator)
import Web.Event.Event (preventDefault)

-- | An array of HTML nodes used to render a form. The action type is the input
-- | type.
type FormHTML input slots m = Array (H.ComponentHTML input slots m)

-- | A Form is a specialization of a Reporter. It operates in the `FormM`
-- | monad, which allows the input to be updated imperatively, and it
-- | accumulates an array of HTML which renders the form. Composing Forms
-- | composes the rendered HTML sequentially.
type Form slots m input a =
  Reporter (FormM input m) (FormHTML input slots m) input a

-- | Input for a form which does part of its validation in an asynchronous
-- | call.
data AsyncInput input error output = AsyncInput input (RemoteData error output)

-- | A form which adds additional information to its input that carries the
-- | status of an asynchronous call.
type AsyncForm slots m input error output =
  Form slots m (AsyncInput input error output) output

-- | For internal use. Intermediate type in evaluation.
type FormEvalResult slots m input a =
  MaybeT (WriterT (FormHTML input slots m) (FormM input m)) a

-- | A specification used to create a form with the mkForm function.
type FormSpec slots m error input output =
  { validator :: Validator m error input output
  , render ::
      input -> Either error output -> FormM input m (FormHTML input slots m)
  }

-- | Create a form from a specification. Input will be validated with the
-- | provided `Validator` and rendered with the `render` callback. The results
-- | of validation will be passed to the `render` callback.
mkForm
  :: forall slots m input error output
   . Monad m
  => FormSpec slots m error input output
  -> Form slots m input output
mkForm { validator, render } = Reporter $ Star \input -> MaybeT $ WriterT $ do
  V result <- lift $ runValidator validator input
  Tuple (hush result) <$> render input result

-- | A specification used to create a form with the mkAsyncForm function.
type AsyncFormSpec slots m error input output =
  { validator :: Validator m error input output
  , render ::
      input
      -> RemoteData error output
      -> FormM input m (FormHTML input slots m)
  }

-- | Create a form from a specification. Input will be validated with the
-- | provided `Validator` and rendered with the `render` callback. The results
-- | of validation an status of any asynchronous processing will be passed to
-- | the `render` callback.
mkAsyncForm
  :: forall slots m input error output
   . Monad m
  => AsyncFormSpec slots m error input output
  -> AsyncForm slots m input error output
mkAsyncForm { validator, render } =
  Reporter $ Star \(AsyncInput input remote) -> MaybeT $ WriterT $ do
    remote' <- case remote of
      NotAsked -> do
        update (AsyncInput input Loading)
        V result <- lift $ runValidator validator input
        let newRemote = fromEither result
        update (AsyncInput input newRemote)
        pure newRemote
      r -> pure r
    mapInput (flip AsyncInput remote')
      $ Tuple (toMaybe remote') <<< mapFormHTML (flip AsyncInput remote')
          <$> render input remote'

-- | Combine two forms that operate on an produce tuples of each form's input
-- | and output, respectively. This is a monomorphisation of
-- | Data.Profunctor.Strong.splitStrong. Unfortunately, `Form` has no
-- | Profunctor instance, which means it can't have `Strong` or `Choice`
-- | instances either.
split
  :: forall slots m input1 input2 output1 output2
   . Monad m
  => Form slots m input1 output1
  -> Form slots m input2 output2
  -> Form slots m (Tuple input1 input2) (Tuple output1 output2)
split f g = Tuple <$> subform _1 f <*> subform _2 g

-- | Adapt a form that operates on a value to operate on a traversable
-- | collection of values. The form depends on the index of the position in the
-- | container.
multiWithIndex
  :: forall slots m t index input output
   . TraversableWithIndex index t
  => Index (t input) index input
  => Monad m
  => (index -> Form slots m input output)
  -> Form slots m (t input) (t output)
multiWithIndex getItemForm =
  Reporter $ Star \ti -> traverseWithIndex (runItemForm ti) ti
  where
  runItemForm ti index =
    let
      Reporter (Star itemForm) = getItemForm index
    in
      mapFormEvalResult (\i -> set (ix index) i ti) <<< itemForm

-- | Adapt a form that operates on a value to operate on a traversable
-- | collection of values.
multi
  :: forall slots m t index input output
   . TraversableWithIndex index t
  => Index (t input) index input
  => Monad m
  => Form slots m input output
  -> Form slots m (t input) (t output)
multi = multiWithIndex <<< const

-- | Adapt a form to accept an input that contains its input using a Lens.
subform
  :: forall slots m input1 input2 output
   . Functor m
  => Lens' input1 input2
  -> Form slots m input2 output
  -> Form slots m input1 output
subform lens (Reporter (Star f)) = Reporter $ Star \input ->
  mapFormEvalResult (flip (set lens) input) $ f (view lens input)

-- | lift a natural transformation on the base monad to a natural
-- | transformation over a form.
hoistForm
  :: forall input slots m1 m2 output
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> Form slots m1 input output
  -> Form slots m2 input output
hoistForm alpha = over Reporter $ over Star $ map $ hoistFormEvalResult alpha

-- | Evaluate a form.
runForm
  :: forall slots m input output
   . Functor m
  => Form slots m input output
  -> input
  -> Free (FormF input m) (Tuple (Maybe output) (FormHTML input slots m))
runForm f = unwrap <<< Reporter.runReporter f

-- | Internal
mapFormHTML
  :: forall slots m input1 input2
   . Functor m
  => (input1 -> input2)
  -> FormHTML input1 slots m
  -> FormHTML input2 slots m
mapFormHTML f = map (bimap (map f) f)

-- | Internal
mapFormEvalResult
  :: forall slots m input1 input2
   . Functor m
  => (input1 -> input2)
  -> FormEvalResult slots m input1 ~> FormEvalResult slots m input2
mapFormEvalResult f =
  mapMaybeT $ mapWriterT $ mapInput f <<< map (map (mapFormHTML f))

-- | Internal
hoistFormHTML
  :: forall slots m1 m2 input
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> FormHTML input slots m1
  -> FormHTML input slots m2
hoistFormHTML alpha = map $ lmap $ hoistSlot alpha

-- | Internal
hoistFormEvalResult
  :: forall slots m1 m2 input
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> FormEvalResult slots m1 input ~> FormEvalResult slots m2 input
hoistFormEvalResult alpha =
  mapMaybeT $ mapWriterT $ hoistFormM alpha <<< map (map (hoistFormHTML alpha))

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
  :: forall slots m input output
   . MonadAff m
  => Eq input
  => Eq output
  => Form slots m input output
  -> input
  -> Hook m (UseForm slots m input output) (FormResult slots m output)
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
          interpretFormF :: FormF input m ~> Hooks.HookM m
          interpretFormF (Update newInput next) = do
            withVersionCheck initialVersion 0 versionAVar do
              Hooks.put inputId newInput
              liftEffect $ Ref.modify_ (add 1) nextVersionRef
            pure next

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
