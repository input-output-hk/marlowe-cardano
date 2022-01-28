-- TODO move this to its own library
module Halogen.Form
  ( Action(..)
  , AsyncForm
  , AsyncFormSpec
  , AsyncInput(..)
  , Form(..)
  , FormHTML
  , FormSpec
  , decorate
  , hoistForm
  , html
  , mkAsyncForm
  , mkForm
  , runForm
  , runFormHalogenM
  , sequenceForms
  , split
  , subform
  , traverseForms
  , traverseFormsWithIndex
  , update
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT)
import Control.Monad.Rec.Class (class MonadRec)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT(..), mapWriterT)
import Control.Plus (class Plus)
import Data.Bifunctor (bimap, lmap)
import Data.Either (Either, hush)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Lens (Lens, Lens', _1, _2, iso, lens, preview, set, view)
import Data.Lens as Lens
import Data.Lens.Index (class Index, ix)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, over)
import Data.Profunctor.Star (Star(..))
import Data.Show.Generic (genericShow)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Validation.Semigroup (V(..))
import Halogen (HalogenM)
import Halogen as H
import Halogen.Component (hoistSlot)
import Halogen.Form.FormM (FormM, hoistFormM, runFormM, runFormMHalogenM, zoom)
import Halogen.Form.FormM (update) as FormM
import Network.RemoteData (RemoteData(..), fromEither, toMaybe)
import Polyform (Reporter(..), Validator)
import Polyform.Reporter (R, liftFnR, lmapReporter)
import Polyform.Reporter as Reporter
import Polyform.Validator (runValidator)

data Action parentAction input
  = Update (input -> input)
  | Raise parentAction

update
  :: forall parentAction input
   . input
  -> Action parentAction input
update = Update <<< const

-- | An array of HTML nodes used to render a form. The action type is the input
-- | type.
type FormHTML parentAction input slots m =
  Array (H.ComponentHTML (Action parentAction input) slots m)

-- | A Form is a specialization of a Reporter. It operates in the `FormM`
-- | monad, which allows the input to be updated imperatively, and it
-- | accumulates an array of HTML which renders the form. Composing Forms
-- | composes the rendered HTML sequentially.
newtype Form parentAction slots m input a = Form
  ( Reporter (FormM input m) (FormHTML parentAction input slots m) input a
  )

derive instance Newtype (Form pa s m i o) _

derive instance Functor m => Functor (Form pa s m i)

derive newtype instance Monad m => Apply (Form pa s m i)

-- NOTE: This instance must be manually specified. There is a bug in the
-- instance resolution for PureScript when an instance is declared using
-- newtype deriving if it has a manually declared supertype. The nested
-- supertype instance will be the superclass of the _wrapped_ type.
--
-- In this context, `Reporter` declares a manual instance for `Apply` (Star's
-- instance does not provide the same semantics). However, it uses newtype
-- deriving for its `Applicative` instance. When this is requested, `Star`'s
-- applicative instance will be used, which will contain `Star`'s `Apply`
-- instance, not `Reporter`'s.
--
-- Probably the best solution in PureScript to tackle this problem is to
-- disallow the use of `derive newtype` for classes if a superclass has an
-- explicit instance.
instance Monad m => Applicative (Form pa s m i) where
  pure = Form <<< pure

derive newtype instance Monad m => Alt (Form pa s m i)

derive newtype instance Monad m => Plus (Form pa s m i)

-- | Input for a form which does part of its validation in an asynchronous
-- | call.
data AsyncInput input error output = AsyncInput input (RemoteData error output)

derive instance eqAsyncInput :: (Eq i, Eq e, Eq a) => Eq (AsyncInput i e a)
derive instance genericAsyncInput :: Generic (AsyncInput i e a) _
derive instance functorAsyncInput :: Functor (AsyncInput i e)
instance showAsyncInput :: (Show i, Show e, Show a) => Show (AsyncInput i e a) where
  show = genericShow

-- | A form which adds additional information to its input that carries the
-- | status of an asynchronous call.
type AsyncForm parentAction slots m input error output =
  Form parentAction slots m (AsyncInput input error output) output

-- | For internal use. Intermediate type in evaluation.
type FormEvalResult parentAction slots m input a =
  MaybeT (WriterT (FormHTML parentAction input slots m) (FormM input m)) a

-- | Create a form that just renders the given static HTML.
html
  :: forall parentAction slots m input
   . Monad m
  => H.ComponentHTML (Action parentAction input) slots m
  -> Form parentAction slots m input Unit
html = Form <<< liftFnR <<< const <<< Tuple (Just unit) <<< pure

-- | Create a form that wraps the view produced by another form.
decorate
  :: forall parentAction slots m input output
   . Monad m
  => ( FormHTML parentAction input slots m
       -> H.ComponentHTML (Action parentAction input) slots m
     )
  -> Form parentAction slots m input output
  -> Form parentAction slots m input output
decorate = over Form <<< lmapReporter <<< map pure

-- | A specification used to create a form with the mkForm function.
type FormSpec parentAction slots m error input output =
  { validator :: Validator m error input output
  , render ::
      input
      -> Either error output
      -> FormM input m (FormHTML parentAction input slots m)
  }

-- | Create a form from a specification. Input will be validated with the
-- | provided `Validator` and rendered with the `render` callback. The results
-- | of validation will be passed to the `render` callback.
mkForm
  :: forall parentAction slots m input error output
   . Monad m
  => FormSpec parentAction slots m error input output
  -> Form parentAction slots m input output
mkForm { validator, render } =
  Form $ Reporter $ Star \input -> MaybeT $ WriterT do
    V result <- lift $ runValidator validator input
    Tuple (hush result) <$> render input result

-- | A specification used to create a form with the mkAsyncForm function.
type AsyncFormSpec parentAction slots m error input output =
  { validator :: Validator m error input output
  , render ::
      input
      -> RemoteData error output
      -> FormM input m (FormHTML parentAction input slots m)
  }

_value
  :: forall input1 input2 error output
   . Lens
       (AsyncInput input1 error output)
       (AsyncInput input2 error output)
       input1
       input2
_value =
  lens
    (\(AsyncInput value _) -> value)
    \(AsyncInput _ remote) value -> AsyncInput value remote

-- | Create a form from a specification. Input will be validated with the
-- | provided `Validator` and rendered with the `render` callback. The results
-- | of validation an status of any asynchronous processing will be passed to
-- | the `render` callback.
mkAsyncForm
  :: forall parentAction slots m input error output
   . Monad m
  => AsyncFormSpec parentAction slots m error input output
  -> AsyncForm parentAction slots m input error output
mkAsyncForm { validator, render } =
  Form $ Reporter $ Star \(AsyncInput input remote) -> MaybeT $ WriterT $ do
    remote' <- case remote of
      NotAsked -> do
        FormM.update (AsyncInput input Loading)
        V result <- lift $ runValidator validator input
        let newRemote = fromEither result
        FormM.update (AsyncInput input newRemote)
        pure newRemote
      r -> pure r
    zoom _value
      $ Tuple (toMaybe remote') <<< subformHTML unlawfulResetValue <$> render
          input
          remote'
  where
  -- We use an unlawful ISO here because a side-effect of updating the input
  -- from the UI is that any previous remote results or progress is discarded.
  unlawfulResetValue =
    iso (\(AsyncInput value _) -> value) \value -> AsyncInput value NotAsked

-- | Combine two forms that operate on an produce tuples of each form's input
-- | and output, respectively. This is a monomorphisation of
-- | Data.Profunctor.Strong.splitStrong. Unfortunately, `Form` has no
-- | Profunctor instance, which means it can't have `Strong` or `Choice`
-- | instances either.
split
  :: forall parentAction slots m input1 input2 output1 output2
   . Monad m
  => Form parentAction slots m input1 output1
  -> Form parentAction slots m input2 output2
  -> Form parentAction slots m (Tuple input1 input2) (Tuple output1 output2)
split f g = Tuple <$> subform _1 f <*> subform _2 g

-- | Adapt a form that operates on a value to operate on a traversable
-- | collection of values. The form depends on the index of the position in the
-- | container.
sequenceForms
  :: forall parentAction slots m t index input output
   . TraversableWithIndex index t
  => Index (t input) index input
  => Monad m
  => Monoid input
  => t (Form parentAction slots m input output)
  -> Form parentAction slots m (t input) (t output)
sequenceForms = traverseWithIndex go
  where
  go index itemForm = subform l itemForm
    where
    traversal = ix index
    l = lens getter setter
    getter = fromMaybe mempty <<< preview traversal
    setter = flip (set traversal)

-- | Traverse a collection of items and for each produce a form. Convert the
-- | results to a form that operates on a collection of inputs.
traverseForms
  :: forall parentAction slots m t index a input output
   . TraversableWithIndex index t
  => Index (t input) index input
  => Monad m
  => Monoid input
  => (a -> Form parentAction slots m input output)
  -> t a
  -> Form parentAction slots m (t input) (t output)
traverseForms f = sequenceForms <<< map f

-- | Traverse a collection of items and for each produce a form. Convert the
-- | results to a form that operates on a collection of inputs.
traverseFormsWithIndex
  :: forall parentAction slots m t index a input output
   . TraversableWithIndex index t
  => Index (t input) index input
  => Monad m
  => Monoid input
  => (index -> a -> Form parentAction slots m input output)
  -> t a
  -> Form parentAction slots m (t input) (t output)
traverseFormsWithIndex f = sequenceForms <<< mapWithIndex f

-- | Adapt a form that operates on a value to operate on a traversable
-- | collection of values.

-- | Adapt a form to accept an input that contains its input using a Lens.
subform
  :: forall parentAction slots m input1 input2 output
   . Functor m
  => Lens' input1 input2
  -> Form parentAction slots m input2 output
  -> Form parentAction slots m input1 output
subform l (Form (Reporter (Star f))) = Form $ Reporter
  $ Star
  $ subformEvalResult l <<< f <<< view l

-- | lift a natural transformation on the base monad to a natural
-- | transformation over a form.
hoistForm
  :: forall parentAction input slots m1 m2 output
   . Monad m1
  => Monad m2
  => (m1 ~> m2)
  -> Form parentAction slots m1 input output
  -> Form parentAction slots m2 input output
hoistForm alpha =
  over Form $ over Reporter $ over Star $ map $ hoistFormEvalResult alpha

runFormHalogenM
  :: forall state action houtput parentAction slots m input output
   . MonadRec m
  => Form parentAction slots m input output
  -> input
  -> ((input -> input) -> HalogenM state action slots houtput m Unit)
  -> HalogenM
       state
       action
       slots
       houtput
       m
       (R (FormHTML parentAction input slots m) output)
runFormHalogenM (Form f) = runFormMHalogenM <<< Reporter.runReporter f

runForm
  :: forall parentAction slots m input output
   . MonadRec m
  => Form parentAction slots m input output
  -> input
  -> ((input -> input) -> m Unit)
  -> m (R (FormHTML parentAction input slots m) output)
runForm (Form f) = runFormM <<< Reporter.runReporter f

subAction
  :: forall parentAction input1 input2
   . Lens' input1 input2
  -> Action parentAction input2
  -> Action parentAction input1
subAction l = case _ of
  Update updater -> Update $ Lens.over l updater
  Raise parentAction -> Raise parentAction

-- | Internal
subformHTML
  :: forall parentAction slots m input1 input2
   . Functor m
  => Lens' input1 input2
  -> FormHTML parentAction input2 slots m
  -> FormHTML parentAction input1 slots m
subformHTML l = map (bimap (map (subAction l)) (subAction l))

-- | Internal
subformEvalResult
  :: forall parentAction slots m input1 input2
   . Functor m
  => Lens' input1 input2
  -> FormEvalResult parentAction slots m input2
       ~> FormEvalResult parentAction slots m input1
subformEvalResult l =
  mapMaybeT $ mapWriterT $ zoom l <<< map (map (subformHTML l))

-- | Internal
hoistFormHTML
  :: forall parentAction slots m1 m2 input
   . Functor m1
  => Functor m2
  => (m1 ~> m2)
  -> FormHTML parentAction input slots m1
  -> FormHTML parentAction input slots m2
hoistFormHTML alpha = map $ lmap $ hoistSlot alpha

-- | Internal
hoistFormEvalResult
  :: forall parentAction slots m1 m2 input
   . Monad m1
  => Monad m2
  => (m1 ~> m2)
  -> FormEvalResult parentAction slots m1 input
       ~> FormEvalResult parentAction slots m2 input
hoistFormEvalResult alpha =
  mapMaybeT $ mapWriterT $ hoistFormM alpha <<< map (map (hoistFormHTML alpha))
