module Component.Form
  ( Action
  , Component
  , ComponentHTML
  , Input(..)
  , Query
  , Render
  , Slot
  , State
  , component
  , emit
  , renderInSlot
  , setFormProps
  , update
  , void
  , zoom
  ) where

import Prelude

import DOM.HTML.Indexed (HTMLform)
import Data.Bifunctor (bimap)
import Data.Lens (_Just, set, (^?))
import Data.Lens.AffineTraversal (AffineTraversal')
import Data.Maybe (Maybe(..))
import Data.Symbol (class IsSymbol)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML as HP
import Halogen.HTML.Events as HE
import Prim.Row as Row
import Record as Record
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event, preventDefault)

data Query (a :: Type)

instance Functor Query where
  map f = map f

type Render pa output slots m = Maybe output -> ComponentHTML pa output slots m

type Input pa output slots m =
  { load :: Maybe output
  , render :: Render pa output slots m
  }

type ComponentHTML pa output slots m =
  H.ComponentHTML (Action pa output slots m) slots m

type State pa output slots m =
  { result :: Maybe output
  , render :: Render pa output slots m
  }

data Action pa output slots m
  = OnReceive (Input pa output slots m)
  | OnUpdate (Maybe output -> Maybe output)
  | OnEmit pa
  | OnSubmit Event
  | Void

emit :: forall pa output slots m. pa -> Action pa output slots m
emit = OnEmit

update :: forall pa output slots m. Maybe output -> Action pa output slots m
update = OnUpdate <<< const

void :: forall pa output slots m. Action pa output slots m
void = Void

type Component pa output slots m =
  H.Component Query (Input pa output slots m) pa m

type Slot pa slot = H.Slot Query pa slot

type DSL pa output slots m =
  H.HalogenM (State pa output slots m) (Action pa output slots m) slots pa m

component
  :: forall pa output slots m. MonadEffect m => Component pa output slots m

component =
  H.mkComponent
    { initialState
    , render: \{ render, result } -> render result
    , eval: H.mkEval H.defaultEval
        { handleAction = case _ of
            Void -> pure unit
            OnReceive input -> H.put $ initialState input
            OnUpdate d -> H.modify_ $ Record.modify _result d
            OnEmit pa -> H.raise pa
            OnSubmit event -> H.liftEffect $ preventDefault event
        , receive = Just <<< OnReceive
        }
    }
  where
  initialState = Record.rename _load _result
  _load = Proxy :: _ "load"
  _result = Proxy :: _ "result"

setFormProps
  :: forall pa slots m output
   . Array (HP.IProp HTMLform (Action pa output slots m))
  -> Array (HP.IProp HTMLform (Action pa output slots m))
setFormProps props = props <> [ HE.onSubmit OnSubmit ]

renderInSlot
  :: forall pa slots m label slot slots' output childSlots
   . MonadEffect m
  => Row.Cons label (Slot pa slot) slots' slots
  => IsSymbol label
  => Ord slot
  => Proxy label
  -> slot
  -> Maybe output
  -> Render pa output childSlots m
  -> H.ComponentHTML pa slots m
renderInSlot label slot load render =
  HH.slot label slot component { load, render } identity

zoom
  :: forall pa output output' slots m
   . AffineTraversal' output output'
  -> ComponentHTML pa output' slots m
  -> ComponentHTML pa output slots m
zoom optic = bimap (map $ zoomAction optic) $ zoomAction optic

zoomAction
  :: forall pa output output' slots m
   . AffineTraversal' output output'
  -> Action pa output' slots m
  -> Action pa output slots m
zoomAction optic = case _ of
  OnReceive input -> OnReceive
    -- This may look odd, and it is. The problem is that we can't obtain an
    -- `output` given only an `output'` in this context. The implication here
    -- is that if any child element raises an `OnReceive` action, it will
    -- ignore whatever result it assigned to `load`. This is a bit of a bluff,
    -- because in practice, no child element will ever call `OnReceive`. It
    -- is _only_ called by the component's _eval_ function, and we don't export
    -- the constructors for `Action`.
    { load: Nothing
    , render: zoom optic <<< input.render <<< (_ ^? _Just <<< optic)
    }
  OnUpdate d -> OnUpdate $ \mOutput -> do
    output <- mOutput
    output' <- d $ output ^? optic
    pure $ set optic output' output
  OnEmit pa -> OnEmit pa
  OnSubmit input -> OnSubmit input
  Void -> Void
