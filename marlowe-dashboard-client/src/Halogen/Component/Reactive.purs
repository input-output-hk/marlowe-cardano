module Halogen.Component.Reactive
  ( Spec
  , Eval
  , HalogenM
  , State
  , class ReactiveEval
  , _derived
  , _input
  , _transient
  , fromHandleAction
  , fromHandleActionAndInput
  , mkReactiveComponent
  , toReactiveEval
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Foldable (traverse_)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Extra (imapState)
import Halogen.Query.HalogenM (mapAction)
import Type.Proxy (Proxy(..))

data Action input action
  = Init
  | Finalize
  | Receive input
  | Action action

_input
  :: forall input derived transient. Lens' (State input derived transient) input
_input = prop (Proxy :: _ "input")

_derived
  :: forall input derived transient
   . Lens' (State input derived transient) derived
_derived = prop (Proxy :: _ "derived")

_transient
  :: forall input derived transient
   . Lens' (State input derived transient) transient
_transient = prop (Proxy :: _ "transient")

type State input derived transient =
  { input :: input
  , derived :: derived
  , transient :: transient
  }

type Spec resources derived transient action slots input output m =
  { deriveState :: input -> derived
  , initialTransient :: transient
  , render :: State input derived transient -> H.ComponentHTML action slots m
  , eval ::
      Eval resources derived transient action slots input
        output
        m
  }

type HalogenM input derived transient action slots output m =
  H.HalogenM (State input derived transient) action slots output m

type Eval resources derived transient action slots input output m =
  { initialize ::
      HalogenM input derived transient action slots output m resources
  , finalize ::
      resources -> HalogenM input derived transient action slots output m Unit
  , handleInput ::
      resources
      -> Maybe (State input derived transient)
      -> HalogenM input derived transient action slots output m Unit
  , handleAction ::
      action -> HalogenM input derived transient action slots output m Unit
  }

class
  ReactiveEval resources derived transient action slots input output m a
  | a -> resources derived transient action slots input output m where
  toReactiveEval
    :: a -> Eval resources derived transient action slots input output m

fromHandleAction
  :: forall derived transient action slots input output m
   . Applicative m
  => (action -> HalogenM input derived transient action slots output m Unit)
  -> Eval Unit derived transient action slots input output m
fromHandleAction handleAction =
  { handleAction
  , handleInput: const $ const $ pure unit
  , initialize: pure unit
  , finalize: const $ pure unit
  }

fromHandleActionAndInput
  :: forall derived transient action slots input output m
   . Applicative m
  => { handleAction ::
         action -> HalogenM input derived transient action slots output m Unit
     , handleInput ::
         Maybe (State input derived transient)
         -> HalogenM input derived transient action slots output m Unit
     }
  -> Eval Unit derived transient action slots input output m
fromHandleActionAndInput { handleAction, handleInput } =
  { handleAction
  , handleInput: const $ handleInput
  , initialize: pure unit
  , finalize: const $ pure unit
  }

mkReactiveComponent
  :: forall resources derived transient query action slots input output m
   . Monad m
  => Eq derived
  => Spec
       resources
       derived
       transient
       action
       slots
       input
       output
       m
  -> H.Component query input output m
mkReactiveComponent { deriveState, initialTransient, render, eval } =
  H.mkComponent
    { initialState: \input ->
        { resources: Nothing
        , state:
            { input
            , derived: deriveState input
            , transient: initialTransient
            }
        }
    , render: bimap (map Action) Action <<< render <<< _.state
    , eval: H.mkEval
        { initialize: Just Init
        , receive: Just <<< Receive
        , finalize: Just Finalize
        , handleAction: mapAction Action <<< case _ of
            Init -> do
              resources <- imapState _state eval.initialize
              H.modify_ _ { resources = Just resources }
              imapState _state $ eval.handleInput resources Nothing
            Finalize -> do
              resources <- H.gets _.resources
              imapState _state $ traverse_ eval.finalize resources
            Receive input -> do
              let derived = deriveState input
              { resources, state } <- H.get
              when (derived /= state.derived) do
                let state' = state { derived = derived, input = input }
                H.modify_ _ { state = state' }
                imapState _state $ traverse_
                  (flip eval.handleInput $ Just state)
                  resources
            Action action ->
              imapState _state $ eval.handleAction action
        , handleQuery: const $ pure Nothing
        }
    }
  where
  _state = prop (Proxy :: _ "state")
