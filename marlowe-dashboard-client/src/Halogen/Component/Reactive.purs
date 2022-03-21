module Halogen.Component.Reactive
  ( ReactiveComponentSpec
  , ReactiveComponentEval
  , State
  , defaultReactiveEval
  , mkReactiveComponent
  , _input
  , _derived
  , _transient
  ) where

import Prelude

import Control.Monad.State (get)
import Control.React (ReactT, runReactT)
import Data.Bifunctor (bimap)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.Maybe (Maybe(..))
import Data.These (These(..))
import Halogen as H
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

type ReactiveComponentSpec derived transient action slots input output m =
  { deriveState :: input -> derived
  , initialTransient :: transient
  , render :: State input derived transient -> H.ComponentHTML action slots m
  , eval ::
      ReactiveComponentEval derived transient action slots input output m
  }

type ReactiveComponentEval derived transient action slots input output m =
  { react ::
      ReactT
        (State input derived transient)
        (H.HalogenM (State input derived transient) action slots output m)
        Unit
  , handleAction ::
      action
      -> H.HalogenM (State input derived transient) action slots output m Unit
  }

defaultReactiveEval
  :: forall derived transient action slots input output m
   . ReactiveComponentEval derived transient action slots input output m
defaultReactiveEval =
  { react: pure unit
  , handleAction: const (pure unit)
  }

mkReactiveComponent
  :: forall derived transient query action slots input output m
   . Functor m
  => ReactiveComponentSpec derived transient action slots input output m
  -> H.Component query input output m
mkReactiveComponent { deriveState, initialTransient, render, eval } =
  H.mkComponent
    { initialState: \input ->
        { input
        , derived: deriveState input
        , transient: initialTransient
        }
    , render: bimap (map Action) Action <<< render
    , eval: H.mkEval
        { initialize: Just Init
        , receive: Just <<< Receive
        , finalize: Just Finalize
        , handleAction: mapAction Action <<< case _ of
            Init -> do
              runReactT eval.react <<< That =<< get
            Finalize -> do
              runReactT eval.react <<< This =<< get
            Receive input -> do
              let derived = deriveState input
              state <- H.get
              let state' = state { derived = derived, input = input }
              H.put state'
              runReactT eval.react $ Both state state'
            Action action -> eval.handleAction action
        , handleQuery: const $ pure Nothing
        }
    }
