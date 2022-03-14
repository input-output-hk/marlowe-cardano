module Halogen.Component.Reactive
  ( ReactiveComponentSpec
  , ReactiveComponentEval
  , defaultReactiveEval
  , mkReactiveComponent
  ) where

import Prelude

import Data.Bifunctor (bimap)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Query.HalogenM (mapAction)

data Action input action
  = Init
  | Finalize
  | Receive input
  | Action action

type ReactiveComponentSpec state query action slots input output m =
  { deriveState :: input -> Maybe state -> state
  , render :: state -> H.ComponentHTML action slots m
  , eval :: ReactiveComponentEval state query action slots output m
  }

type ReactiveComponentEval state query action slots output m =
  { initialize :: H.HalogenM state action slots output m Unit
  , finalize :: H.HalogenM state action slots output m Unit
  , handleStateUpdate ::
      Maybe state -> H.HalogenM state action slots output m Unit
  , handleAction :: action -> H.HalogenM state action slots output m Unit
  , handleQuery ::
      forall a. query a -> H.HalogenM state action slots output m (Maybe a)
  }

defaultReactiveEval
  :: forall state query action slots output m
   . ReactiveComponentEval state query action slots output m
defaultReactiveEval =
  { initialize: pure unit
  , finalize: pure unit
  , handleStateUpdate: const (pure unit)
  , handleAction: const (pure unit)
  , handleQuery: const (pure Nothing)
  }

mkReactiveComponent
  :: forall state query action slots input output m
   . Functor m
  => ReactiveComponentSpec state query action slots input output m
  -> H.Component query input output m
mkReactiveComponent { deriveState, render, eval } =
  H.mkComponent
    { initialState: \input -> deriveState input Nothing
    , render: bimap (map Action) Action <<< render
    , eval: H.mkEval
        { initialize: Just Init
        , receive: Just <<< Receive
        , finalize: Just Finalize
        , handleAction: mapAction Action <<< case _ of
            Init -> do
              eval.initialize
              eval.handleStateUpdate Nothing
            Finalize -> do
              eval.finalize
            Receive input -> do
              state <- H.get
              H.put $ deriveState input (Just state)
              eval.handleStateUpdate (Just state)
            Action action -> eval.handleAction action
        , handleQuery: \query -> mapAction Action (eval.handleQuery query)
        }
    }
