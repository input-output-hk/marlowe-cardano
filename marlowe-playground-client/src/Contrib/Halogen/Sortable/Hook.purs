module Contrib.Halogen.Components.Sortable.Hook where

import Prelude

import Contrib.Data.Unfoldable (Move) as Unfoldable
import Contrib.Halogen.Components.Sortable
  ( DragHandlers(..)
  , OrderingVersion
  , initialState
  , nextVersion
  ) as Sortable
import Contrib.Halogen.Components.Sortable (OrderingVersion, GenDragHandlers)
import Data.List.Infinite (unfold) as Infinite
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML.Events (onDragEnd, onDragEnter, onDragStart) as Events
import Halogen.Hooks (class HookNewtype, type (<>), Hook, HookM, UseEffect)
import Halogen.Hooks as Hooks
import Halogen.Hooks.Extra.Hooks (UseGet, UseStateFn, useGet, useModifyState_)
import Web.Event.Event (preventDefault) as Event
import Web.HTML.Event.DragEvent (toEvent) as DragEvent

foreign import data UseSortable :: Hooks.HookType

type State
  =
  { dragging :: Maybe Int
  , orderingVersion :: Sortable.OrderingVersion
  , move :: Maybe Unfoldable.Move
  }

type UseSortable'
  = UseStateFn State <> UseGet State <> Hooks.Pure

instance newtypeUseSortable :: HookNewtype UseSortable UseSortable'

useSortable
  :: forall m
   . MonadEffect m
  => Hook
       m
       UseSortable
       { dragging :: Maybe Int
       , genDragHandlers :: GenDragHandlers (HookM m Unit)
       , reordering ::
           { move :: Maybe Unfoldable.Move
           , version :: OrderingVersion
           }
       }
useSortable = Hooks.wrap hook
  where
  hook :: Hook m UseSortable' _
  hook = Hooks.do
    state /\ modifyState <-
      useModifyState_
        { dragging: Sortable.initialState.dragging
        , orderingVersion: Sortable.initialState.orderingVersion
        , move: Nothing
        }
    getState <- useGet state
    let
      genDragHandlers =
        Infinite.unfold 0 \idx -> do
          let
            handlers =
              Sortable.DragHandlers
                { onDragStart:
                    Events.onDragStart \event -> do
                      { dragging, orderingVersion } <- getState
                      if
                        ( orderingVersion == state.orderingVersion && dragging
                            /= Just idx
                        ) then
                        modifyState _ { dragging = Just idx }
                      else do
                        liftEffect $ Event.preventDefault
                          (DragEvent.toEvent event)
                , onDragEnd:
                    Events.onDragEnd
                      $ \_ -> do
                          { dragging } <- getState
                          when (isJust dragging) do
                            modifyState _ { dragging = Nothing }
                , onDragEnter:
                    Events.onDragEnter
                      $ \_ ->
                          getState
                            >>= case _ of
                              { dragging: Just dragging, orderingVersion }
                                | orderingVersion == state.orderingVersion &&
                                    dragging /= idx -> do
                                    let
                                      orderingVersion' = Sortable.nextVersion
                                        orderingVersion

                                      move = Just { from: dragging, to: idx }

                                      state' =
                                        { dragging: Just idx
                                        , orderingVersion: orderingVersion'
                                        , move
                                        }
                                    modifyState $ const state'
                              _ -> pure unit
                }
          handlers /\ (idx + 1)
    Hooks.pure
      { dragging: state.dragging
      , genDragHandlers
      , reordering:
          { move: state.move
          , version: state.orderingVersion
          }
      }

useSortable'
  :: forall m
   . MonadEffect m
  => (Unfoldable.Move -> HookM m Unit)
  -> Hook
       m
       (UseSortable <> UseEffect <> UseEffect)
       { dragging :: Maybe Int
       , genDragHandlers :: GenDragHandlers (HookM m Unit)
       }
useSortable' handleReordering = Hooks.do
  { dragging, genDragHandlers, reordering } <- useSortable
  Hooks.captures { version: reordering.version }
    $ flip Hooks.useTickEffect do
        case reordering.move of
          Just m -> handleReordering m
          Nothing -> pure unit
        pure Nothing
  Hooks.pure
    { dragging
    , genDragHandlers
    }
