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

type State
  =
  { dragged :: Maybe Int
  , orderingVersion :: Sortable.OrderingVersion
  , move :: Maybe Unfoldable.Move
  }

type UseSortable'
  = UseStateFn State <> UseGet State <> Hooks.Pure

foreign import data UseSortable :: Hooks.HookType

instance newtypeUseSortable :: HookNewtype UseSortable UseSortable'

useSortable
  :: forall m
   . MonadEffect m
  => Hook
       m
       UseSortable
       { dragged :: Maybe Int
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
        { dragged: Sortable.initialState.dragged
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
                      { dragged, orderingVersion } <- getState
                      if
                        orderingVersion == state.orderingVersion && dragged /=
                          Just idx then
                        modifyState _ { dragged = Just idx }
                      else
                        liftEffect $ Event.preventDefault
                          (DragEvent.toEvent event)
                , onDragEnd:
                    Events.onDragEnd
                      $ \_ -> do
                          { dragged } <- getState
                          when (isJust dragged) do
                            modifyState _ { dragged = Nothing }
                , onDragEnter:
                    Events.onDragEnter $ const $ getState >>= case _ of
                      { dragged: Just dragged, orderingVersion }
                        | orderingVersion == state.orderingVersion &&
                            dragged /= idx ->
                            modifyState $ const
                              { dragged: Just idx
                              , orderingVersion: Sortable.nextVersion
                                  orderingVersion
                              , move: Just { from: dragged, to: idx }
                              }
                      _ -> pure unit
                }
          handlers /\ (idx + 1)
    Hooks.pure
      { dragged: state.dragged
      , genDragHandlers
      , reordering:
          { move: state.move
          , version: state.orderingVersion
          }
      }

-- | A handy shortcut which keeps track of ordering version
-- | runs provided reordering effect only when necessary.
type UseApplySortable' = UseSortable <> UseEffect <> UseEffect

foreign import data UseApplySortable :: Hooks.HookType

instance newtypeUseApplySortable ::
  HookNewtype UseApplySortable UseApplySortable'

useApplySortable
  :: forall m
   . MonadEffect m
  => (Unfoldable.Move -> HookM m Unit)
  -> Hook
       m
       UseApplySortable
       { dragged :: Maybe Int
       , genDragHandlers :: GenDragHandlers (HookM m Unit)
       }
useApplySortable handleReordering = Hooks.wrap $ Hooks.do
  { dragged, genDragHandlers, reordering } <- useSortable
  Hooks.captures { version: reordering.version }
    $ flip Hooks.useTickEffect do
        case reordering.move of
          Just m -> handleReordering m
          Nothing -> pure unit
        pure Nothing
  Hooks.pure
    { dragged
    , genDragHandlers
    }
