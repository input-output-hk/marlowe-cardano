module Contrib.Halogen.Components.Sortable where

import Prelude

import Contrib.Data.Unfoldable (Move) as Unfoldable
import Control.Alternative (guard) as Alternative
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Lens (Lens')
import Data.Lens (set, view) as Lens
import Data.List.Infinite (List, unfold) as Infinite
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (HalogenM)
import Halogen (get, put) as H
import Halogen.HTML (IProp)
import Halogen.HTML.Events (onDragEnd, onDragEnter, onDragStart) as Events
import Web.Event.Event (preventDefault)
import Web.HTML.Event.DragEvent (DragEvent)
import Web.HTML.Event.DragEvent (toEvent) as DragEvent

newtype OrderingVersion = OrderingVersion Int

derive newtype instance eqOrderingVersion :: Eq OrderingVersion

nextVersion :: OrderingVersion -> OrderingVersion
nextVersion (OrderingVersion i) = OrderingVersion (i + 1)

-- | We have to keep ordering version because sorting can trigger
-- | reordering which asynchronously is repainted. If in between
-- | we trigger some other reordering it is going to be inconsistent
-- | with the new one so we have to ignore all events till everything is
-- | repainted
type State = { orderingVersion :: OrderingVersion, dragged :: Maybe Int }

initialState :: State
initialState = { orderingVersion: OrderingVersion 0, dragged: Nothing }

data Action
  = DragStart OrderingVersion DragEvent Int
  | DragEnd
  | MoveTo OrderingVersion Int

type DragHandler r action = IProp
  ( onDragEnd :: DragEvent
  , onDragEnter :: DragEvent
  , onDragStart :: DragEvent
  | r
  )
  action

-- | We need this `newtype` because of escaping type variable `r` error.
-- | The benefit is that we get a Functor out of it.
newtype DragHandlers action = DragHandlers
  { onDragEnd :: forall r. DragHandler r action
  , onDragEnter :: forall r. DragHandler r action
  , onDragStart :: forall r. DragHandler r action
  }

instance functorDragHandlers :: Functor DragHandlers where
  map f (DragHandlers r) =
    DragHandlers
      { onDragStart: map f r.onDragStart
      , onDragEnter: map f r.onDragEnter
      , onDragEnd: map f r.onDragEnd
      }

type GenDragHandlers action = Infinite.List (DragHandlers action)

mkGenDragHandlers :: OrderingVersion -> GenDragHandlers Action
mkGenDragHandlers orderingVersion =
  Infinite.unfold 0 \idx -> do
    let
      onDragStart :: forall r. DragHandler r Action
      onDragStart =
        Events.onDragStart \event -> do
          DragStart orderingVersion event idx

      onDragEnd :: forall r. DragHandler r Action
      onDragEnd = Events.onDragEnd $ const DragEnd

      onDragEnter :: forall r. DragHandler r Action
      onDragEnter =
        Events.onDragEnter
          $ const do
              -- | The action handler is going to check if
              -- | idices are different etc.
              MoveTo orderingVersion idx
    Tuple
      ( DragHandlers
          { onDragStart
          , onDragEnd
          , onDragEnter
          }
      )
      (idx + 1)

mkGenDragHandlers'
  :: forall state. Lens' state State -> state -> GenDragHandlers Action
mkGenDragHandlers' l = mkGenDragHandlers <<< _.orderingVersion <<< Lens.view l

-- | We provide a reordering function instead of assuming that items
-- | container is accessible through local state because it is usually
-- | not the case (it can be in `halogen-store` and local state etc.)
handleAction
  :: forall m
   . MonadEffect m
  => (Unfoldable.Move -> m Unit)
  -> Action
  -> State
  -> m (Maybe State)
handleAction handleReordering action state = case action of
  DragStart orderingVersion event dragged ->
    if
      ( orderingVersion == state.orderingVersion && state.dragged /= Just
          dragged
      ) then
      pure $ Just $ state { dragged = Just dragged }
    else do
      liftEffect $ preventDefault (DragEvent.toEvent event)
      pure Nothing
  DragEnd ->
    pure
      $
        Alternative.guard
          (isJust state.dragged)
          $> state { dragged = Nothing }
  MoveTo orderingVersion idx ->
    runMaybeT do
      dragged <- hoistMaybe state.dragged
      st /\ f <-
        hoistMaybe $
          Alternative.guard
            (state.orderingVersion == orderingVersion && dragged /= idx)
            $> Tuple
              { dragged: Just idx
              , orderingVersion: nextVersion state.orderingVersion
              }
              { from: dragged, to: idx }
      lift $ handleReordering f
      pure st

handleActionH
  :: forall action m output slots state
   . MonadEffect m
  => (Unfoldable.Move -> HalogenM state action slots output m Unit)
  -> Lens' state State
  -> Action
  -> HalogenM state action slots output m Unit
handleActionH handleReordering stateL action = do
  state <- H.get
  let
    updated =
      runMaybeT do
        state' <- MaybeT $ handleAction handleReordering action $ Lens.view
          stateL
          state
        pure $ Lens.set stateL state' state
  updated
    >>= case _ of
      Just state' -> H.put state'
      Nothing -> pure unit
