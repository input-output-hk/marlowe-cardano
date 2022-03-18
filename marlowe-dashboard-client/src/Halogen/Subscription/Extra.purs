module Halogen.Subscription.Extra where

import Prologue

import Data.Foldable (for_, traverse_)
import Data.Map as Map
import Data.Traversable (traverse)
import Effect.Aff
  ( Aff
  , bracket
  , error
  , killFiber
  , launchAff
  , launchAff_
  , supervise
  )
import Effect.Aff as Aff
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription (Emitter, SubscribeIO, Subscription)
import Halogen.Subscription as HS

-- Connect an emitter to an existing SubscribeIO.
connectEmitter
  :: forall m a. MonadEffect m => Emitter a -> SubscribeIO a -> m Subscription
connectEmitter emitter { listener } = liftEffect $ HS.subscribe emitter $
  HS.notify listener

-- Connect an emitter to an existing SubscribeIO. Discards the subscription.
connectEmitter_
  :: forall m a. MonadEffect m => Emitter a -> SubscribeIO a -> m Unit
connectEmitter_ emitter = void <<< connectEmitter emitter

-- Turn an emitter of Maybes into an emitter that only fires `Just` values.
compactEmitter :: forall a. Emitter (Maybe a) -> Emitter a
compactEmitter emitter = HS.makeEmitter $
  pure
    <<< HS.unsubscribe
    <=< HS.subscribe emitter <<< traverse

-- Map an emitter to a Maybe and discard any `Nothing` occurances.
filterMapEmitter :: forall a b. (a -> Maybe b) -> Emitter a -> Emitter b
filterMapEmitter f = compactEmitter <<< map f

-- Create an emitter from an emitter of emitters. The resulting emitter will
-- emit whatever the most recent inner emitter emits.
switchEmitter :: forall a. Emitter (Emitter a) -> Emitter a
switchEmitter emitter = HS.makeEmitter \push -> do
  innerSub <- Ref.new Nothing
  outerSub <- HS.subscribe emitter \emitter' -> do
    traverse_ HS.unsubscribe =<< Ref.read innerSub
    flip Ref.write innerSub <<< Just =<< HS.subscribe emitter' push
  pure do
    traverse_ HS.unsubscribe =<< Ref.read innerSub
    HS.unsubscribe outerSub

-- For each value produced by the argument emitter, produce a new emitter and
-- begin emitting its values.
switchMapEmitter :: forall a b. (a -> Emitter b) -> Emitter a -> Emitter b
switchMapEmitter f = switchEmitter <<< map f

-- React to events fired by an emitter by performing an asynchronous action.
-- The resulting emitter will emit the result of evaluating the asynchronous
-- action. The result includes Errors thrown by the callback.
reactimate :: forall a b. (a -> Aff b) -> Emitter a -> Emitter b
reactimate runAction emitter = HS.makeEmitter \push -> do
  -- We track the fibers spawned in the subscribe callback because we can't
  -- `supervise` them in Effect, so we need to manually collect and cancel them
  -- in the dispose block.
  nextFiberId <- Ref.new 0
  fibersRef <- Ref.new Map.empty
  -- Subscribe to the emitter and call runAction
  subscription <- HS.subscribe emitter \a -> do
    -- Get the next fiber ID and update it
    fiberId <- Ref.read nextFiberId
    Ref.write (fiberId + 1) nextFiberId
    let
      -- | Removes this fiber from the Map of fibers.
      removeFiber _ = liftEffect $ Ref.modify_ (Map.delete fiberId) fibersRef
    -- | Run the callback in a new Aff, making sure to delete the fiber from
    -- | the map on completion.
    fiber <- launchAff $ bracket (pure unit) removeFiber \_ -> do
      b <- supervise $ runAction a
      liftEffect $ push b
    -- | Add the new fiber to the fibers map
    Ref.modify_ (Map.insert fiberId fiber) fibersRef
  pure do
    -- | Dispose the subscription created when subscribing to the argument
    -- | emitter.
    HS.unsubscribe subscription
    -- | Kill all pending Fibers.
    fibers <- Ref.read fibersRef
    traverse_ (launchAff_ <<< killFiber (error "unsubscribed")) fibers

-- Helper function to subscribe for the first notification from an emitter
subscribeOnce :: Emitter ~> Aff
subscribeOnce emitter =
  Aff.makeAff \resolve -> do
    refSubscription <- Ref.new Nothing
    let
      unsubscribe = do
        mSubscription <- Ref.read refSubscription
        for_ mSubscription HS.unsubscribe
        Ref.write Nothing refSubscription

    subscription <- HS.subscribe emitter \a -> do
      unsubscribe
      resolve (Right a)
    Ref.write (Just subscription) refSubscription
    pure $ Aff.effectCanceler unsubscribe

