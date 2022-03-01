module Halogen.Time where

import Prologue

import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Rec.Class (forever)
import Data.DateTime.Instant (Instant)
import Data.Time.Duration (class Duration, fromDuration)
import Effect.Aff (delay, error, killFiber, launchAff, launchAff_)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Subscription as HS

subscribeTime
  :: forall state action slots output m d
   . MonadAff m
  => MonadTime m
  => Duration d
  => d
  -> (Instant -> action)
  -> H.HalogenM state action slots output m H.SubscriptionId
subscribeTime interval action = do
  let
    emitter = HS.makeEmitter \push -> do
      fiber <- launchAff $ forever do
        liftEffect <<< push =<< now
        delay $ fromDuration interval
      pure $ launchAff_ $ killFiber (error "cleanup") fiber
  H.subscribe $ action <$> emitter

subscribeTime'
  :: forall state action slots output m d
   . MonadAff m
  => MonadTime m
  => Duration d
  => d
  -> (Instant -> action)
  -> H.HalogenM state action slots output m Unit
subscribeTime' interval = void <<< subscribeTime interval
