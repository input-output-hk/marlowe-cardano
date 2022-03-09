module Control.Concurrent.EventBus
  ( EventBus
  , create
  , notify
  , subscribe
  , subscribeOnce
  ) where

import Prologue

import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Halogen.Subscription (SubscribeIO)
import Halogen.Subscription as HS

-- This type allows producers to `notify` events and for consumers to `subscribe`
-- to particular events on a channel
newtype EventBus channel event = EventBus
  (SubscribeIO (channel /\ event))

create :: forall channel event. Effect (EventBus channel event)
create = EventBus <$> HS.create

-- Subscribe to events under a particular channel
subscribe
  :: forall channel event r
   . Eq channel
  => EventBus channel event
  -> channel
  -> (event -> Effect r)
  -> Effect HS.Subscription
subscribe (EventBus { emitter }) channel =
  HS.subscribe emitter'
  where
  emitter' =
    snd <$> HS.filter (\(channel' /\ _) -> channel' == channel) emitter

-- Helper function to subscribe for the first notification on a channel.
subscribeOnce
  :: forall channel event
   . Eq channel
  => EventBus channel event
  -> channel
  -> Aff event
subscribeOnce bus channel =
  Aff.makeAff \resolve -> do
    refSubscription <- Ref.new Nothing
    let
      unsubscribe = do
        mSubscription <- Ref.read refSubscription
        for_ mSubscription HS.unsubscribe
        Ref.write Nothing refSubscription

    subscription <- subscribe bus channel \event -> do
      unsubscribe
      resolve (Right event)
    Ref.write (Just subscription) refSubscription
    pure $ Aff.effectCanceler unsubscribe

notify
  :: forall channel event
   . EventBus channel event
  -> channel
  -> event
  -> Effect Unit
notify (EventBus { listener }) channel event = HS.notify listener $ channel /\
  event

