module Control.Concurrent.EventBus
  ( EventBus
  , BusEmitter
  , BusListener
  , create
  , notify
  , makeBusEmitter
  , subscribe
  , subscribeAll
  , subscribeOnce
  , subscribeOnceWithTimeout
  ) where

import Prologue

import Control.Alt (class Alt, (<|>))
import Control.Alternative (class Plus)
import Control.Parallel (parOneOf)
import Control.Plus (empty)
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Compactable (class Compactable, compact, separate)
import Data.Either (either, hush)
import Data.Filterable (class Filterable, filterDefault, partitionDefault)
import Data.Functor.Contravariant (class Contravariant, cmap)
import Data.Profunctor (lcmap)
import Data.Time.Duration (class Duration, fromDuration)
import Data.Traversable (for_, sequence, traverse)
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Aff (Aff, delay)
import Effect.Aff as Aff
import Effect.Ref as Ref
import Halogen.Subscription as HS
import Unsafe.Coerce (unsafeCoerce)

-- | The output end of an EventBus. Allows a specific channel to be subscribed
-- | to.
newtype BusEmitter channel event = BusEmitter
  (HS.Emitter (channel /\ event))

derive instance Functor (BusEmitter channel)

instance Alt (BusEmitter channel) where
  alt (BusEmitter a) (BusEmitter b) = BusEmitter $ a <|> b

instance Plus (BusEmitter channel) where
  empty = BusEmitter empty

instance Bifunctor BusEmitter where
  bimap f g (BusEmitter emitter) = BusEmitter $ map (bimap f g) emitter

instance Compactable (BusEmitter channel) where
  compact (BusEmitter emitter) = BusEmitter $ HS.makeEmitter $
    pure <<< HS.unsubscribe
      <=< HS.subscribe emitter <<< lcmap sequence <<< traverse
  separate (BusEmitter emitter) =
    { left:
        compact $ BusEmitter $ (map $ either Just (const Nothing)) <$> emitter
    , right: compact $ BusEmitter $ map hush <$> emitter
    }

instance Filterable (BusEmitter channel) where
  filter p = filterDefault p
  partition p = partitionDefault p
  filterMap f = compact <<< map f
  partitionMap f = separate <<< map f

-- | The input end of an EventBus. Allows events to be sent to a specific
-- | channel.
newtype BusListener channel event = BusListener
  (HS.Listener (channel /\ event))

instance Contravariant (BusListener channel) where
  cmap f (BusListener listener) = BusListener $ cmap (map f) listener

instance Semigroup (BusListener channel event) where
  append (BusListener a) (BusListener b) =
    BusListener
      $ unsafeWrapListener
          \event -> unsafeUnwrapListener a event *> unsafeUnwrapListener b event
    where
    unsafeWrapListener :: forall a. (a -> Effect Unit) -> HS.Listener a
    unsafeWrapListener = unsafeCoerce

    unsafeUnwrapListener :: forall a. HS.Listener a -> (a -> Effect Unit)
    unsafeUnwrapListener = unsafeCoerce

instance Monoid (BusListener channel event) where
  mempty = BusListener $ unsafeWrapListener $ const $ pure unit
    where
    unsafeWrapListener :: forall a. (a -> Effect Unit) -> HS.Listener a
    unsafeWrapListener = unsafeCoerce

-- | Allows events to be sent to specific channels and for those channels to be
-- | listened to.
type EventBus channel event =
  { emitter :: BusEmitter channel event, listener :: BusListener channel event }

makeBusEmitter
  :: forall channel event
   . ((channel -> event -> Effect Unit) -> Effect (Effect Unit))
  -> BusEmitter channel event
makeBusEmitter f = BusEmitter $ HS.makeEmitter $ lcmap curry f

create :: forall channel event. Effect (EventBus channel event)
create = do
  { listener, emitter } <- HS.create
  pure { listener: BusListener listener, emitter: BusEmitter emitter }

-- Subscribe to events under a particular channel
subscribe
  :: forall channel event r
   . Eq channel
  => BusEmitter channel event
  -> channel
  -> (event -> Effect r)
  -> Effect HS.Subscription
subscribe (BusEmitter emitter) channel =
  HS.subscribe emitter'
  where
  emitter' =
    snd <$> HS.filter (\(channel' /\ _) -> channel' == channel) emitter

-- Subscribe to events on all channels
subscribeAll
  :: forall channel event r
   . BusEmitter channel event
  -> (channel -> event -> Effect r)
  -> Effect HS.Subscription
subscribeAll (BusEmitter emitter) = HS.subscribe emitter <<< uncurry

-- Helper function to subscribe for the first notification on a channel.
subscribeOnce
  :: forall channel event
   . Eq channel
  => BusEmitter channel event
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

-- Helper function to subscribe for the first notification on a channel with a
-- timeout.
subscribeOnceWithTimeout
  :: forall channel event duration
   . Eq channel
  => Duration duration
  => BusEmitter channel event
  -> duration
  -> channel
  -> Aff (Maybe event)
subscribeOnceWithTimeout bus timeout channel = parOneOf
  [ Just <$> subscribeOnce bus channel
  , Nothing <$ delay (fromDuration timeout)
  ]

notify
  :: forall channel event
   . BusListener channel event
  -> channel
  -> event
  -> Effect Unit
notify (BusListener listener) channel event =
  HS.notify listener $ channel /\ event
