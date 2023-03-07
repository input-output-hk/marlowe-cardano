{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer
  where

import Control.Monad.With (MonadWithExceptable)
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event.Component (GetSelectorConfig, SelectorConfig(..), SomeJSON(..), singletonFieldConfigWith)
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import Observe.Event.Network.Protocol (MessageToJSON(..))

data PeerSelector ps f where
  Send :: PeerSelector ps (AnyMessageAndAgency ps)
  Recv :: PeerSelector ps (AnyMessageAndAgency ps)

getPeerSelectorConfig :: MessageToJSON ps => Bool -> GetSelectorConfig (PeerSelector ps)
getPeerSelectorConfig defaultEnabled = \case
  Send -> SelectorConfig "send" defaultEnabled $ singletonFieldConfigWith
    (\(AnyMessageAndAgency tok msg) -> SomeJSON $ messageToJSON tok msg)
    "message"
    True
  Recv -> SelectorConfig "recv" defaultEnabled $ singletonFieldConfigWith
    (\(AnyMessageAndAgency tok msg) -> SomeJSON $ messageToJSON tok msg)
    "message"
    True

hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle

logPeer
  :: (MonadWithExceptable m)
  => EventBackend m r (PeerSelector ps)
  -> Peer ps pr st m a
  -> Peer ps pr st m a
logPeer eventBackend = \case
  Effect m -> Effect $ logPeer eventBackend <$> m
  Done tok a -> Done tok a
  Yield tok msg peer -> Effect $ withEvent eventBackend Send \ev -> do
    addField ev $ AnyMessageAndAgency tok msg
    pure $ Yield tok msg $ logPeer eventBackend peer
  Await tok handle -> Await tok \msg -> Effect $ withEvent eventBackend Recv \ev -> do
    addField ev $ AnyMessageAndAgency tok msg
    pure $ logPeer eventBackend $ handle msg
