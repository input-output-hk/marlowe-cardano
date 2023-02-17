{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer
  where

import Control.Monad.Cleanup (MonadCleanup)
import Control.Monad.Trans.Control (MonadBaseControl)
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(AnyMessageAndAgency))
import Observe.Event (EventBackend, addField, withEvent)

data PeerSelector ps f where
  Send :: PeerSelector ps (AnyMessageAndAgency ps)
  Recv :: PeerSelector ps (AnyMessageAndAgency ps)

hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle

logPeer
  :: (MonadCleanup m, MonadBaseControl IO m)
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
