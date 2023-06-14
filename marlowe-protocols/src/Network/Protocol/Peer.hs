{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Network.Protocol.Peer where

import Network.TypedProtocol

hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle
