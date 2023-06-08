{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Network.Protocol.Peer where

import Data.Kind (Type)
import Network.TypedProtocol

type AgencyEquals :: forall ps. PeerRole -> PeerRole -> ps -> ps -> Type
data AgencyEquals pr pr' st st' where
  AgencyRefl :: AgencyEquals pr pr st st

class TestAgencyEquality ps where
  testAgencyEquality
    :: PeerHasAgency pr (st :: ps)
    -> PeerHasAgency pr' (st' :: ps)
    -> Maybe (AgencyEquals pr pr' st st')

hoistPeer :: Functor m => (forall x. m x -> n x) -> Peer protocol pr st m a -> Peer protocol pr st n a
hoistPeer f = \case
  Effect m -> Effect $ f $ hoistPeer f <$> m
  Done na a -> Done na a
  Yield wa msg peer -> Yield wa msg $ hoistPeer f peer
  Await ta handle -> Await ta $ hoistPeer f . handle
