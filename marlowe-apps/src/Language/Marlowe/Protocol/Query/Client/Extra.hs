
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Language.Marlowe.Protocol.Query.Client.Extra
  ( hoistMarloweQueryClient
  , marloweQueryClientPeer
  ) where


import Language.Marlowe.Protocol.Query.Client
import Language.Marlowe.Protocol.Query.Types
import Network.Protocol.Driver (hoistPeer)
import Network.TypedProtocol


marloweQueryClientPeer
  :: forall m a
  .  MarloweQueryClient m a
  -> Peer MarloweQuery 'AsClient 'StInit m a
marloweQueryClientPeer (MarloweQueryClientPure x) = Done ({- FIXME -} error "marloweQueryClientPeer is pure" :: NobodyHasAgency 'StInit) x
marloweQueryClientPeer (MarloweQueryClientPeer p) = p


hoistMarloweQueryClient
  :: forall m n a
  .  Functor m
  => (forall x . m x -> n x)
  -> MarloweQueryClient m a
  -> MarloweQueryClient n a
hoistMarloweQueryClient _ (MarloweQueryClientPure x) = MarloweQueryClientPure x
hoistMarloweQueryClient f (MarloweQueryClientPeer p) = MarloweQueryClientPeer $ hoistPeer f p
