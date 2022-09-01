{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE RankNTypes     #-}

module Network.Protocol.Driver where

import Control.Monad (join)
import Data.Void (absurd)
import Network.Channel (Channel (..))
import Network.TypedProtocol (Message, Peer (..), PeerHasAgency (..), PeerRole (..), Protocol (..), SomeMessage)
import Network.TypedProtocol.Codec (Codec (..), DecodeStep (..))
import Network.TypedProtocol.Driver (Driver (..))

mkDriver
  :: forall ps failure bytes m
   . Monad m
  => (forall a. failure -> m a)
  -> Codec ps failure m bytes
  -> Channel m bytes
  -> Driver ps (Maybe bytes) m
mkDriver throwImpl Codec{..} Channel{..} = Driver{..}
  where
    sendMessage
      :: forall (pr :: PeerRole) (st :: ps) (st' :: ps)
       . PeerHasAgency pr st
      -> Message ps st st'
      -> m ()
    sendMessage tok = send . encode tok

    recvMessage
      :: forall (pr :: PeerRole) (st :: ps)
       . PeerHasAgency pr st
      -> Maybe bytes
      -> m (SomeMessage st, Maybe bytes)
    recvMessage tok trailing = decodeChannel trailing =<< decode tok

    decodeChannel
      :: Maybe bytes
      -> DecodeStep bytes failure m a
      -> m (a, Maybe bytes)
    decodeChannel _ (DecodeDone a trailing)     = pure (a, trailing)
    decodeChannel _ (DecodeFail failure)        = throwImpl failure
    decodeChannel Nothing (DecodePartial next)  = recv >>= next >>= decodeChannel Nothing
    decodeChannel trailing (DecodePartial next) = next trailing >>= decodeChannel Nothing

    startDState :: Maybe bytes
    startDState = Nothing

runPeers :: (Monad m, Protocol ps) => Peer ps 'AsClient st m a -> Peer ps 'AsServer st m b -> m (a, b)
runPeers client server = case (client, server) of
  (Effect mClient, Effect mServer)            -> join $ runPeers <$> mClient <*> mServer
  (Effect mClient, _)                         -> flip runPeers server =<< mClient
  (_, Effect mServer)                         -> runPeers client =<< mServer
  (Done _ a, Done _ b)                        -> pure (a, b)
  (Yield _ msg client', Await _ k)            -> runPeers client' $ k msg
  ( Await _ k,Yield _ msg server')            -> runPeers (k msg) server'
  (Yield (ClientAgency tok) _ _, Yield (ServerAgency tok') _ _) -> absurd $ exclusionLemma_ClientAndServerHaveAgency tok tok'
  (Await (ServerAgency tok) _, Await (ClientAgency tok') _) -> absurd $ exclusionLemma_ClientAndServerHaveAgency tok' tok
  (Done tok _, Yield (ServerAgency tok') _ _) -> absurd $ exclusionLemma_NobodyAndServerHaveAgency tok tok'
  (Done tok _, Await (ClientAgency tok') _)   -> absurd $ exclusionLemma_NobodyAndClientHaveAgency tok tok'
  (Yield (ClientAgency tok) _ _, Done tok' _) -> absurd $ exclusionLemma_NobodyAndClientHaveAgency tok' tok
  (Await (ServerAgency tok) _, Done tok' _)   -> absurd $ exclusionLemma_NobodyAndServerHaveAgency tok' tok
