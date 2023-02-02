{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.Query.Client
  where

import Control.Applicative (liftA2)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol

data MarloweQueryClient m a
  = MarloweQueryClientPure a
  | MarloweQueryClientPeer (Peer MarloweQuery 'AsClient 'StInit m a)
  deriving (Functor)

instance Applicative m => Applicative (MarloweQueryClient m) where
  pure = MarloweQueryClientPure
  MarloweQueryClientPure f <*> a = f <$> a
  f <*> MarloweQueryClientPure a = ($ a) <$> f
  MarloweQueryClientPeer peerF <*> MarloweQueryClientPeer peerA = MarloweQueryClientPeer $ apPeerInit peerF peerA
    where
    apPeerInit
      :: Peer MarloweQuery 'AsClient 'StInit m (a -> b)
      -> Peer MarloweQuery 'AsClient 'StInit m a
      -> Peer MarloweQuery 'AsClient 'StInit m b
    apPeerInit pf pa = case (pf, pa) of
      (Effect m1, Effect m2) -> Effect $ liftA2 apPeerInit m1 m2
      (Effect m1, _) -> Effect $ flip apPeerInit pa <$> m1
      (_, Effect m2) -> Effect $ apPeerInit pf <$> m2
      (Yield (ClientAgency TokInit) (MsgRequest req1) pf', Yield (ClientAgency TokInit) (MsgRequest req2) pa') ->
        Yield (ClientAgency TokInit) (MsgRequest (ReqBoth req1 req2)) $ apPeerRequest pf' pa'

    apPeerRequest
      :: Peer MarloweQuery 'AsClient ('StRequest r1) m (a -> b)
      -> Peer MarloweQuery 'AsClient ('StRequest r2) m a
      -> Peer MarloweQuery 'AsClient ('StRequest (r1, r2)) m b
    apPeerRequest pf pa = case (pf, pa) of
      (Effect m1, Effect m2) -> Effect $ liftA2 apPeerRequest m1 m2
      (Effect m1, _) -> Effect $ flip apPeerRequest pa <$> m1
      (_, Effect m2) -> Effect $ apPeerRequest pf <$> m2
      (Await (ServerAgency (TokRequest req1)) handleF, Await (ServerAgency (TokRequest req2)) handleA) ->
        Await (ServerAgency $ TokRequest $ TokBoth req1 req2) \case
          MsgRespond (r1, r2) -> appPeerDone (handleF $ MsgRespond r1) (handleA $ MsgRespond r2)

    appPeerDone
      :: Peer MarloweQuery 'AsClient 'StDone m (a -> b)
      -> Peer MarloweQuery 'AsClient 'StDone m a
      -> Peer MarloweQuery 'AsClient 'StDone m b
    appPeerDone pf pa = case (pf, pa) of
      (Effect m1, Effect m2) -> Effect $ liftA2 appPeerDone m1 m2
      (Effect m1, _) -> Effect $ flip appPeerDone pa <$> m1
      (_, Effect m2) -> Effect $ appPeerDone pf <$> m2
      (Done TokDone f, Done TokDone a) -> Done TokDone $ f a

getContractHeaders :: Range ContractId -> MarloweQueryClient m (Page ContractId ContractHeader)
getContractHeaders range = MarloweQueryClientPeer $
  Yield (ClientAgency TokInit) (MsgRequest $ ReqContractHeaders range) $
  Await (ServerAgency $ TokRequest TokContractHeaders) \(MsgRespond page) ->
    Done TokDone page
