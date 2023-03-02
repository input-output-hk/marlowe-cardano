{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Protocol.Query.Client
  where

import Control.Applicative (liftA2)
import Control.Monad.Base (MonadBase(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.ChainSync.Api (TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol

data MarloweQueryClient m a where
  ClientRequest :: Request x -> (x -> m (MarloweQueryClient m a)) -> MarloweQueryClient m a
  ClientLift :: m (MarloweQueryClient m a) -> MarloweQueryClient m a
  ClientPure :: a -> MarloweQueryClient m a

deriving instance Functor m => Functor (MarloweQueryClient m)

instance Applicative m => Applicative (MarloweQueryClient m) where
  pure = ClientPure
  ClientPure f <*> a = f <$> a
  f <*> ClientPure a = ($ a) <$> f
  f <*> ClientLift a = ClientLift $ (f <*>) <$> a
  ClientLift f <*> a = ClientLift $ (<*> a) <$> f
  ClientRequest req1 contF <*> ClientRequest req2 contA =
    ClientRequest (ReqBoth req1 req2) \(r1, r2) -> liftA2 (<*>) (contF r1) (contA r2)

instance Monad m => Monad (MarloweQueryClient m) where
  ClientPure a >>= k = k a
  ClientLift m >>= k = ClientLift $ (>>= k) <$> m
  ClientRequest req cont >>= k = ClientRequest req $ fmap (>>= k) . cont

instance MonadTrans MarloweQueryClient where
  lift = ClientLift . fmap pure

instance MonadIO m => MonadIO (MarloweQueryClient m) where
  liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (MarloweQueryClient m) where
  liftBase = lift . liftBase

request :: Applicative m => Request a -> MarloweQueryClient m a
request req = ClientRequest req $ pure . pure

getWithdrawal :: Applicative m => TxId -> MarloweQueryClient m (Maybe Withdrawal)
getWithdrawal = request . ReqWithdrawal

getWithdrawals :: Applicative m => WithdrawalFilter -> Range TxId -> MarloweQueryClient m (Maybe (Page TxId Withdrawal))
getWithdrawals = fmap request . ReqWithdrawals

getContractHeaders :: Applicative m => ContractFilter -> Range ContractId -> MarloweQueryClient m (Maybe (Page ContractId ContractHeader))
getContractHeaders = fmap request . ReqContractHeaders

getContractState :: Applicative m => ContractId -> MarloweQueryClient m (Maybe SomeContractState)
getContractState = request . ReqContractState

getTransactions :: Applicative m => ContractId -> MarloweQueryClient m (Maybe SomeTransactions)
getTransactions = request . ReqTransactions

getTransaction :: Applicative m => TxId -> MarloweQueryClient m (Maybe SomeTransaction)
getTransaction = request . ReqTransaction

hoistMarloweQueryClient :: Functor m => (forall x. m x -> n x) -> MarloweQueryClient m a -> MarloweQueryClient n a
hoistMarloweQueryClient f = \case
  ClientPure a -> ClientPure a
  ClientLift m -> ClientLift $ f $ hoistMarloweQueryClient f <$> m
  ClientRequest req cont -> ClientRequest req $ f . fmap (hoistMarloweQueryClient f) . cont

marloweQueryClientPeer :: Functor m => MarloweQueryClient m a -> Peer MarloweQuery 'AsClient 'StReq m a
marloweQueryClientPeer = \case
  ClientPure a ->
    Yield (ClientAgency TokReq) MsgDone $
    Done TokDone a
  ClientLift m ->
    Effect $ marloweQueryClientPeer <$> m
  ClientRequest req cont ->
    Yield (ClientAgency TokReq) (MsgRequest req) $
    Await (ServerAgency (TokRes $ requestToSt req)) \case
      MsgRespond r -> Effect $ marloweQueryClientPeer <$> cont r
