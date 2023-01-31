{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.Query.Server
  where

import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol

type MarloweQueryServer = Peer MarloweQuery 'AsServer 'StInit

marloweQueryServer
  :: Monad m
  => (Range ContractId -> m (Page ContractId ContractHeader))
  -> MarloweQueryServer m ()
marloweQueryServer getContractHeaders = Await (ClientAgency TokInit) \case
  MsgGetContractHeaders range -> Effect do
    page <- getContractHeaders range
    pure $
      Yield (ServerAgency TokGetContractHeaders) (MsgResolve page) $
      Done TokDone ()
