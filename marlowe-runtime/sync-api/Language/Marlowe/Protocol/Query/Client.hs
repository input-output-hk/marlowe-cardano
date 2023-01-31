{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Protocol.Query.Client
  where

import Data.Void (absurd)
import Language.Marlowe.Protocol.Query.Types
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.TypedProtocol

type MarloweQueryClient = Peer MarloweQuery 'AsClient 'StInit

getContractHeaders :: Range ContractId -> MarloweQueryClient m (Page ContractId ContractHeader)
getContractHeaders range =
  Yield (ClientAgency TokInit) (MsgGetContractHeaders range) $
  Await (ServerAgency TokGetContractHeaders) \case
    MsgResolve page -> Done TokDone page
    MsgReject err -> absurd err
