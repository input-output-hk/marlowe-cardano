{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.List
  ( allContracts
  , allHeaders
  , getContract
  ) where


import Data.Type.Equality ((:~:)(Refl))
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweVersion, assertVersionsEqual)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)

import qualified Language.Marlowe.Protocol.Query.Client as Query
import Language.Marlowe.Protocol.Query.Types (ContractFilter)
import qualified Language.Marlowe.Protocol.Query.Types as Query
import qualified Language.Marlowe.Protocol.Sync.Client as Sync
  ( ClientStFollow(ClientStFollow, recvMsgContractFound, recvMsgContractNotFound)
  , ClientStIdle(SendMsgDone, SendMsgRequestNext)
  , ClientStInit(SendMsgFollowContract)
  , ClientStNext(..)
  , ClientStWait(SendMsgCancel)
  , MarloweSyncClient(MarloweSyncClient)
  )
import Language.Marlowe.Runtime.Client (runMarloweQueryClient, runMarloweSyncClient)


allContracts :: ContractFilter -> Client [ContractId]
allContracts = (fmap . fmap) contractId . listContracts


allHeaders :: ContractFilter -> Client [ContractHeader]
allHeaders = listContracts


pageSize :: Int
pageSize = 1024


listContracts :: ContractFilter -> Client [ContractHeader]
listContracts cFilter =
  let
    append = (. Query.getContractHeaders cFilter) . (=<<) . handleNextPage
    handleNextPage previous Nothing = pure previous
    handleNextPage previous (Just Query.Page{..}) =
      let
        cumulative = previous <> items
      in
        case nextRange of
          Nothing    -> pure cumulative
          Just range -> cumulative `append` range
  in
    runMarloweQueryClient $ mempty `append` Query.Range Nothing 0 pageSize Query.Ascending


getContract
  :: forall v
  .  IsMarloweVersion v
  => ContractId
  -> Client (Either String (CreateStep v, [ContractStep v]))
getContract contractId' =
  let
    stNext version create previous =
      Sync.ClientStNext
      {
        Sync.recvMsgRollBackCreation = pure $ Left "Creation transaction was rolled back."
      , Sync.recvMsgRollBackward = const . pure . Sync.SendMsgDone $ Left "Input application was rolled back."
      , Sync.recvMsgRollForward = \_ steps -> pure
                                                . Sync.SendMsgRequestNext
                                                . stNext version create
                                                $ previous <> steps
      , Sync.recvMsgWait = pure
                             . Sync.SendMsgCancel
                             . Sync.SendMsgDone
                             $ Right (create, previous)
      }
  in
    runMarloweSyncClient
      . Sync.MarloweSyncClient
      . pure
      $ Sync.SendMsgFollowContract contractId' Sync.ClientStFollow
        {
          Sync.recvMsgContractNotFound = pure $ Left "Contract not found."
        , Sync.recvMsgContractFound = \_ version create ->
            case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
              Refl -> pure . Sync.SendMsgRequestNext $ stNext version create mempty
        }
