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
import Language.Marlowe.Runtime.App.Run (runMarloweSyncClient, runQueryClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweVersion, assertVersionsEqual)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)

import qualified Language.Marlowe.Protocol.Query.Client as Query
import qualified Language.Marlowe.Protocol.Query.Types as Query
import qualified Language.Marlowe.Protocol.Sync.Client as Sync
  ( ClientStFollow(ClientStFollow, recvMsgContractFound, recvMsgContractNotFound)
  , ClientStIdle(SendMsgDone, SendMsgRequestNext)
  , ClientStInit(SendMsgFollowContract)
  , ClientStNext(..)
  , ClientStWait(SendMsgCancel)
  , MarloweSyncClient(MarloweSyncClient)
  )


allContracts :: Client [ContractId]
allContracts = listContracts runSyncQueryClient $ fmap contractId


allHeaders :: Client [ContractHeader]
allHeaders = listContracts runSyncQueryClient id


pageSize :: Int
pageSize = 1024


listContracts
  :: Monoid a
  -- TODO consider refactoring this - it is a bit confusing to pass around these
  -- handler functions that have only one implementation, and I'm not sure what
  -- benefit it adds.
  => (forall x. Services IO -> Query.MarloweQueryClient IO x -> IO x)
  -> ([ContractHeader] -> a)
  -> Client a
listContracts run =
  let
    bundleContracts getContractHeaders extract =
      let
        append = (. getContractHeaders) . (=<<) . handleNextPage
        handleNextPage previous Nothing = pure previous
        handleNextPage previous (Just Query.Page{..}) =
          let
            cumulative = previous <> extract items
          in
            case nextRange of
              Nothing    -> pure cumulative
              Just range -> cumulative `append` range
      in
        mempty `append` Query.Range Nothing 0 pageSize Query.Ascending
  in
    bundleContracts
      $ runQueryClient run . Query.getContractHeaders


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
    runMarloweSyncClient runSyncSyncClient
      . Sync.MarloweSyncClient
      . pure
      $ Sync.SendMsgFollowContract contractId' Sync.ClientStFollow
        {
          Sync.recvMsgContractNotFound = pure $ Left "Contract not found."
        , Sync.recvMsgContractFound = \_ version create ->
            case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
              Refl -> pure . Sync.SendMsgRequestNext $ stNext version create mempty
        }
