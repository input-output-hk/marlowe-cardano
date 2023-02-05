

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.List
  ( allContracts
  , getContract
  ) where


import Data.Type.Equality ((:~:)(Refl))
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.App.Run (runMarloweSyncClient, runQueryClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(..), MarloweVersion, assertVersionsEqual)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId), DiscoveryQuery(..))
import Language.Marlowe.Runtime.History.Api (ContractStep, CreateStep)

import qualified Language.Marlowe.Protocol.Sync.Client as Sync
  ( ClientStFollow(ClientStFollow, recvMsgContractFound, recvMsgContractNotFound)
  , ClientStIdle(SendMsgDone, SendMsgRequestNext)
  , ClientStInit(SendMsgFollowContract)
  , ClientStNext(..)
  , ClientStWait(SendMsgCancel)
  , MarloweSyncClient(MarloweSyncClient)
  )
import qualified Network.Protocol.Query.Client as Query
  ( ClientStInit(SendMsgRequest)
  , ClientStNext(ClientStNext)
  , ClientStNextCanReject(..)
  , ClientStPage(..)
  , QueryClient(QueryClient)
  )


allContracts :: Client [ContractId]
allContracts = listContracts GetContractHeaders runDiscoveryQueryClient $ fmap contractId


listContracts
  :: Monoid a
  => query delimiter Void results
  -> (Services IO -> Query.QueryClient query IO a -> IO a)
  -> (results -> a)
  -> Client a
listContracts query run extract =
  let
    handleNextPage previous results nextPage =
      let
        cumulative = previous <> extract results
      in
        pure
          $ maybe
            (Query.SendMsgDone cumulative)
            (flip Query.SendMsgRequestNext . Query.ClientStNext $ handleNextPage cumulative)
            nextPage
  in
    runQueryClient run
      . Query.QueryClient
      . pure
      $ Query.SendMsgRequest query Query.ClientStNextCanReject
        { Query.recvMsgReject = absurd
        , Query.recvMsgNextPage = handleNextPage mempty
        }


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
    runMarloweSyncClient runHistorySyncClient
      . Sync.MarloweSyncClient
      . pure
      $ Sync.SendMsgFollowContract contractId' Sync.ClientStFollow
        {
          Sync.recvMsgContractNotFound = pure $ Left "Contract not found."
        , Sync.recvMsgContractFound = \_ version create ->
            case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
              Refl -> pure . Sync.SendMsgRequestNext $ stNext version create mempty
        }
