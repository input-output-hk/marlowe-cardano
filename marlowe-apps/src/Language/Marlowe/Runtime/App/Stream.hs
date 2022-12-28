

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , hasClosed
  , streamAllContractIds
  , streamAllContractIdsClient
  , streamAllContractSteps
  , streamAllContractStepsClient
  , streamContractHeaders
  , streamContractHeadersClient
  , streamContractSteps
  , streamContractStepsClient
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), object, (.=))
import Data.Maybe (isNothing, mapMaybe)
import Data.Type.Equality ((:~:)(Refl))
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.App.Run (runMarloweHeaderSyncClient, runMarloweSyncClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId
  , IsMarloweVersion(..)
  , MarloweVersion
  , MarloweVersionTag(V1)
  , Transaction(Transaction, output)
  , TransactionOutput(TransactionOutput, scriptOutput)
  , assertVersionsEqual
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep(ApplyTransaction), CreateStep)

import qualified Language.Marlowe.Protocol.HeaderSync.Client as HSync
  ( ClientStIdle(SendMsgRequestNext)
  , ClientStNext(..)
  , ClientStWait(SendMsgPoll)
  , MarloweHeaderSyncClient(MarloweHeaderSyncClient)
  )
import qualified Language.Marlowe.Protocol.Sync.Client as CSync
  ( ClientStFollow(ClientStFollow, recvMsgContractFound, recvMsgContractNotFound)
  , ClientStIdle(SendMsgDone, SendMsgRequestNext)
  , ClientStInit(SendMsgFollowContract)
  , ClientStNext(..)
  , ClientStWait(SendMsgPoll)
  , MarloweSyncClient(MarloweSyncClient)
  )


streamAllContractIds
  :: TChan ContractId
  -> Client (Either String ())
streamAllContractIds = streamContractHeaders $ Just . contractId


streamAllContractIdsClient
  :: TChan ContractId
  -> MarloweHeaderSyncClient Client (Either String ())
streamAllContractIdsClient = streamContractHeadersClient $ Just . contractId


streamContractHeaders
  :: (ContractHeader -> Maybe a)
  -> TChan a
  -> Client (Either String ())
streamContractHeaders extract channel =
  runMarloweHeaderSyncClient runDiscoverySyncClient
    $ streamContractHeadersClient extract channel


streamContractHeadersClient
  :: (ContractHeader -> Maybe a)
  -> TChan a
  -> MarloweHeaderSyncClient Client (Either String ())
streamContractHeadersClient extract channel =
  let
    clientIdle = HSync.SendMsgRequestNext clientNext
    clientWait = HSync.SendMsgPoll clientNext
    clientNext =
      HSync.ClientStNext
      {
        HSync.recvMsgNewHeaders = \_ results -> do
                                                 liftIO . atomically
                                                   . mapM_ (writeTChan channel)
                                                   $ mapMaybe extract results
                                                 pure clientIdle
      , HSync.recvMsgRollBackward = const $ pure clientIdle
      , HSync.recvMsgWait = clientWait <$ liftIO (threadDelay 5_000_000)
      }
  in
    HSync.MarloweHeaderSyncClient
      $ pure clientIdle


data ContractStream v =
    ContractStreamStart
    {
      csContractId :: ContractId
    , csBlockHeader :: BlockHeader
    , csCreateStep :: CreateStep v
    }
  | ContractStreamContinued
    {
      csContractId :: ContractId
    , csBlockHeader :: BlockHeader
    , csContractStep :: ContractStep v
    }
  | ContractStreamRolledBack
    {
      csContractId :: ContractId
    , csBlockHeader :: BlockHeader
    }
  | ContractStreamFinish
    {
      csContractId :: ContractId
    , csFinish :: Either String Bool  -- ^ Either an error message or an indication whether the contract closed.
    }

instance ToJSON (ContractStream 'V1) where
  toJSON ContractStreamStart{..} =
    object
      [
        "contractId" .= csContractId
      , "blockHeader" .= csBlockHeader
      , "createStep" .= csCreateStep
      ]
  toJSON ContractStreamContinued{..} =
    object
      [
        "contractId" .= csContractId
      , "blockHeader" .= csBlockHeader
      , "contractStep" .= csContractStep
      ]
  toJSON ContractStreamRolledBack{..} =
    object
      [
        "contractId" .= csContractId
      , "blockHeader" .= csBlockHeader
      ]
  toJSON ContractStreamFinish{..} =
    object
      [
        "contractId" .= csContractId
      , "finish" .= csFinish
      ]


streamAllContractSteps
  :: forall v
  .  IsMarloweVersion v
  => ContractId
  -> TChan (ContractStream v)
  -> Client ()
streamAllContractSteps = streamContractSteps True $ const True


streamAllContractStepsClient
  :: forall v
  .  IsMarloweVersion v
  => ContractId
  -> TChan (ContractStream v)
  -> MarloweSyncClient Client ()
streamAllContractStepsClient = streamContractStepsClient True $ const True


hasClosed :: ContractStep v -> Bool
hasClosed (ApplyTransaction Transaction{output=TransactionOutput{..}}) = isNothing scriptOutput
hasClosed  _ = False


streamContractSteps
  :: forall v
  .  IsMarloweVersion v
  => Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChan (ContractStream v)
  -> Client ()
streamContractSteps finishOnClose accept csContractId channel =
  runMarloweSyncClient runHistorySyncClient
    $ streamContractStepsClient finishOnClose accept csContractId channel


streamContractStepsClient
  :: forall v
  .  IsMarloweVersion v
  => Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChan (ContractStream v)
  -> MarloweSyncClient Client ()
streamContractStepsClient finishOnClose accept csContractId channel =
  let
    clientInit =
      CSync.SendMsgFollowContract csContractId
        CSync.ClientStFollow
        {
          CSync.recvMsgContractNotFound = liftIO . atomically
                                         . writeTChan channel
                                         . ContractStreamFinish csContractId
                                         $ Left "Contract not found."
        , CSync.recvMsgContractFound = \csBlockHeader version csCreateStep ->
            case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
              Refl -> if accept $ Left csCreateStep
                        then do
                               liftIO . atomically
                                 $ writeTChan channel
                                   ContractStreamStart{..}
                               pure $ clientIdle version
                        else do
                               liftIO . atomically
                                 . writeTChan channel
                                 . ContractStreamFinish csContractId
                                 $ Right False
                               pure $ CSync.SendMsgDone ()
        }
    clientIdle = CSync.SendMsgRequestNext . clientNext
    clientWait = CSync.SendMsgPoll . clientNext
    clientNext :: MarloweVersion v -> CSync.ClientStNext v Client ()
    clientNext version =
      CSync.ClientStNext
      {
        CSync.recvMsgRollBackCreation =
          liftIO . atomically
            . writeTChan channel
            . ContractStreamFinish csContractId
            $ Left "Creation transaction was rolled back."
      , CSync.recvMsgRollBackward = \csBlockHeader -> do
          liftIO . atomically
            $ writeTChan channel
              ContractStreamRolledBack{..}
          pure $ clientIdle version
      , CSync.recvMsgRollForward = \csBlockHeader steps -> do
          let
            acceptances = accept . Right <$> steps
          liftIO . atomically
            . mapM_ (\csContractStep -> writeTChan channel ContractStreamContinued{..})
            $ fst <$> takeWhile snd (zip steps acceptances)
          if and acceptances
            then if finishOnClose && any hasClosed steps
                   then do
                          liftIO . atomically
                            . writeTChan channel
                            . ContractStreamFinish csContractId
                            $ Right True
                          pure $ CSync.SendMsgDone ()
                   else pure $ clientIdle version
            else do
                   liftIO . atomically
                     . writeTChan channel
                     . ContractStreamFinish csContractId
                     $ Right False
                   pure $ CSync.SendMsgDone ()
      , CSync.recvMsgWait = clientWait version <$ liftIO (threadDelay 5_000_000)
      }
  in
    CSync.MarloweSyncClient
      $ pure clientInit
