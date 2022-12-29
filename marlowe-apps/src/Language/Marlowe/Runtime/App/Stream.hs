

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , contractFromStep
  , contractFromStream
  , datumFromStep
  , datumFromStream
  , hasClosed
  , isContractStreamFinish
  , streamAllContractIds
  , streamAllContractIdsClient
  , streamAllContractSteps
  , streamAllContractStepsClient
  , streamContractHeaders
  , streamContractHeadersClient
  , streamContractSteps
  , streamContractStepsClient
  , transactionIdFromStream
  ) where


import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON(..), encode, object, (.=))
import Data.Maybe (isNothing, mapMaybe)
import Data.Type.Equality ((:~:)(Refl))
import Language.Marlowe.Core.V1.Semantics (MarloweData(marloweContract))
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.App.Run (runMarloweHeaderSyncClient, runMarloweSyncClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, TxId, TxOutRef(TxOutRef, txId))
import Language.Marlowe.Runtime.Core.Api
  ( ContractId
  , IsMarloweVersion(..)
  , MarloweVersion
  , MarloweVersionTag(V1)
  , Transaction(Transaction, output, transactionId)
  , TransactionOutput(TransactionOutput, scriptOutput)
  , TransactionScriptOutput(TransactionScriptOutput, datum, utxo)
  , assertVersionsEqual
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep(ApplyTransaction), CreateStep(CreateStep, createOutput))

import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1 (Contract)
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
  , ClientStWait(SendMsgCancel, SendMsgPoll)
  , MarloweSyncClient(MarloweSyncClient)
  )


streamAllContractIds
  :: Int
  -> TChan ContractId
  -> Client (Either String ())
streamAllContractIds pollingFrequency = streamContractHeaders pollingFrequency $ Just . contractId


streamAllContractIdsClient
  :: Int
  -> TChan ContractId
  -> MarloweHeaderSyncClient Client (Either String ())
streamAllContractIdsClient pollingFrequency = streamContractHeadersClient pollingFrequency $ Just . contractId


streamContractHeaders
  :: Int
  -> (ContractHeader -> Maybe a)
  -> TChan a
  -> Client (Either String ())
streamContractHeaders pollingFrequency extract channel =
  runMarloweHeaderSyncClient runDiscoverySyncClient
    $ streamContractHeadersClient pollingFrequency extract channel


streamContractHeadersClient
  :: Int
  -> (ContractHeader -> Maybe a)
  -> TChan a
  -> MarloweHeaderSyncClient Client (Either String ())
streamContractHeadersClient pollingFrequency extract channel =
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
      , HSync.recvMsgWait = clientWait <$ liftIO (threadDelay pollingFrequency)
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
  | ContractStreamWait
    {
      csContractId :: ContractId
    }
  | ContractStreamFinish
    {
      csContractId :: ContractId
    , csFinish :: Either String Bool  -- ^ Either an error message or an indication whether the contract closed.
    }
instance Show (ContractStream 'V1) where
  show = LBS8.unpack . encode

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
  toJSON ContractStreamWait{..} =
    object
      [
        "contractId" .= csContractId
      ]
  toJSON ContractStreamFinish{..} =
    object
      [
        "contractId" .= csContractId
      , "finish" .= csFinish
      ]


transactionIdFromStream
  :: ContractStream v
  -> Maybe TxId
transactionIdFromStream ContractStreamStart{csCreateStep=CreateStep{createOutput=TransactionScriptOutput{utxo=TxOutRef{txId}}}} = pure txId
transactionIdFromStream ContractStreamContinued{csContractStep=(ApplyTransaction Transaction{transactionId})} = pure transactionId
transactionIdFromStream _ = Nothing


isContractStreamFinish
  :: ContractStream v
  -> Bool
isContractStreamFinish ContractStreamFinish{} = True
isContractStreamFinish _ = False


datumFromStep
  :: Either (CreateStep v) (ContractStep v)
  -> Maybe (Datum v)
datumFromStep (Left CreateStep{createOutput=TransactionScriptOutput{datum}}) = pure datum
datumFromStep (Right (ApplyTransaction Transaction{output=TransactionOutput{scriptOutput=Just TransactionScriptOutput{datum}}})) = pure datum
datumFromStep _ = Nothing


contractFromStep
  :: Either (CreateStep 'V1) (ContractStep 'V1)
  -> Maybe V1.Contract
contractFromStep = fmap marloweContract . datumFromStep


datumFromStream
  :: ContractStream v
  -> Maybe (Datum v)
datumFromStream ContractStreamStart{csCreateStep} = datumFromStep $ Left csCreateStep
datumFromStream ContractStreamContinued{csContractStep} = datumFromStep $ Right csContractStep
datumFromStream _ = Nothing


contractFromStream
  :: ContractStream 'V1
  -> Maybe V1.Contract
contractFromStream = fmap marloweContract . datumFromStream


streamAllContractSteps
  :: forall v
  .  IsMarloweVersion v
  => Int
  -> Bool
  -> ContractId
  -> TChan (ContractStream v)
  -> Client ()
streamAllContractSteps pollingFrequency finishOnWait = streamContractSteps pollingFrequency True finishOnWait $ const True


streamAllContractStepsClient
  :: forall v
  .  IsMarloweVersion v
  => Int
  -> Bool
  -> ContractId
  -> TChan (ContractStream v)
  -> MarloweSyncClient Client ()
streamAllContractStepsClient pollingFrequency finishOnWait = streamContractStepsClient pollingFrequency True finishOnWait $ const True


hasClosed :: ContractStep v -> Bool
hasClosed (ApplyTransaction Transaction{output=TransactionOutput{..}}) = isNothing scriptOutput
hasClosed  _ = False


streamContractSteps
  :: forall v
  .  IsMarloweVersion v
  => Int
  -> Bool
  -> Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChan (ContractStream v)
  -> Client ()
streamContractSteps pollingFrequency finishOnClose finishOnWait accept csContractId channel =
  runMarloweSyncClient runHistorySyncClient
    $ streamContractStepsClient pollingFrequency finishOnClose finishOnWait accept csContractId channel


streamContractStepsClient
  :: forall v
  .  IsMarloweVersion v
  => Int
  -> Bool
  -> Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChan (ContractStream v)
  -> MarloweSyncClient Client ()
streamContractStepsClient pollingFrequency finishOnClose finishOnWait accept csContractId channel =
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
      , CSync.recvMsgWait =
          do
            -- FIXME: It would have been helpful if `recvMsgWait` had reported the current tip.
            liftIO . atomically
              . writeTChan channel
              $ ContractStreamWait csContractId
            if finishOnWait
              then pure . CSync.SendMsgCancel $ CSync.SendMsgDone ()
              else clientWait version <$ liftIO (threadDelay pollingFrequency)
      }
  in
    CSync.MarloweSyncClient
      $ pure clientInit
