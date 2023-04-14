{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Stream
  ( ContractStream(..)
  , ContractStreamError(..)
  , EOF(..)
  , SyncEvent
  , TChanEOF
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
import Data.Text (Text)
import Data.Type.Equality ((:~:)(Refl))
import Language.Marlowe.Core.V1.Semantics (MarloweData(marloweContract))
import Language.Marlowe.Protocol.HeaderSync.Client (MarloweHeaderSyncClient)
import Language.Marlowe.Protocol.Sync.Client (MarloweSyncClient)
import Language.Marlowe.Runtime.App.Types (Client, PollingFrequency(PollingFrequency))
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, TxId, TxOutRef(TxOutRef, txId))
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
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(blockHeader, contractId))
import Language.Marlowe.Runtime.History.Api (ContractStep(ApplyTransaction), CreateStep(CreateStep, createOutput))
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import Observe.Event.Syntax ((≔))

import Control.Arrow ((&&&))
import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1 (Contract)
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HSync
  (ClientStIdle(..), ClientStNext(..), ClientStWait(..), MarloweHeaderSyncClient(MarloweHeaderSyncClient))
import qualified Language.Marlowe.Protocol.Sync.Client as CSync
  ( ClientStFollow(ClientStFollow, recvMsgContractFound, recvMsgContractNotFound)
  , ClientStIdle(SendMsgDone, SendMsgRequestNext)
  , ClientStInit(SendMsgFollowContract)
  , ClientStNext(..)
  , ClientStWait(SendMsgCancel, SendMsgPoll)
  , MarloweSyncClient(MarloweSyncClient)
  )
import Language.Marlowe.Runtime.Client (runMarloweHeaderSyncClient, runMarloweSyncClient)

-- | Either a rollback or some new contract data
type SyncEvent a = Either ChainPoint (BlockHeader, a)

data EOF = EOF

type TChanEOF a = TChan (Either EOF a)

streamAllContractIds
  :: EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> TChanEOF (SyncEvent ContractId)
  -> Client (Maybe ContractStreamError)
streamAllContractIds eventBackend pollingFrequency endOnWait = streamContractHeaders eventBackend pollingFrequency endOnWait $ Just . fmap (blockHeader &&& contractId)


streamAllContractIdsClient
  :: EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> TChanEOF (SyncEvent ContractId)
  -> MarloweHeaderSyncClient Client (Maybe ContractStreamError)
streamAllContractIdsClient eventBackend pollingFrequency endOnWait = streamContractHeadersClient eventBackend pollingFrequency endOnWait $ Just . fmap (blockHeader &&& contractId)


streamContractHeaders
  :: EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> (Either ChainPoint ContractHeader -> Maybe a)
  -> TChanEOF a
  -> Client (Maybe ContractStreamError)
streamContractHeaders eventBackend pollingFrequency endOnWait extract channel =
  runMarloweHeaderSyncClient $ streamContractHeadersClient eventBackend pollingFrequency endOnWait extract channel


streamContractHeadersClient
  :: EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> (Either ChainPoint ContractHeader -> Maybe a)
  -> TChanEOF a
  -> MarloweHeaderSyncClient Client (Maybe ContractStreamError)
streamContractHeadersClient eventBackend (PollingFrequency pollingFrequency) endOnWait extract channel =
  let
    clientIdle = HSync.SendMsgRequestNext clientNext
    clientWait
      | endOnWait = do
          atomically $ writeTChan channel $ Left EOF
          pure $ HSync.SendMsgCancel $ HSync.SendMsgDone Nothing
      | otherwise = HSync.SendMsgPoll clientNext <$ threadDelay (fromIntegral pollingFrequency)
    clientNext =
      HSync.ClientStNext
      {
        HSync.recvMsgNewHeaders = \blockHeader results ->
          liftIO . withEvent eventBackend (DynamicEventSelector "HeadersClientNew")
            $ \event ->
              do
                addField event $ ("blockHeader" :: Text) ≔ blockHeader
                addField event $ ("preFilterCount" :: Text) ≔ length results
                let
                  extracted = mapMaybe (extract . Right) results
                addField event $ ("postFilterCount" :: Text) ≔ length extracted
                atomically $ mapM_ (writeTChan channel . Right) extracted
                pure clientIdle
      , HSync.recvMsgRollBackward = \chainPoint ->
          liftIO . withEvent eventBackend (DynamicEventSelector "HeadersClientRollback")
            $ \event ->
              do
                addField event $ ("blockHeader" :: Text) ≔ chainPoint
                let
                  extracted = extract $ Left chainPoint
                atomically $ mapM_ (writeTChan channel . Right) extracted
                pure clientIdle
      , HSync.recvMsgWait =
          liftIO
            . withEvent eventBackend (DynamicEventSelector "HeadersClientWait")
            . const
            $ clientWait
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
    , csFinish :: Maybe ContractStreamError
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


data ContractStreamError =
    ContractNotFound
  | ContractCreationRolledback
  | ContractMarloweVersionMismatch
  | ContractRejected
  deriving Show

instance ToJSON ContractStreamError where
  toJSON = toJSON . show

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
  :: IsMarloweVersion v
  => EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> ContractId
  -> TChanEOF (ContractStream v)
  -> Client ()
streamAllContractSteps eventBackend pollingFrequency finishOnWait =
  streamContractSteps eventBackend pollingFrequency True finishOnWait
    $ const True


streamAllContractStepsClient
  :: IsMarloweVersion v
  => EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> ContractId
  -> TChanEOF (ContractStream v)
  -> MarloweSyncClient Client ()
streamAllContractStepsClient eventBackend pollingFrequency finishOnWait =
  streamContractStepsClient eventBackend pollingFrequency True finishOnWait
    $ const True


hasClosed :: ContractStep v -> Bool
hasClosed (ApplyTransaction Transaction{output=TransactionOutput{..}}) = isNothing scriptOutput
hasClosed  _ = False


streamContractSteps
  :: IsMarloweVersion v
  => EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChanEOF (ContractStream v)
  -> Client ()
streamContractSteps eventBackend pollingFrequency finishOnClose finishOnWait accept csContractId channel =
  runMarloweSyncClient $ streamContractStepsClient eventBackend pollingFrequency finishOnClose finishOnWait accept csContractId channel


streamContractStepsClient
  :: forall v r
  .  IsMarloweVersion v
  => EventBackend IO r DynamicEventSelector
  -> PollingFrequency
  -> Bool
  -> Bool
  -> (Either (CreateStep v) (ContractStep v) -> Bool)
  -> ContractId
  -> TChanEOF (ContractStream v)
  -> MarloweSyncClient Client ()
streamContractStepsClient eventBackend (PollingFrequency pollingFrequency) finishOnClose finishOnWait accept csContractId channel =
  let
    clientInit =
      CSync.SendMsgFollowContract csContractId
        CSync.ClientStFollow
        {
          CSync.recvMsgContractNotFound =
            liftIO . withEvent eventBackend (DynamicEventSelector "StepsClientContractNotFound")
              $ \event ->
                do
                  addField event $ ("contractId" :: Text) ≔ csContractId
                  atomically . writeTChan channel
                    . Right
                    . ContractStreamFinish csContractId
                    $ Just ContractNotFound
        , CSync.recvMsgContractFound = \csBlockHeader version csCreateStep ->
            liftIO . withEvent eventBackend (DynamicEventSelector "StepsClientContractFound")
              $ \event ->
                do
                  addField event $ ("contractId" :: Text) ≔ csContractId
                  addField event $ ("blockHeader" :: Text) ≔ csBlockHeader
--                addField event $ ("createStep" :: Text) ≔ csCreateStep
                  case version `assertVersionsEqual` (marloweVersion :: MarloweVersion v) of
                    Refl -> do
                              let
                                accepted = accept $ Left csCreateStep
                              addField event $ ("accepted" :: Text) ≔ accepted
                              if accepted
                                then do
                                       atomically . writeTChan channel
                                         $ Right ContractStreamStart{..}
                                       pure $ clientIdle version
                                else do
                                       atomically . writeTChan channel
                                         . Right
                                         . ContractStreamFinish csContractId
                                         $ Just ContractMarloweVersionMismatch
                                       pure $ CSync.SendMsgDone ()
        }
    clientIdle = CSync.SendMsgRequestNext . clientNext
    clientWait = CSync.SendMsgPoll . clientNext
    clientNext :: MarloweVersion v -> CSync.ClientStNext v Client ()
    clientNext version =
      CSync.ClientStNext
      {
        CSync.recvMsgRollBackCreation =
          liftIO . withEvent eventBackend (DynamicEventSelector "StepsClientCreateRollback")
            $ \event ->
              do
                addField event $ ("contractId" :: Text) ≔ csContractId
                atomically . writeTChan channel
                  . Right
                  . ContractStreamFinish csContractId
                  $ Just ContractCreationRolledback
      , CSync.recvMsgRollBackward = \csBlockHeader ->
          liftIO . withEvent eventBackend (DynamicEventSelector "StepsClientApplyRollback")
            $ \event ->
              do
                addField event $ ("contractId" :: Text) ≔ csContractId
                addField event $ ("blockHeader" :: Text) ≔ csBlockHeader
                atomically . writeTChan channel
                  $ Right ContractStreamRolledBack{..}
                pure $ clientIdle version
      , CSync.recvMsgRollForward = \csBlockHeader steps ->
          liftIO . withEvent eventBackend (DynamicEventSelector "StepsClientApplyForward")
            $ \event ->
              do
                addField event $ ("contractId" :: Text) ≔ csContractId
                addField event $ ("blockHeader" :: Text) ≔ csBlockHeader
                addField event $ ("stepsCount" :: Text) ≔ length steps
--              addField event $ ("steps" :: Text) ≔ steps
                let
                  acceptances = accept . Right <$> steps
                addField event $ ("acceptancesCount" :: Text) ≔ length (filter id acceptances)
                addField event $ ("rejectionsCount" :: Text) ≔ length (filter not acceptances)
                atomically
                  . mapM_ (\csContractStep -> writeTChan channel $ Right ContractStreamContinued{..})
                  $ fst <$> takeWhile snd (zip steps acceptances)
                if and acceptances
                  then if finishOnClose && any hasClosed steps
                         then do
                                atomically . writeTChan channel
                                  . Right
                                  . ContractStreamFinish csContractId
                                  $ Nothing
                                pure $ CSync.SendMsgDone ()
                         else pure $ clientIdle version
                  else do
                         atomically . writeTChan channel
                           . Right
                           . ContractStreamFinish csContractId
                           $ Just ContractRejected
                         pure $ CSync.SendMsgDone ()
      , CSync.recvMsgWait =
          liftIO . withEvent eventBackend (DynamicEventSelector "StepsClientWait")
            $ \event ->
              do
                addField event $ ("contractId" :: Text) ≔ csContractId
                -- FIXME: It would have been helpful if `recvMsgWait` had reported the current tip.
                atomically . writeTChan channel
                  . Right
                  $ ContractStreamWait csContractId
                if finishOnWait
                  then pure . CSync.SendMsgCancel $ CSync.SendMsgDone ()
                  else clientWait version <$ threadDelay (fromIntegral pollingFrequency)
      }
  in
    CSync.MarloweSyncClient
      $ pure clientInit
