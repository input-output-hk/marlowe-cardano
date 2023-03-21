{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}


module Language.Marlowe.Runtime.ChainSync.NodeClient
  ( NodeClient(..)
  , NodeClientDependencies(..)
  , NodeClientSelector(..)
  , QueryNode
  , SubmitToNode
  , getNodeClientSelectorConfig
  , nodeClient
  ) where


import Cardano.Api
  ( BlockInMode
  , CardanoMode
  , ChainPoint
  , ChainTip
  , EraInMode(..)
  , LocalChainSyncClient(LocalChainSyncClient)
  , LocalNodeClientProtocols(..)
  , LocalNodeClientProtocolsInMode
  , QueryInMode
  , TxInMode(TxInMode)
  , TxValidationErrorInMode
  , chainTipToChainPoint
  , serialiseToTextEnvelope
  )
import Cardano.Api.ChainSync.Client (ChainSyncClient(..), ClientStIdle(..), ClientStIntersect(..), ClientStNext(..))
import Cardano.Api.Shelley (AcquiringFailure(..))
import Control.Concurrent.Component (Component, component)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)
import Data.Aeson (Value(String), toJSON)
import Data.Bifunctor (first)
import qualified Data.Text as T (pack)
import Data.Void (Void)
import Observe.Event.Component
  (GetSelectorConfig, SelectorConfig(..), SomeJSON(..), absurdFieldConfig, singletonFieldConfigWith)
import Observe.Event.Explicit (EventBackend, addField, withEvent)

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Q
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Q
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as S


type SubmitToNode
  =  TxInMode CardanoMode
  -> IO (S.SubmitResult (TxValidationErrorInMode CardanoMode))


type QueryNode result
  =  Maybe ChainPoint
  -> QueryInMode CardanoMode result
  -> IO (Either AcquiringFailure result)


data NodeClient = NodeClient
  {
    submitTxToNode  :: SubmitToNode
  , queryNode :: forall result . QueryNode result
  }


data NodeClientDependencies r = NodeClientDependencies
  {
    connectToLocalNode :: !(LocalNodeClientProtocolsInMode CardanoMode -> IO ())
  , eventBackend :: !(EventBackend IO r NodeClientSelector)
  }


data NodeClientSelector f where
  Connect :: NodeClientSelector Void
  Submit :: NodeClientSelector (TxInMode CardanoMode)
  Query :: NodeClientSelector (Q.Some (QueryInMode CardanoMode))


getNodeClientSelectorConfig :: GetSelectorConfig NodeClientSelector
getNodeClientSelectorConfig Connect = SelectorConfig "connect" True absurdFieldConfig
getNodeClientSelectorConfig Submit =
  SelectorConfig "submit" True $ singletonFieldConfigWith (SomeJSON . txToJSON) "tx" True
  where
    txToJSON :: TxInMode CardanoMode -> Value
    txToJSON (TxInMode tx ShelleyEraInCardanoMode) = toJSON $ serialiseToTextEnvelope Nothing tx
    txToJSON (TxInMode tx AllegraEraInCardanoMode) = toJSON $ serialiseToTextEnvelope Nothing tx
    txToJSON (TxInMode tx MaryEraInCardanoMode) = toJSON $ serialiseToTextEnvelope Nothing tx
    txToJSON (TxInMode tx AlonzoEraInCardanoMode) = toJSON $ serialiseToTextEnvelope Nothing tx
    txToJSON (TxInMode tx BabbageEraInCardanoMode) = toJSON $ serialiseToTextEnvelope Nothing tx
    txToJSON _ = String "<<a transaction>>"
getNodeClientSelectorConfig Query =
  SelectorConfig "query" True $ singletonFieldConfigWith (SomeJSON . queryToJSON) "query" True
  where
    queryToJSON :: Q.Some (QueryInMode CardanoMode) -> Value
    queryToJSON (Q.Some query) = String . T.pack $ show query


nodeClient :: Component IO (NodeClientDependencies r) NodeClient
nodeClient =
  component \NodeClientDependencies{..} ->
    do
      queryChannel <- newTChan
      submitChannel <- newTChan
      let
        runNodeClient = withEvent eventBackend Connect \_ ->
          connectToLocalNode LocalNodeClientProtocols
          {
            -- We don't use the block and tip information, but the chain
            -- sync client keeps the connection open. Without this client,
            -- the node would close the connection in about three seconds,
            -- making it impossible to submit and query.
            localChainSyncClient = LocalChainSyncClient chainSyncClient
          , localTxSubmissionClient = Just $ submitClient submitChannel
          , localTxMonitoringClient = Nothing
          , localStateQueryClient = Just $ queryClient queryChannel
          }
      pure
        (
          runNodeClient
        , NodeClient
          {
            submitTxToNode = submitTxToNodeChannel eventBackend submitChannel
          , queryNode = queryNodeChannel eventBackend queryChannel
          }
        )


type SubmitChannel = TChan SubmitJob


data SubmitJob =
  SubmitJob
  {
    tx :: TxInMode CardanoMode
  , submitResultTMVar :: TMVar (S.SubmitResult (TxValidationErrorInMode CardanoMode))
  }


submitTxToNodeChannel
  :: EventBackend IO r NodeClientSelector
  -> SubmitChannel
  -> TxInMode CardanoMode
  -> IO (S.SubmitResult (TxValidationErrorInMode CardanoMode))
submitTxToNodeChannel eventBackend channel tx =
  withEvent eventBackend Submit \event ->
    do
      event `addField` tx
      submitResultTMVar <- newEmptyTMVarIO
      atomically $ writeTChan channel SubmitJob{..}
      atomically $ takeTMVar submitResultTMVar


submitClient
  :: SubmitChannel
  -> S.LocalTxSubmissionClient (TxInMode CardanoMode) (TxValidationErrorInMode CardanoMode) IO ()
submitClient channel =
  let
    next =
      do
        SubmitJob{..} <- atomically $ readTChan channel
        pure . S.SendMsgSubmitTx tx
          $ \result ->
            do
              atomically $ putTMVar submitResultTMVar result
              next
  in
    S.LocalTxSubmissionClient next



type QueryChannel = TChan QueryJob


data QueryJob = forall result .
  QueryJob
  {
    point :: Maybe ChainPoint
  , query :: QueryInMode CardanoMode result
  , queryResultTMVar :: TMVar (Either Q.AcquireFailure result)
  }


queryNodeChannel
  :: EventBackend IO r NodeClientSelector
  -> QueryChannel
  -> Maybe ChainPoint
  -> QueryInMode CardanoMode result
  -> IO (Either AcquiringFailure result)
queryNodeChannel eventBackend channel point query =
  withEvent eventBackend Query \event ->
    do
      event `addField` Q.Some query
      let
        toAcquiringFailure Q.AcquireFailurePointTooOld = AFPointTooOld
        toAcquiringFailure Q.AcquireFailurePointNotOnChain = AFPointNotOnChain
      queryResultTMVar <- newEmptyTMVarIO
      atomically . writeTChan channel $ QueryJob{..}
      atomically $ first toAcquiringFailure <$> takeTMVar queryResultTMVar


queryClient
  :: QueryChannel
  -> Q.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
queryClient channel =
  let
    next =
      do
        QueryJob{..} <- atomically $ readTChan channel
        pure . Q.SendMsgAcquire point
          $ Q.ClientStAcquiring
            {
              recvMsgAcquired =
                pure . Q.SendMsgQuery query
                  $ Q.ClientStQuerying
                    {
                      recvMsgResult = \result ->
                        do
                          atomically . putTMVar queryResultTMVar $ Right result
                          pure $ Q.SendMsgRelease next
                    }
            , recvMsgFailure = \failure ->
                do
                  atomically . putTMVar queryResultTMVar $ Left failure
                  next  -- Allow future queries after an acquisition failure.
            }
  in
    Q.LocalStateQueryClient next


chainSyncClient :: ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
chainSyncClient =
  let
    stStart :: ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    stStart = SendMsgRequestNext stFirst $ pure stNext
    stFirst :: ClientStNext (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    stFirst =
      ClientStNext
      {
        recvMsgRollForward  = \_ tip -> ChainSyncClient . pure $ stTip tip  -- Identified the tip, now intersect with it.
      , recvMsgRollBackward = \_ tip -> ChainSyncClient . pure $ stTip tip  -- Identified the tip, now intersect with it.
      }
    stTip :: ChainTip -> ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    stTip tip =
      SendMsgFindIntersect [chainTipToChainPoint tip]
        $ ClientStIntersect
          {
            recvMsgIntersectFound    = \_ _  -> ChainSyncClient $ pure stIdle        -- The tip was found, so follow it.
          , recvMsgIntersectNotFound = \tip' -> ChainSyncClient . pure $ stTip tip'  -- The tip was not found, so try again.
          }
    stIdle :: ClientStIdle (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    stIdle = SendMsgRequestNext stNext $ pure stNext
    stNext :: ClientStNext (BlockInMode CardanoMode) ChainPoint ChainTip IO ()
    stNext =
      ClientStNext
      {
        recvMsgRollForward  = \_ _ -> ChainSyncClient $ pure stIdle  -- Continuing following the tip.
      , recvMsgRollBackward = \_ _ -> ChainSyncClient $ pure stIdle  -- Continuing following the tip.
      }
  in
    ChainSyncClient $ pure stStart
