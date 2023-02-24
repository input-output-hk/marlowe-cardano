
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Runtime.ChainSync.NodeClient
  where


import Cardano.Api (BlockInMode, CardanoMode, ChainPoint, QueryInMode, TxInMode, TxValidationErrorInMode)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, readTChan, writeTChan)
import Control.Concurrent.STM.TMVar (TMVar, newEmptyTMVarIO, putTMVar, takeTMVar)

import qualified Ouroboros.Network.Protocol.LocalStateQuery.Client as Q
import qualified Ouroboros.Network.Protocol.LocalStateQuery.Type as Q
import qualified Ouroboros.Network.Protocol.LocalTxSubmission.Client as S


type SubmitChannel = TChan SubmitJob


data SubmitJob =
  SubmitJob
  {
    tx :: TxInMode CardanoMode
  , submitResultTMVar :: TMVar (S.SubmitResult (TxValidationErrorInMode CardanoMode))
  }


submitTxToNodeChannel
  :: SubmitChannel
  -> TxInMode CardanoMode
  -> IO (S.SubmitResult (TxValidationErrorInMode CardanoMode))
submitTxToNodeChannel channel tx =
  do
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



type QueryChannel = TChan (Q.Some QueryJob)


data QueryJob result =
  QueryJob
  {
    point :: Maybe ChainPoint
  , query :: QueryInMode CardanoMode result
  , queryResultTMVar :: TMVar (Either Q.AcquireFailure result)
  }


queryNodeChannel
  :: QueryChannel
  -> Maybe ChainPoint
  -> QueryInMode CardanoMode result
  -> IO (Either Q.AcquireFailure result)
queryNodeChannel channel point query =
  do
    queryResultTMVar <- newEmptyTMVarIO
    atomically $ writeTChan channel $ Q.Some QueryJob{..}
    atomically $ takeTMVar queryResultTMVar


queryClient
  :: QueryChannel
  -> Q.LocalStateQueryClient (BlockInMode CardanoMode) ChainPoint (QueryInMode CardanoMode) IO ()
queryClient channel =
  let
    next =
      do
        Q.Some QueryJob{..} <- atomically $ readTChan channel
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
