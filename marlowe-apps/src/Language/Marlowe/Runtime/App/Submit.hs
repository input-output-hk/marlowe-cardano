{-# LANGUAGE NumericUnderscores #-}

module Language.Marlowe.Runtime.App.Submit (
  submit,
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Language.Marlowe.Runtime.App.Types (Client)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (
  TxId,
 )
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand (Submit))

import Cardano.Api (BabbageEraOnwards)
import qualified Cardano.Api as C (
  Tx,
  getTxBody,
  getTxId,
 )
import qualified Network.Protocol.Job.Client as J (
  ClientStAwait (..),
  ClientStCmd (..),
  ClientStInit (..),
  JobClient (..),
 )

submit
  :: Int
  -> BabbageEraOnwards era
  -> C.Tx era
  -> Client (Either String TxId)
submit pollingFrequency era tx =
  let next =
        J.ClientStCmd
          { J.recvMsgAwait = \_ _ -> do
              liftIO $ threadDelay $ pollingFrequency * 1_000_000
              pure $ J.SendMsgPoll next
          , J.recvMsgFail = const . pure $ Left "Submission failed."
          , J.recvMsgSucceed = const . pure . Right . fromCardanoTxId . C.getTxId $ C.getTxBody tx
          }
      jobClient = J.JobClient . pure $ J.SendMsgExec (Submit era tx) next
   in runMarloweTxClient jobClient
