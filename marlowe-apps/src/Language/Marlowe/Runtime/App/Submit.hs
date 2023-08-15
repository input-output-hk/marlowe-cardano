{-# LANGUAGE NumericUnderscores #-}

module Language.Marlowe.Runtime.App.Submit (
  submit,
  submit',
  waitForTx',
) where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Functor (($>))
import Language.Marlowe.Runtime.App.Run (runChainSeekClient, runJobClient)
import Language.Marlowe.Runtime.App.Types (Client, Services (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainSyncCommand (SubmitTx),
  Move (FindTx),
  Transaction,
  TxId,
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Transaction.Api (MarloweTxCommand (Submit))
import Network.Protocol.ChainSeek.Client (
  ChainSeekClient (ChainSeekClient),
  ClientStIdle (..),
  ClientStNext (..),
  ClientStPoll (..),
 )
import Network.Protocol.Job.Client (liftCommand)

import qualified Cardano.Api as C (
  BabbageEra,
  ScriptDataSupportedInEra (ScriptDataInBabbageEra),
  Tx,
  getTxBody,
  getTxId,
 )
import Cardano.Api.Shelley (ReferenceTxInsScriptsInlineDatumsSupportedInEra)
import qualified Network.Protocol.Job.Client as J (
  ClientStAwait (..),
  ClientStCmd (..),
  ClientStInit (..),
  JobClient (..),
 )

submit
  :: Int
  -> ReferenceTxInsScriptsInlineDatumsSupportedInEra era
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

{-# DEPRECATED submit' "Crashes the chain-sync worker!" #-}
submit'
  :: C.Tx C.BabbageEra
  -> Client (Either String TxId)
submit' tx =
  fmap (second . const . fromCardanoTxId . C.getTxId $ C.getTxBody tx)
    . runJobClient runChainSeekCommandClient
    . liftCommand
    $ SubmitTx C.ScriptDataInBabbageEra tx

{-# DEPRECATED waitForTx' "Crashes the chain-sync worker!" #-}
waitForTx'
  :: Int
  -> TxId
  -> Client (Either String Transaction)
waitForTx' pollingFrequency txId =
  let clientIdle = SendMsgQueryNext (FindTx txId True) clientNext
      clientNext =
        ClientStNext
          { recvMsgQueryRejected = \err _ ->
              pure $ SendMsgDone $ Left $ "Chain sync rejected query: " <> show err <> "."
          , recvMsgWait = liftIO (threadDelay $ pollingFrequency * 1_000_000) $> SendMsgPoll clientNext
          , recvMsgRollBackward = \_ _ -> pure clientIdle
          , recvMsgRollForward = \tx point _ -> case point of
              Genesis -> pure $ SendMsgDone $ Left "Chain sync rolled forward to genesis."
              At _ -> pure $ SendMsgDone $ Right tx
          }
   in runChainSeekClient runChainSeekSyncClient
        . ChainSeekClient
        $ pure clientIdle
