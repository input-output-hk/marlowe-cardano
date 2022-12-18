
{-# LANGUAGE NumericUnderscores #-}


module Language.Marlowe.Runtime.Client.Submit
  ( submit
  , waitForTx
  ) where


import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Functor (($>))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncCommand(SubmitTx), Move(FindTx), Transaction, TxId, WithGenesis(..), moveSchema)
import Language.Marlowe.Runtime.Client.Run (runChainSeekClient, runJobClient)
import Language.Marlowe.Runtime.Client.Types (Client, Services(..))
import Network.Protocol.ChainSeek.Client
  ( ChainSeekClient(ChainSeekClient)
  , ClientStHandshake(..)
  , ClientStIdle(..)
  , ClientStInit(..)
  , ClientStNext(..)
  , ClientStPoll(..)
  )
import Network.Protocol.Job.Client (liftCommand)

import qualified Cardano.Api as C (BabbageEra, ScriptDataSupportedInEra(ScriptDataInBabbageEra), Tx, getTxBody, getTxId)


submit
  :: C.Tx C.BabbageEra
  -> Client (Either String TxId)
submit tx =
  fmap (second . const . fromCardanoTxId . C.getTxId $ C.getTxBody tx)
    . runJobClient runSyncCommandClient
    . liftCommand
    $ SubmitTx C.ScriptDataInBabbageEra tx


waitForTx
  :: Int
  -> TxId
  -> Client (Either String Transaction)
waitForTx pollingFrequency txId =
  let
    clientInit = SendMsgRequestHandshake moveSchema ClientStHandshake
      { recvMsgHandshakeRejected = \_ ->
          pure $ Left "Chain seek schema version mismatch."
      , recvMsgHandshakeConfirmed = pure clientIdle
      }
    clientIdle = SendMsgQueryNext (FindTx txId True) clientNext
    clientNext = ClientStNext
      { recvMsgQueryRejected = \err _ ->
          pure $ SendMsgDone $ Left $ "Chain seek rejected query: " <> show err <> "."
      , recvMsgWait = liftIO (threadDelay $ pollingFrequency * 1_000_000) $> SendMsgPoll clientNext
      , recvMsgRollBackward = \_ _ -> pure clientIdle
      , recvMsgRollForward = \tx point _ -> case point of
          Genesis -> pure $ SendMsgDone $ Left  "Chain seek rolled forward to genesis."

          At _    -> pure $ SendMsgDone $ Right tx
      }
  in
    runChainSeekClient runSyncClient
      . ChainSeekClient
      $ pure clientInit
