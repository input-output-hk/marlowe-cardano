

module Language.Marlowe.Runtime.Client.Submit
  ( submit
  , waitForTx
  ) where


import Data.Bifunctor (second)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api
  (ChainSyncCommand(SubmitTx), Move(FindTx), TxId, WithGenesis(..), moveSchema)
import Language.Marlowe.Runtime.Client.Run (runChainSeekClient, runJobClient)
import Language.Marlowe.Runtime.Client.Types (Client, Services(..))
import Network.Protocol.ChainSeek.Client
  (ChainSeekClient(ChainSeekClient), ClientStHandshake(..), ClientStIdle(..), ClientStInit(..), ClientStNext(..))
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
  :: TxId
  -> Client (Either String TxId)
waitForTx txId =
  let
    clientInit = SendMsgRequestHandshake moveSchema ClientStHandshake
      { recvMsgHandshakeRejected = \_ ->
          pure $ Left "Chain seek schema version mismatch."
      , recvMsgHandshakeConfirmed = pure clientIdle
      }
    clientIdle = SendMsgQueryNext (FindTx txId True) clientNext (pure clientNext)
    clientNext = ClientStNext
      { recvMsgQueryRejected = \err _ ->
          pure $ SendMsgDone $ Left $ "Chain seek rejected query: " <> show err <> "."
      , recvMsgRollBackward = \_ _ -> pure clientIdle
      , recvMsgRollForward = \_ point _ -> case point of
          Genesis -> pure $ SendMsgDone $ Left  "Chain seek rolled forward to genesis."
          At _    -> pure $ SendMsgDone $ Right txId
      }
  in
    runChainSeekClient runSyncClient
      . ChainSeekClient
      $ pure clientInit
