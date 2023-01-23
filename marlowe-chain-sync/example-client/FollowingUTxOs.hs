module FollowingUTxOs
  where

import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.ChainSync.Api
import Network.Protocol.ChainSeek.Client

-- This client follows a Genesis TxOut through transactions until it finds a
-- UTxO at index 0 of some tx.
client :: RuntimeChainSeekClient IO ()
client = ChainSeekClient stInit
  where
    stInit = pure $ SendMsgRequestHandshake moveSchema stHandshake

    stHandshake = ClientStHandshake
      { recvMsgHandshakeRejected = \supportedVersions -> do
          putStr "Schema version not supported by server. Supported versions: "
          print supportedVersions
      , recvMsgHandshakeConfirmed = stIdle []
      }

    stIdle hist = do
      let
        (txOut, hist') = fromMaybe
          (TxOutRef "0a3eecbba8c47b5d941a4a06b3661a664c7290c071212365bb0fbc2e8ccd4112" 0, [])
          $ uncons hist
      putStrLn $ "Waiting for UTxO to be consumed: " <> show txOut
      pure $ SendMsgQueryNext (FindConsumingTx txOut) (stNext txOut hist')

    stNext txOut hist = ClientStNext
      { recvMsgQueryRejected = \err _ -> case err of
          UTxONotFound -> do
            putStrLn $ "TxOut not found: " <> show txOut
            putStrLn "Exiting"
            pure $ SendMsgDone ()
          UTxOSpent txId -> do
            putStrLn $ "TxOut already spent: " <> show txOut
            putStrLn $ "Spent by tx: " <> show txId
            putStrLn "Exiting"
            pure $ SendMsgDone ()
      , recvMsgRollForward = \Transaction{..} _ _ -> do
          putStrLn $ "UTxO spent by transaction: " <> show txId
          stIdle $ TxOutRef txId 0 : hist
      , recvMsgRollBackward = \_ _ -> do
          putStr "Rolling back"
          stIdle hist
      , recvMsgWait = pure $ SendMsgCancel $ SendMsgDone ()
      }
