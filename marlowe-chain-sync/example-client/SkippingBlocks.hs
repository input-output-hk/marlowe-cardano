module SkippingBlocks where

import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Network.Protocol.ChainSeek.Client

-- This client advances 1000 blocks at a time until it reaches the tip.
client :: RuntimeChainSeekClient IO ()
client = ChainSeekClient stInit
  where
    stInit = pure $ SendMsgRequestHandshake schemaVersion1_0 stHandshake

    stHandshake = ClientStHandshake
      { recvMsgHandshakeRejected = \supportedVersions -> do
          putStr "Schema version not supported by server. Supported versions: "
          print supportedVersions
      , recvMsgHandshakeConfirmed = stIdle 1000
      }

    stIdle stepSize = do
      putStrLn $ "Advancing " <> show stepSize <> " block(s)"
      pure $ SendMsgQueryNext (AdvanceBlocks stepSize) stNext $ pure stNext

    stNext = ClientStNext
      { recvMsgQueryRejected = absurd
      , recvMsgRollForward = const handleNewPoint
      , recvMsgRollBackward = handleNewPoint
      }

    handleNewPoint point tip = do
      putStrLn $ "New local tip: " <> show point
      let
        pointBlock = \case
          Genesis            -> -1
          At BlockHeader{..} -> blockNo
      let stepSize = max 1 $ min 1000 $ pointBlock tip - pointBlock point
      stIdle $ fromIntegral stepSize
