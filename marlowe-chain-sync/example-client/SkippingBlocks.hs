module SkippingBlocks
  where

import Control.Concurrent (threadDelay)
import Data.Functor (($>))
import Data.Void (absurd)
import Language.Marlowe.Runtime.ChainSync.Api
import Network.Protocol.ChainSeek.Client

-- This client advances 1000 blocks at a time until it reaches the tip.
client :: RuntimeChainSeekClient IO ()
client = ChainSeekClient $ stIdle 1000
  where
    stIdle stepSize = do
      putStrLn $ "Advancing " <> show stepSize <> " block(s)"
      pure $ SendMsgQueryNext (AdvanceBlocks stepSize) stNext

    stNext = ClientStNext
      { recvMsgQueryRejected = absurd
      , recvMsgRollForward = const handleNewPoint
      , recvMsgRollBackward = handleNewPoint
      , recvMsgWait = threadDelay 1_000_000 $> SendMsgPoll stNext
      }

    handleNewPoint point tip = do
      putStrLn $ "New local tip: " <> show point
      let
        pointBlock = \case
          Genesis            -> -1
          At BlockHeader{..} -> blockNo
      let stepSize = max 1 $ min 1000 $ pointBlock tip - pointBlock point
      stIdle $ fromIntegral stepSize
