{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor ((<&>))
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, Move, RuntimeChainSeekServer, WithGenesis (..))
import Language.Marlowe.Runtime.ChainSync.Database (
  Collect (..),
  CollectResult (..),
  GetTip (..),
  MoveClient (..),
  MoveResult (..),
  Scan (..),
 )
import Network.Protocol.ChainSeek.Server hiding (Wait)
import Network.Protocol.Connection (ServerSource (..))
import Numeric.Natural (Natural)
import UnliftIO (MonadUnliftIO)

data ChainSyncServerDependencies m = ChainSyncServerDependencies
  { moveClient :: MoveClient m
  , scan :: Scan m
  , getTip :: GetTip m
  , scanBatchSize :: Natural
  }

chainSyncServer
  :: forall m
   . (MonadUnliftIO m)
  => ChainSyncServerDependencies m
  -> ServerSource RuntimeChainSeekServer m ()
chainSyncServer ChainSyncServerDependencies{..} = ServerSource $ pure server
  where
    server = ChainSeekServer $ pure $ stIdle Genesis

    stIdle :: ChainPoint -> ServerStIdle Move ChainPoint ChainPoint m ()
    stIdle pos =
      ServerStIdle
        { recvMsgQueryNext = stNext pos
        , recvMsgScan = \move -> stScan move pos <$> runScan scan pos move
        , recvMsgDone = pure ()
        }

    stNext :: ChainPoint -> Move err result -> m (ServerStNext Move err result ChainPoint ChainPoint m ())
    stNext pos move =
      runMoveClient moveClient pos move <&> \case
        RollForward result pos' tip -> SendMsgRollForward result (At pos') tip $ stIdle $ At pos'
        RollBack pos' tip -> SendMsgRollBackward pos' tip $ stIdle pos'
        Reject err tip -> SendMsgQueryRejected err tip $ stIdle pos
        Wait tip -> SendMsgWait $ stPoll move pos tip

    stPoll :: Move err result -> ChainPoint -> ChainPoint -> ServerStPoll Move err result ChainPoint ChainPoint m ()
    stPoll move pos tip =
      ServerStPoll
        { recvMsgPoll = do
            newTip <- runGetTip getTip
            if tip /= newTip
              then stNext pos move
              else pure $ SendMsgWait $ stPoll move pos tip
        , recvMsgCancel = pure $ stIdle pos
        }

    stScan
      :: Move err result -> ChainPoint -> Collect err result m -> ServerStScan Move err result ChainPoint ChainPoint m ()
    stScan move pos collect =
      ServerStScan
        { recvMsgCollect =
            runCollect collect scanBatchSize <&> \case
              NextBlocks results tip collect' ->
                SendMsgCollected (first At <$> NE.toList results) tip $
                  stScan move (At $ fst $ NE.last results) collect'
              CollectRollBack pos' tip ->
                SendMsgCollectRollBackward pos' tip $ stIdle pos'
              CollectReject err tip ->
                SendMsgCollectFailed err tip $ stIdle pos
              CollectWait tip ->
                SendMsgCollectWait tip $ stPoll move pos tip
        , recvMsgCancelScan = pure $ stIdle pos
        }
