{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Data.Functor ((<&>))
import Language.Marlowe.Runtime.ChainSync.Api (ChainPoint, Move, RuntimeChainSeekServer, WithGenesis (..))
import Language.Marlowe.Runtime.ChainSync.Database (GetTip (..), MoveClient (..), MoveResult (..))
import Network.Protocol.ChainSeek.Server
import Network.Protocol.Connection (ServerSource (..))
import UnliftIO (MonadUnliftIO)

data ChainSyncServerDependencies m = ChainSyncServerDependencies
  { moveClient :: MoveClient m
  , getTip :: GetTip m
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
