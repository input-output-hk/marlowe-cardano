{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.ChainSync.Server where

import Control.Monad.Trans.Class (MonadTrans (..))
import Data.Bifunctor (Bifunctor (..))
import qualified Data.List.NonEmpty as NE
import Language.Marlowe.Runtime.ChainSync.Api (
  ChainPoint,
  ChainSyncMove,
  RuntimeChainSeekServerT,
  WithGenesis (..),
 )
import Language.Marlowe.Runtime.ChainSync.Database (
  GetTip (..),
  MoveClient (..),
 )
import qualified Language.Marlowe.Runtime.ChainSync.Database as DB
import Network.Protocol.ChainSeek.Types
import Network.Protocol.Connection (ServerSource (..))
import Network.Protocol.Peer.Monad (await, yield)
import qualified Network.Protocol.Peer.Monad as PeerT
import Numeric.Natural (Natural)
import UnliftIO (MonadUnliftIO)

data ChainSyncServerDependencies m = ChainSyncServerDependencies
  { moveClient :: MoveClient m
  , scan :: DB.Scan m
  , getTip :: GetTip m
  , scanBatchSize :: Natural
  }

chainSyncServer
  :: forall m
   . (MonadUnliftIO m)
  => ChainSyncServerDependencies m
  -> ServerSource (RuntimeChainSeekServerT 'StIdle 'StDone) m ()
chainSyncServer ChainSyncServerDependencies{..} = ServerSource $ pure $ stIdle Genesis
  where
    stIdle :: ChainPoint -> RuntimeChainSeekServerT 'StIdle 'StDone m ()
    stIdle pos = await \case
      QueryNext move -> stNext pos move
      Scan move -> PeerT.do
        collect <- lift $ DB.runScan scan pos move
        stScan move pos collect
      Done -> pure ()

    stNext :: ChainPoint -> Move (t :: ChainSyncMove) -> RuntimeChainSeekServerT ('StNext t) 'StDone m ()
    stNext pos move = withSingTag (moveTag move) PeerT.do
      res <- lift (DB.runMoveClient moveClient pos move)
      case res of
        DB.RollForward result pos' tip -> PeerT.do
          yield $ RollForward result (At pos') tip
          stIdle $ At pos'
        DB.RollBack pos' tip -> PeerT.do
          yield $ RollBackward pos' tip
          stIdle pos'
        DB.Reject err tip -> PeerT.do
          yield $ RejectQuery err tip
          stIdle pos
        DB.Wait tip -> PeerT.do
          yield Wait
          stPoll move pos tip

    stPoll :: Move (t :: ChainSyncMove) -> ChainPoint -> ChainPoint -> RuntimeChainSeekServerT ('StPoll t) 'StDone m ()
    stPoll move pos tip = withSingTag (moveTag move) $ await \case
      Poll -> PeerT.do
        newTip <- lift $ runGetTip getTip
        if tip /= newTip
          then stNext pos move
          else PeerT.do
            yield Wait
            stPoll move pos tip
      Cancel -> stIdle pos

    stScan
      :: Move (t :: ChainSyncMove) -> ChainPoint -> DB.Collect t m -> RuntimeChainSeekServerT ('StScan t) 'StDone m ()
    stScan move pos collect = withSingTag (moveTag move) $ await \case
      Collect -> PeerT.do
        result <- lift $ DB.runCollect collect scanBatchSize
        case result of
          DB.NextBlocks results tip collect' -> PeerT.do
            yield $ Collected (first At <$> NE.toList results) tip
            stScan move (At $ fst $ NE.last results) collect'
          DB.CollectRollBack pos' tip -> PeerT.do
            yield $ CollectRollBackward pos' tip
            stIdle pos'
          DB.CollectReject err tip -> PeerT.do
            yield $ CollectFailed err tip
            stIdle pos
          DB.CollectWait tip -> PeerT.do
            yield $ CollectWait tip
            stPoll move pos tip
      CancelScan -> stIdle pos
