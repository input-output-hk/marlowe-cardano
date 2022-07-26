module Language.Marlowe.Runtime.ChainSync.Store
  ( Changes(..)
  , ChainStoreDependencies(..)
  , ChainStore(..)
  , mkChainStore
  ) where

import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Monad (guard)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Foldable (traverse_)
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitRollback (..))
import Language.Marlowe.Runtime.ChainSync.NodeClient (Changes (..), isEmptyChanges)
import Prelude hiding (filter)
import Witherable (Witherable (..))

data ChainStoreDependencies = ChainStoreDependencies
  { commitRollback :: !(CommitRollback IO)
  , commitBlocks   :: !(CommitBlocks IO)
  , rateLimit      :: !NominalDiffTime
  , getChanges     :: !(STM Changes)
  }

newtype ChainStore = ChainStore
  { runChainStore :: IO ()
  }

mkChainStore :: ChainStoreDependencies -> STM ChainStore
mkChainStore ChainStoreDependencies{..} = do
  let
    awaitChanges :: Maybe Delay -> STM Changes
    awaitChanges delay = do
      traverse_ waitDelay delay
      changes <- getChanges
      guard $ not $ isEmptyChanges changes
      pure changes

    runChainStore :: IO ()
    runChainStore = go Nothing
      where
        go lastWrite = do
          delay <- wither computeDelay lastWrite
          Changes{..} <- atomically $ awaitChanges delay
          traverse_ (runCommitRollback commitRollback) changesRollback
          runCommitBlocks commitBlocks changesBlocks changesPoint changesTip
          go . Just =<< getCurrentTime

    computeDelay :: UTCTime -> IO (Maybe Delay)
    computeDelay lastWrite = runMaybeT do
      currentTime <- lift getCurrentTime
      let nextWrite = addUTCTime rateLimit lastWrite
      guard $ nextWrite > currentTime
      let delay = nextWrite `diffUTCTime` currentTime
      let delayMicroseconds = floor $ 1_000_000 * nominalDiffTimeToSeconds delay
      lift $ newDelay delayMicroseconds

  pure $ ChainStore { runChainStore }
