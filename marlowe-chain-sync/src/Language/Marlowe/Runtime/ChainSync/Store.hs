module Language.Marlowe.Runtime.ChainSync.Store
  ( Changes(..)
  , ChainStoreDependencies(..)
  , ChainStore(..)
  , mkChainStore
  ) where

import Cardano.Api (ChainPoint (..), ChainTip (..), SlotNo (..))
import Cardano.Api.Shelley (Hash (..))
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.Delay (Delay, newDelay, waitDelay)
import Control.Monad (guard, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.ByteString.Base16 (encodeBase16)
import Data.ByteString.Short (fromShort)
import Data.Foldable (for_, traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, diffUTCTime, getCurrentTime, nominalDiffTimeToSeconds)
import Language.Marlowe.Runtime.ChainSync.Database (CommitBlocks (..), CommitRollback (..))
import Language.Marlowe.Runtime.ChainSync.NodeClient (Changes (..), isEmptyChanges)
import Prelude hiding (filter)
import System.IO (stderr)
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
          for_ changesRollback \point -> do
            case point of
              ChainPointAtGenesis -> T.hPutStrLn stderr "Rolling back to Genesis"
              ChainPoint (SlotNo slot) (HeaderHash hash) -> T.hPutStrLn stderr $ T.intercalate " "
                [ "Rolling back to block"
                , encodeBase16 $ fromShort hash
                , "at slot"
                , T.pack $ show slot
                ]
            runCommitRollback commitRollback point
          when (changesBlockCount > 0) do
            T.hPutStrLn stderr $ mconcat
              [ "Saving "
              , T.pack $ show changesBlockCount
              , " blocks, "
              , T.pack $ show changesTxCount
              , " transactions. New tip: "
              , case changesPoint of
                  ChainPointAtGenesis -> "Genesis"
                  ChainPoint (SlotNo slot) (HeaderHash hash) -> T.intercalate " "
                    [ "block"
                    , encodeBase16 $ fromShort hash
                    , "at slot"
                    , T.pack $ show slot
                    ]
              , " (node tip: "
              , case changesTip of
                  ChainTipAtGenesis           -> "Genesis"
                  ChainTip (SlotNo slot) _  _ -> T.pack $ show slot
              , ")"
              ]
            runCommitBlocks commitBlocks changesBlocks
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
