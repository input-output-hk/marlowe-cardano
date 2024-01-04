module Language.Marlowe.Runtime.Benchmark (
  measure,
) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)

import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)
import qualified Language.Marlowe.Runtime.Benchmark.HeaderSync as HeaderSync (measure)
import qualified Language.Marlowe.Runtime.Benchmark.Sync as Sync (measure)

measure
  :: Int
  -- ^ Number of parallel workers for `HeaderSync` query.
  -> Int
  -- ^ Maximum number of contracts to read with each `HeaderSync` benchmark.
  -> Int
  -- ^ Number of parallel works for `Sync` query.
  -> Int
  -- ^ Number of contracts for each `Sync` worker to query.
  -> MarloweT IO ()
measure headerSyncParallelism maxContracts syncParallelism syncBatchSize =
  do
    (headerSyncResults, contractIds) <- HeaderSync.measure headerSyncParallelism maxContracts
    liftIO . forM_ headerSyncResults $ LBS8.putStrLn . A.encode
    syncResults <- Sync.measure syncParallelism syncBatchSize contractIds
    liftIO . forM_ syncResults $ LBS8.putStrLn . A.encode
