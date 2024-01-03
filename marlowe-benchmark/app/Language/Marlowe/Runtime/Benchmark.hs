module Language.Marlowe.Runtime.Benchmark (
  measure,
) where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)

import qualified Data.Aeson as A (encode)
import qualified Data.ByteString.Lazy.Char8 as LBS8 (putStrLn)
import qualified Language.Marlowe.Runtime.Benchmark.HeaderSync as HeaderSync (measure)

headerSyncParallelism :: Int
headerSyncParallelism = 10

measure :: MarloweT IO ()
measure =
  do
    (results, _) <- HeaderSync.measure headerSyncParallelism
    liftIO . forM_ results $ LBS8.putStrLn . A.encode
