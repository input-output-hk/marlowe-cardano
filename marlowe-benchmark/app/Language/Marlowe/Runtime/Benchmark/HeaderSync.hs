{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Runtime.Benchmark.HeaderSync (
  Benchmark (..),
  measure,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Control.Monad.Trans.Marlowe.Class
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.Default (Default (..))
import Data.Time.Clock
import Data.Time.Clock.POSIX
import GHC.Generics (Generic)
import Language.Marlowe.Protocol.HeaderSync.Client (
  ClientStIdle (..),
  ClientStNext (..),
  ClientStWait (..),
  MarloweHeaderSyncClient (MarloweHeaderSyncClient),
 )
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (contractId)
import UnliftIO (replicateConcurrently)

import qualified Data.Set as S (Set, fromList, size)

data Benchmark = Benchmark
  { metric :: String
  , blocksPerSecond :: Double
  , contractsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

data Statistics = Statistics
  { blocks :: Integer
  , contracts :: S.Set ContractId
  , duration :: NominalDiffTime
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

instance Default Statistics where
  def = Statistics def mempty 0

measure
  :: Int
  -> MarloweT IO ([Benchmark], S.Set ContractId)
measure count =
  second head . unzip
    <$> replicateConcurrently
      count
      (run "HeaderSync")

run
  :: String
  -> MarloweT IO (Benchmark, S.Set ContractId)
run metric =
  do
    Statistics{..} <- runMarloweHeaderSyncClient . benchmark =<< liftIO getPOSIXTime
    let seconds = realToFrac duration
        blocksPerSecond = fromInteger blocks / seconds
        contractsPerSecond = fromIntegral (S.size contracts) / seconds
    pure (Benchmark{..}, contracts)

benchmark
  :: (MonadIO m)
  => NominalDiffTime
  -> MarloweHeaderSyncClient m Statistics
benchmark start =
  let clientIdle = SendMsgRequestNext . clientNext
      clientWait = pure . SendMsgCancel . SendMsgDone
      clientNext stats@Statistics{..} =
        ClientStNext
          { recvMsgNewHeaders = \_blockHeader results ->
              do
                now <- liftIO getPOSIXTime
                pure $
                  clientIdle $
                    stats
                      { blocks = blocks + 1
                      , contracts = contracts <> S.fromList (contractId <$> results)
                      , duration = now - start
                      }
          , recvMsgRollBackward = \_chainPoint ->
              pure $ clientIdle stats
          , recvMsgWait = clientWait stats
          }
   in MarloweHeaderSyncClient . pure $ clientIdle def
