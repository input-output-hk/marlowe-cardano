{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}

module Language.Marlowe.Runtime.Discovery.QueryServer
  where

import Control.Concurrent.Async (Concurrently(Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api (PolicyId)
import Language.Marlowe.Runtime.Discovery.Api
import Network.Protocol.Query.Server
import Network.Protocol.Query.Types
import Numeric.Natural (Natural)
import System.IO (hPutStrLn, stderr)

newtype RunQueryServer m = RunQueryServer (forall a. QueryServer DiscoveryQuery m a -> m a)

data DiscoveryQueryServerDependencies = DiscoveryQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , getHeaders :: IO [ContractHeader]
  , getHeadersByRoleTokenCurrency :: PolicyId -> IO [ContractHeader]
  , pageSize :: Natural
  }

newtype DiscoveryQueryServer = DiscoveryQueryServer
  { runDiscoveryQueryServer :: IO Void
  }

mkDiscoveryQueryServer :: DiscoveryQueryServerDependencies -> STM DiscoveryQueryServer
mkDiscoveryQueryServer DiscoveryQueryServerDependencies{..} = do
  let
    runDiscoveryQueryServer = do
      runQueryServer <- acceptRunQueryServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runDiscoveryQueryServer
  pure $ DiscoveryQueryServer { runDiscoveryQueryServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Query worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runQueryServer   :: RunQueryServer IO
  , getHeaders :: IO [ContractHeader]
  , getHeadersByRoleTokenCurrency :: PolicyId -> IO [ContractHeader]
  , pageSize :: Natural
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunQueryServer run = runQueryServer
  in
    pure Worker { runWorker = run server }

  where
    server :: QueryServer DiscoveryQuery IO ()
    server = queryServer querySchema $ pure $ ServerStIdle
      { recvMsgRequest = \case
          GetContractHeaders ->
            getContractHeadersServer pageSize getHeaders
          GetContractHeadersByRoleTokenCurrency policyId ->
            getContractHeadersByRoleTokenCurrencyServer policyId getHeadersByRoleTokenCurrency
      , recvMsgDone = pure ()
      }

getContractHeadersServer
  :: Natural
  -> IO [ContractHeader]
  -> IO (ServerStNext DiscoveryQuery 'CanReject () Void [ContractHeader] IO ())
getContractHeadersServer pageSize getHeaders = next <$> getHeaders
  where
  next
    :: [ContractHeader]
    -> ServerStNext DiscoveryQuery k () Void [ContractHeader] IO ()
  next headers = case splitAt (fromIntegral pageSize) headers of
    (page, []) -> lastPage page
    (page, headers') -> SendMsgNextPage page (Just ()) ServerStPage
      { recvMsgRequestDone = pure ()
      , recvMsgRequestNext = const $ pure $ next headers'
      }
  lastPage
    :: [ContractHeader]
    -> ServerStNext DiscoveryQuery k () Void [ContractHeader] IO ()
  lastPage page = SendMsgNextPage page Nothing ServerStPage
    { recvMsgRequestDone = pure ()
    , recvMsgRequestNext = const $ pure $ lastPage []
    }

getContractHeadersByRoleTokenCurrencyServer
  :: PolicyId
  -> (PolicyId -> IO [ContractHeader])
  -> IO (ServerStNext DiscoveryQuery 'CanReject Void Void [ContractHeader] IO ())
getContractHeadersByRoleTokenCurrencyServer policyId getHeadersByRoleTokenCurrency = do
  headers <- getHeadersByRoleTokenCurrency policyId
  pure $ SendMsgNextPage headers Nothing ServerStPage
    { recvMsgRequestDone = pure ()
    , recvMsgRequestNext = absurd
    }
