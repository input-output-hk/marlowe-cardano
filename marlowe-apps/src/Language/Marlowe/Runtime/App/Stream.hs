

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Stream
  where


import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Language.Marlowe.Runtime.App.Run (runMarloweHeaderSyncClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId))

import qualified Language.Marlowe.Protocol.HeaderSync.Client as Sync


streamAllContracts
  :: TChan ContractId
  -> Client (Either String ())
streamAllContracts = streamContractHeaders $ Just . contractId


streamContractHeaders
  :: (ContractHeader -> Maybe a)
  -> TChan a
  -> Client (Either String ())
streamContractHeaders extract channel =
  let
    clientIdle = Sync.SendMsgRequestNext clientNext
    clientWait = Sync.SendMsgPoll clientNext
    clientNext =
      Sync.ClientStNext
      {
        Sync.recvMsgNewHeaders = \_ results -> do
                                                 liftIO . atomically
                                                   . mapM_ (writeTChan channel)
                                                   $ mapMaybe extract results
                                                 pure clientIdle
      , Sync.recvMsgRollBackward = const $ pure clientIdle
      , Sync.recvMsgWait = clientWait <$ liftIO (threadDelay 5_000_000)
      }
  in
    runMarloweHeaderSyncClient runDiscoverySyncClient
      . Sync.MarloweHeaderSyncClient
      $ pure clientIdle


