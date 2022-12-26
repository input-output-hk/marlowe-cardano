

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}


module Language.Marlowe.Runtime.App.Stream
  where


import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.App.Run (runQueryClient)
import Language.Marlowe.Runtime.App.Types (Client, Services(..))
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(contractId), DiscoveryQuery(..))
import System.IO (hPutStrLn, stderr)

import qualified Network.Protocol.Query.Client as Query
  ( ClientStInit(SendMsgRequest)
  , ClientStNext(ClientStNext)
  , ClientStNextCanReject(..)
  , ClientStPage(..)
  , QueryClient(QueryClient)
  )


streamAllContracts :: TChan ContractId -> Client ()
streamAllContracts =
  streamQuery GetContractHeaders runDiscoveryQueryClient True
    $ Just . contractId


streamQuery
  :: query () Void [result]
  -> (Services IO -> Query.QueryClient query IO () -> IO ())
  -> Bool
  -> (result -> Maybe a)
  -> TChan a
  -> Client ()
streamQuery query run follow extract channel =
  let
    handleNextPage results nextPage =
      do
        liftIO . atomically
          . mapM_ (writeTChan channel)
          $ mapMaybe extract results
        case nextPage of
          Nothing -> if follow
                       then do
                              liftIO $ hPutStrLn stderr "WAITING" >> threadDelay 10000000 >> hPutStrLn stderr "REQUESTING"
                              pure $ () `Query.SendMsgRequestNext` Query.ClientStNext handleNextPage
                       else pure $ Query.SendMsgDone ()
          Just () -> pure $ () `Query.SendMsgRequestNext` Query.ClientStNext handleNextPage
  in
    runQueryClient run
      . Query.QueryClient
      . pure
      $ Query.SendMsgRequest query Query.ClientStNextCanReject
        { Query.recvMsgReject = absurd
        , Query.recvMsgNextPage = handleNextPage
        }
