{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE MonoLocalBinds #-}
module Control.Distributed.Protocol where

import Control.Distributed.Process (Process, SendPort, receiveChan, sendChan)
import Control.Distributed.Process.Internal.Types (ReceivePort)
import Control.Distributed.Process.Serializable (Serializable)
import Data.Foldable (traverse_)

newtype ProtocolClient req res = ProtocolClient (res -> Process (Maybe (req, ProtocolClient req res)))

newtype ProtocolServer req res = ProtocolServer (req -> Process (res, Maybe (ProtocolServer req res)))

runClient :: (Serializable req, Serializable res) => res -> ReceivePort res -> SendPort req -> ProtocolClient req res -> Process ()
runClient res receiveResponse sendRequest (ProtocolClient client) =
  client res >>= traverse_ \(req, next) -> do
    sendChan sendRequest req
    res' <- receiveChan receiveResponse
    runClient res' receiveResponse sendRequest next

runServer :: (Serializable req, Serializable res) => ReceivePort req -> SendPort res -> ProtocolServer req res -> Process ()
runServer receiveRequest sendResponse (ProtocolServer server) = do
  req <- receiveChan receiveRequest
  (res, next) <- server req
  sendChan sendResponse res
  traverse_ (runServer receiveRequest sendResponse) next
