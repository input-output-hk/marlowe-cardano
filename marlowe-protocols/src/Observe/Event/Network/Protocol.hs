{-# LANGUAGE PolyKinds #-}

module Observe.Event.Network.Protocol
  where

import Data.Aeson (Value)
import Network.TypedProtocol (Message, PeerHasAgency)

class MessageToJSON ps where
  messageToJSON :: PeerHasAgency pr (st :: ps) -> Message ps st st' -> Value
