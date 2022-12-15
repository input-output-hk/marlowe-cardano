{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The type of the chain seek protocol.

module Network.Protocol.ChainSeek.Types
  where

import Data.Aeson (ToJSON(..), Value(..), object, (.=))
import Data.Binary (Binary(..), Get, Put)
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Data.Type.Equality (type (:~:))
import Network.Protocol.Driver (MessageToJSON(..))
import Network.TypedProtocol (PeerHasAgency(..), Protocol(..))

data SomeTag q = forall err result. SomeTag (Tag q err result)

class Query (q :: * -> * -> *) where
  data Tag q :: * -> * -> *
  tagFromQuery :: q err result -> Tag q err result
  tagEq :: Tag q err result -> Tag q err' result' -> Maybe (err :~: err', result :~: result')
  putTag :: Tag q err result -> Put
  getTag :: Get (SomeTag q)
  putQuery :: q err result -> Put
  getQuery :: Tag q err result -> Get (q err result)
  putErr :: Tag q err result -> err -> Put
  getErr :: Tag q err result -> Get err
  putResult :: Tag q err result -> result -> Put
  getResult :: Tag q err result -> Get result

class Query q => QueryToJSON (q :: * -> * -> *) where
  queryToJSON :: q err result -> Value
  errToJSON :: Tag q err result -> err -> Value
  resultToJSON :: Tag q err result -> result -> Value

-- | The type of states in the protocol.
data ChainSeek (query :: Type -> Type -> Type) point tip where
  -- | The server is waiting for the client to initiate the handshake.
  StInit :: ChainSeek query point tip

  -- | The client is waiting for the server to accept the handshake.
  StHandshake :: ChainSeek query point tip

  -- | The client and server are idle. The client can send a request.
  StIdle :: ChainSeek query point tip

  -- | The client has sent a next update request. The client is now waiting for
  -- a response, and the server is preparing to send a response. The server can
  -- respond immediately or it can send a 'Wait' message followed by a response
  -- at some point in the future.
  StNext :: err -> result -> ChainSeek query point tip

  -- | The server has sent a ping to the client to determine if it is still
  -- connected and is waiting for a pong.
  StPoll :: err -> result -> ChainSeek query point tip

  -- | The terminal state of the protocol.
  StDone :: ChainSeek query point tip

-- | Schema version used for
newtype SchemaVersion = SchemaVersion Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString, ToJSON)

instance Binary SchemaVersion where
  put (SchemaVersion v) = put $ T.encodeUtf8 v
  get = do
    bytes <- get
    case T.decodeUtf8' bytes of
      Left err      -> fail $ show err
      Right version -> pure $ SchemaVersion version

instance Protocol (ChainSeek query point tip) where

  -- | The type of messages in the protocol. Corresponds to the state
  -- transition in the state machine diagram.
  data Message (ChainSeek query point tip) from to where

    -- | Initiate a handshake for the given schema version.
    MsgRequestHandshake :: SchemaVersion -> Message (ChainSeek query point tip)
      'StInit
      'StHandshake

    -- | Accept the handshake.
    MsgConfirmHandshake :: Message (ChainSeek query point tip)
      'StHandshake
      'StIdle

    -- | Reject the handshake.
    MsgRejectHandshake :: [SchemaVersion] -> Message (ChainSeek query point tip)
      'StHandshake
      'StDone

    -- | Request the next matching result for the given query from the client's
    -- position.
    MsgQueryNext :: query err result -> Message (ChainSeek query point tip)
      'StIdle
      ('StNext err result)

    -- | Reject a query with an error message.
    MsgRejectQuery :: err -> tip -> Message (ChainSeek query point tip)
      ('StNext err result)
      'StIdle

    -- | Send a response to a query and roll the client forward to a new point.
    MsgRollForward :: result -> point -> tip -> Message (ChainSeek query point tip)
      ('StNext err result)
      'StIdle

    -- | Roll the client backward.
    MsgRollBackward :: point -> tip -> Message (ChainSeek query point tip)
      ('StNext err result)
      'StIdle

    -- | Inform the client they must wait indefinitely to receive a reply.
    MsgWait :: Message (ChainSeek query point tip)
      ('StNext err result)
      ('StPoll err result)

    -- | End the protocol
    MsgDone :: Message (ChainSeek query point tip)
      'StIdle
      'StDone

    -- | Ask the server if there have been any updates.
    MsgPoll :: Message (ChainSeek query point tip)
      ('StPoll err result)
      ('StNext err result)

    -- | Cancel the polling loop.
    MsgCancel :: Message (ChainSeek query point tip)
      ('StPoll err result)
      'StIdle

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokIdle :: ClientHasAgency 'StIdle
    TokPoll :: ClientHasAgency ('StPoll err result)

  data ServerHasAgency st where
    TokHandshake :: ServerHasAgency 'StHandshake
    TokNext :: Tag query err result -> ServerHasAgency ('StNext err result :: ChainSeek query point tip)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit = \case
  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case
  exclusionLemma_ClientAndServerHaveAgency TokPoll = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone  = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone  = \case

instance
  ( QueryToJSON query
  , ToJSON tip
  , ToJSON point
  ) => MessageToJSON (ChainSeek query point tip) where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgRequestHandshake version -> object [ "requestHandshake" .= toJSON version ]
    ClientAgency TokIdle -> \case
      MsgQueryNext query -> object [ "queryNext" .= queryToJSON query ]
      MsgDone -> String "done"
    ClientAgency TokPoll -> \case
      MsgPoll -> String "poll"
      MsgCancel -> String "cancel"
    ServerAgency TokHandshake -> \case
      MsgConfirmHandshake -> String "confirmHandshake"
      MsgRejectHandshake versions -> object [ "rejectHandshake" .= toJSON versions ]
    ServerAgency (TokNext tag) -> \case
      MsgRejectQuery err tip -> object
        [ "rejectQuery" .= object
            [ "error" .= errToJSON tag err
            , "tip" .= toJSON tip
            ]
        ]
      MsgRollForward result point tip -> object
        [ "rollForward" .= object
            [ "result" .= resultToJSON tag result
            , "point" .= toJSON point
            , "tip" .= toJSON tip
            ]
        ]
      MsgRollBackward point tip -> object
        [ "rollBackward" .= object
            [ "point" .= toJSON point
            , "tip" .= toJSON tip
            ]
        ]
      MsgWait -> String "wait"
