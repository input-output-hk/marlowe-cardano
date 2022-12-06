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
import GHC.Show (showSpace)
import Network.Protocol.Driver (MessageToJSON(..), ShowMessage(..))
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

class Query q => ShowQuery (q :: * -> * -> *) where
  showsPrecQuery :: Int -> q err result -> ShowS
  showsPrecErr :: Int -> Tag q err result -> err -> ShowS
  showsPrecResult :: Int -> Tag q err result -> result -> ShowS

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
  StNext :: err -> result -> StNextKind -> ChainSeek query point tip

  -- | The server has sent a ping to the client to determine if it is still
  -- connected and is waiting for a pong.
  StPing :: err -> result -> ChainSeek query point tip

  -- | The failed state of the protocol.
  StFault :: ChainSeek query point tip

  -- | The normal terminal state of the protocol.
  StDone :: ChainSeek query point tip

-- | Sub-states of 'StNext'.
data StNextKind
  = StCanAwait  -- ^ The server can send a 'Wait' message or a response
  | StMustReply -- ^ The server must send a reply, having sent a 'Wait'.

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
      'StFault

    -- | Request the next matching result for the given query from the client's
    -- position.
    MsgQueryNext :: query err result -> Message (ChainSeek query point tip)
      'StIdle
      ('StNext err result 'StCanAwait)

    -- | Reject a query with an error message.
    MsgRejectQuery :: err -> tip -> Message (ChainSeek query point tip)
      ('StNext err result wait)
      'StIdle

    -- | Send a response to a query and roll the client forward to a new point.
    MsgRollForward :: result -> point -> tip -> Message (ChainSeek query point tip)
      ('StNext err result wait)
      'StIdle

    -- | Roll the client backward.
    MsgRollBackward :: point -> tip -> Message (ChainSeek query point tip)
      ('StNext err result wait)
      'StIdle

    -- | Inform the client they must wait indefinitely to receive a reply.
    MsgWait :: Message (ChainSeek query point tip)
      ('StNext err result 'StCanAwait)
      ('StNext err result 'StMustReply)

    -- | End the protocol
    MsgDone :: Message (ChainSeek query point tip)
      'StIdle
      'StDone

    -- | Check if the client is still waiting.
    MsgPing :: Message (ChainSeek query point tip)
      ('StNext err result 'StMustReply)
      ('StPing err result)

    -- | Reassure the server we are still waiting.
    MsgPong :: Message (ChainSeek query point tip)
      ('StPing err result)
      ('StNext err result 'StMustReply)

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokIdle :: ClientHasAgency 'StIdle
    TokPing :: ClientHasAgency ('StPing err result)

  data ServerHasAgency st where
    TokHandshake :: ServerHasAgency 'StHandshake
    TokNext :: Tag query err result -> TokNextKind k -> ServerHasAgency ('StNext err result k :: ChainSeek query point tip)

  data NobodyHasAgency st where
    TokFault :: NobodyHasAgency 'StFault
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit = \case
  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case
  exclusionLemma_ClientAndServerHaveAgency TokPing = \case

  exclusionLemma_NobodyAndClientHaveAgency TokFault = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone  = \case

  exclusionLemma_NobodyAndServerHaveAgency TokFault = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone  = \case

data TokNextKind (k :: StNextKind) where
  TokCanAwait :: TokNextKind 'StCanAwait
  TokMustReply :: TokNextKind 'StMustReply

instance
  ( ShowQuery query
  , Show tip
  , Show point
  ) => ShowMessage (ChainSeek query point tip) where
  showsPrecMessage p = \case
    ClientAgency TokInit -> \case
      MsgRequestHandshake version -> showParen (p >= 11)
        ( showString "MsgRequestHandshake"
        . showSpace
        . showsPrec 11 version
        )
    ClientAgency TokIdle -> \case
      MsgQueryNext query -> showParen (p >= 11)
        ( showString "MsgQueryNext"
        . showSpace
        . showsPrecQuery 11 query
        )
      MsgDone -> showString "MsgDone"
    ClientAgency TokPing -> \case
      MsgPong -> showString "MsgPong"
    ServerAgency TokHandshake -> \case
      MsgConfirmHandshake -> showString "MsgConfirmHandshake"
      MsgRejectHandshake versions -> showParen (p >= 11)
        ( showString "MsgRejectHandshake"
        . showSpace
        . showsPrec 11 versions
        )
    ServerAgency (TokNext tag _) -> \case
      MsgRejectQuery err tip -> showParen (p >= 11)
        ( showString "MsgRejectQuery"
        . showSpace
        . showsPrecErr 11 tag err
        . showSpace
        . showsPrec 11 tip
        )
      MsgRollForward result point tip -> showParen (p >= 11)
        ( showString "MsgRollForward"
        . showSpace
        . showsPrecResult 11 tag result
        . showSpace
        . showsPrec 11 point
        . showSpace
        . showsPrec 11 tip
        )
      MsgRollBackward point tip -> showParen (p >= 11)
        ( showString "MsgRollBackward"
        . showSpace
        . showsPrec 11 point
        . showSpace
        . showsPrec 11 tip
        )
      MsgWait -> showString "MsgWait"
      MsgPing -> showString "MsgPing"

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
    ClientAgency TokPing -> \case
      MsgPong -> String "pong"
    ServerAgency TokHandshake -> \case
      MsgConfirmHandshake -> String "confirmHandshake"
      MsgRejectHandshake versions -> object [ "rejectHandshake" .= toJSON versions ]
    ServerAgency (TokNext tag _) -> \case
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
      MsgPing -> String "ping"
