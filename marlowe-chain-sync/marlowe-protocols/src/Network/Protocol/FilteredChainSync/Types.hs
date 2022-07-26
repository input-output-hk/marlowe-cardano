{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE EmptyCase    #-}
{-# LANGUAGE GADTs        #-}
{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the filtered chain sync protocol.

module Network.Protocol.FilteredChainSync.Types where

import Data.Binary (Binary (..))
import Data.Kind (Type)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import Network.TypedProtocol (Protocol (..))

-- | The type of states in the protocol.
data FilteredChainSync (query :: Type -> Type -> Type) point tip where
  -- | The server is waiting for the client to initiate the handshake.
  StInit :: FilteredChainSync query point tip

  -- | The client is waiting for the server to accept the handshake.
  StHandshake :: FilteredChainSync query point tip

  -- | The client and server are idle. The client can send a request.
  StIdle :: FilteredChainSync query point tip

  -- | The client has sent a next update request. The client is now waiting for
  -- a response, and the server is preparing to send a response. The server can
  -- respond immediately or it can send a 'Wait' message followed by a response
  -- at some point in the future.
  StNext :: err -> result -> StNextKind -> FilteredChainSync query point tip

  -- | The failed state of the protocol.
  StFault :: FilteredChainSync query point tip

  -- | The normal terminal state of the protocol.
  StDone :: FilteredChainSync query point tip

-- | Sub-states of 'StNext'.
data StNextKind
  = StCanAwait  -- ^ The server can send a 'Wait' message or a response
  | StMustReply -- ^ The server must send a reply, having sent a 'Wait'.

-- | Schema version used for
newtype SchemaVersion = SchemaVersion Text
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

instance Binary SchemaVersion where
  put (SchemaVersion v) = put $ T.encodeUtf8 v
  get = do
    bytes <- get
    case T.decodeUtf8' bytes of
      Left err      -> fail $ show err
      Right version -> pure $ SchemaVersion version

instance Protocol (FilteredChainSync query point tip) where

  -- | The type of messages in the protocol. Corresponds to the state
  -- transition in the state machine diagram.
  data Message (FilteredChainSync query point tip) from to where

    -- | Initiate a handshake for the given schema version.
    MsgRequestHandshake :: SchemaVersion -> Message (FilteredChainSync query point tip)
      'StInit
      'StHandshake

    -- | Accept the handshake.
    MsgConfirmHandshake :: Message (FilteredChainSync query point tip)
      'StHandshake
      'StIdle

    -- | Reject the handshake.
    MsgRejectHandshake :: [SchemaVersion] -> Message (FilteredChainSync query point tip)
      'StHandshake
      'StFault

    -- | Request the next matching result for the given query from the client's
    -- position.
    MsgQueryNext :: point -> query err result -> Message (FilteredChainSync query point tip)
      'StIdle
      ('StNext err result 'StCanAwait)

    -- | Reject a query with an error message.
    MsgRejectQuery :: query err result -> err -> tip -> Message (FilteredChainSync query point tip)
      ('StNext err result wait)
      'StIdle

    -- | Send a response to a query and roll the client forward to a new point.
    MsgRollForward :: query err result -> result -> point -> tip -> Message (FilteredChainSync query point tip)
      ('StNext err result wait)
      'StIdle

    -- | Roll the client backward.
    MsgRollBackward :: point -> tip -> Message (FilteredChainSync query point tip)
      ('StNext err result wait)
      'StIdle

    -- | Inform the client they must wait indefinitely to receive a reply.
    MsgWait :: Message (FilteredChainSync query point tip)
      ('StNext err result 'StCanAwait)
      ('StNext err result 'StMustReply)

    -- | End the protocol
    MsgDone :: Message (FilteredChainSync query point tip)
      'StIdle
      'StDone

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokIdle :: ClientHasAgency 'StIdle

  data ServerHasAgency st where
    TokHandshake :: ServerHasAgency 'StHandshake
    TokNext :: TokNextKind k -> ServerHasAgency ('StNext err result k)

  data NobodyHasAgency st where
    TokFault :: NobodyHasAgency 'StFault
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit = \case
  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case

  exclusionLemma_NobodyAndClientHaveAgency TokFault = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone  = \case

  exclusionLemma_NobodyAndServerHaveAgency TokFault = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone  = \case

data TokNextKind (k :: StNextKind) where
  TokCanAwait :: TokNextKind 'StCanAwait
  TokMustReply :: TokNextKind 'StMustReply
