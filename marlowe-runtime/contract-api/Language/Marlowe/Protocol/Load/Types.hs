{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The Marlowe load protocol is a protocol for incrementally loading and
-- merkleizing large contracts in a space-efficient way.
module Language.Marlowe.Protocol.Load.Types where

import Control.Monad (join)
import Data.Aeson (encode, toJSON)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary (..), Get, getWord8, putWord8)
import qualified Data.List.NonEmpty as NE
import Data.String (fromString)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import GHC.Show (showSpace)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Language.Marlowe.Runtime.Core.Api ()
import Network.Protocol.Codec (BinaryMessage (..), ShowProtocol (..))
import Network.Protocol.Codec.Spec hiding (SomePeerHasAgency)
import qualified Network.Protocol.Codec.Spec as Codec
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Peer.Trace
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import OpenTelemetry.Attributes

-- | A kind-level datatype for the states in the MarloweLoad protocol.
data MarloweLoad where
  -- | In this state, the server is processing contract nodes pushed by the
  -- client.
  StProcessing :: Node -> MarloweLoad
  -- | In this state, the client can push N more nodes to the server before the
  -- server needs to process them.
  StCanPush :: N -> Node -> MarloweLoad
  -- | The terminal state
  StDone :: MarloweLoad
  -- | In this state, there are no more nodes to push, and the server is
  -- preparing to send the hash of the contract to the client.
  StComplete :: MarloweLoad

-- | A location in a marlowe contract.
data Node where
  -- | The root of the contract
  RootNode :: Node
  -- | A pay contract.
  PayNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | An if contract with an incomplete "then" clause.
  IfLNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | An if contract with an complete "then" clause.
  IfRNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | A when contract somewhere in a contract, with completed cases.
  WhenNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | A case in a when contract somewhere in a contract.
  CaseNode
    :: Node
    -- ^ The parent node of the when node above this case.
    -> Node
  -- | A let contract somewhere in a contract.
  LetNode
    :: Node
    -- ^ The parent node.
    -> Node
  -- | An assert contract somewhere in a contract.
  AssertNode
    :: Node
    -- ^ The parent node.
    -> Node

-- A singleton version of @@Node@@
data SNode (node :: Node) where
  SRootNode :: SNode 'RootNode
  SPayNode :: SNode node -> SNode ('PayNode node)
  SIfLNode :: SNode node -> SNode ('IfLNode node)
  SIfRNode :: SNode node -> SNode ('IfRNode node)
  SWhenNode :: SNode node -> SNode ('WhenNode node)
  SCaseNode :: SNode node -> SNode ('CaseNode node)
  SLetNode :: SNode node -> SNode ('LetNode node)
  SAssertNode :: SNode node -> SNode ('AssertNode node)

-- A type family that computes the next protocol state when popping
-- (completing) a node.
type family Pop (n :: N) (node :: Node) :: MarloweLoad where
  -- Popping the root node transitions to the complete state.
  Pop n 'RootNode = 'StComplete
  -- Popping a pay node pops its parent node.
  Pop n ('PayNode node) = Pop n node
  -- Popping an ifL node transitions to pushing to the else clause.
  Pop n ('IfLNode node) = 'StCanPush n ('IfRNode node)
  -- Popping an ifR node pops its parent node.
  Pop n ('IfRNode node) = Pop n node
  -- Popping a when node pops its parent node.
  Pop n ('WhenNode node) = Pop n node
  -- Popping a case node transitions to pushing to the when clause (pushing the
  -- timeout fallback case).
  Pop n ('CaseNode node) = 'StCanPush n ('WhenNode node)
  -- Popping a let node pops its parent node.
  Pop n ('LetNode node) = Pop n node
  -- Popping an assert node pops its parent node.
  Pop n ('AssertNode node) = Pop n node

instance HasSignature MarloweLoad where
  signature _ = "MarloweLoad"

instance Protocol MarloweLoad where
  data Message MarloweLoad st st' where
    -- \| The server tells the client to resume pushing.
    MsgResume
      :: Nat ('S n)
      -- \^ The number of pushes the client is allowed to perform.
      -> Message MarloweLoad ('StProcessing node) ('StCanPush ('S n) node)
    -- \| Push a close node to the stack. This closes the current stack and pops
    -- until the next incomplete location in the contract is reached.
    MsgPushClose
      :: Message
          MarloweLoad
          ('StCanPush ('S n) node)
          (Pop n node)
    -- \| Push a pay node to the stack.
    MsgPushPay
      :: AccountId
      -> Payee
      -> Token
      -> Value Observation
      -> Message
          MarloweLoad
          ('StCanPush ('S n) node)
          ('StCanPush n ('PayNode node))
    -- \| Push an if node to the stack.
    MsgPushIf
      :: Observation
      -> Message
          MarloweLoad
          ('StCanPush ('S n) node)
          ('StCanPush n ('IfLNode node))
    -- \| Push a when node to the stack
    MsgPushWhen
      :: Timeout
      -> Message
          MarloweLoad
          ('StCanPush ('S n) node)
          ('StCanPush n ('WhenNode node))
    -- \| Push a case node to the stack. Only available if currently on a when node.
    MsgPushCase
      :: Action
      -> Message
          MarloweLoad
          ('StCanPush ('S n) ('WhenNode node))
          ('StCanPush n ('CaseNode node))
    -- \| Push a let node to the stack.
    MsgPushLet
      :: ValueId
      -> Value Observation
      -> Message
          MarloweLoad
          ('StCanPush ('S n) node)
          ('StCanPush n ('LetNode node))
    -- \| Push an assert node to the stack.
    MsgPushAssert
      :: Observation
      -> Message
          MarloweLoad
          ('StCanPush ('S n) node)
          ('StCanPush n ('AssertNode node))
    -- \| Request to resume pushing.
    MsgRequestResume
      :: Message
          MarloweLoad
          ('StCanPush 'Z node)
          ('StProcessing node)
    -- \| Abort the load
    MsgAbort
      :: Message
          MarloweLoad
          ('StCanPush n node)
          'StDone
    -- \| The server sends the hash of the completed (merkleized) contract back
    -- to the client.
    MsgComplete
      :: DatumHash
      -> Message
          MarloweLoad
          'StComplete
          'StDone

  data ClientHasAgency st where
    -- \| The client has agency in the CanPush state.
    TokCanPush :: Nat n -> SNode node -> ClientHasAgency ('StCanPush n node)

  data ServerHasAgency st where
    -- \| The server has agency in the Processing state.
    TokProcessing :: SNode node -> ServerHasAgency ('StProcessing node)
    -- \| The server has agency in the Complete state.
    TokComplete :: ServerHasAgency 'StComplete

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokCanPush{} = \case {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

data SomePeerHasAgency (st :: k) = forall pr. SomePeerHasAgency (PeerHasAgency pr st)

-- | A term-level version of the @@Pop@@ type family using singleton types.
sPop :: Nat n -> SNode node -> SomePeerHasAgency (Pop n node)
sPop n = \case
  SRootNode -> SomePeerHasAgency $ ServerAgency TokComplete
  SPayNode node -> sPop n node
  SIfLNode node -> SomePeerHasAgency $ ClientAgency $ TokCanPush n $ SIfRNode node
  SIfRNode node -> sPop n node
  SWhenNode node -> sPop n node
  SCaseNode node -> SomePeerHasAgency $ ClientAgency $ TokCanPush n $ SWhenNode node
  SLetNode node -> sPop n node
  SAssertNode node -> sPop n node

instance BinaryMessage MarloweLoad where
  putMessage = \case
    ClientAgency TokCanPush{} -> \case
      MsgPushClose -> putWord8 0x00
      MsgPushPay payor payee token value -> do
        putWord8 0x01
        put payor
        put payee
        put token
        put value
      MsgPushIf cond -> do
        putWord8 0x02
        put cond
      MsgPushWhen timeout -> do
        putWord8 0x03
        put timeout
      MsgPushCase action -> do
        putWord8 0x04
        put action
      MsgPushLet valueId value -> do
        putWord8 0x05
        put valueId
        put value
      MsgPushAssert obs -> do
        putWord8 0x06
        put obs
      MsgRequestResume -> putWord8 0x07
      MsgAbort -> putWord8 0x08
    ServerAgency TokComplete -> \case
      MsgComplete hash -> put hash
    ServerAgency (TokProcessing _) -> \case
      MsgResume n -> put $ natToInt n

  getMessage = \case
    ClientAgency (TokCanPush n tok) -> do
      tag <- getWord8
      case tag of
        0x00 -> getPushMsg n $ \k -> k $ pure $ SomeMessage MsgPushClose
        0x01 -> getPushMsg n \k -> k do
          msg <- MsgPushPay <$> get <*> get <*> get <*> get
          pure $ SomeMessage msg
        0x02 -> getPushMsg n \k -> k do
          msg <- MsgPushIf <$> get
          pure $ SomeMessage msg
        0x03 -> getPushMsg n \k -> k do
          msg <- MsgPushWhen <$> get
          pure $ SomeMessage msg
        0x04 -> getPushMsg n \k -> k case tok of
          SWhenNode _ -> do
            msg <- MsgPushCase <$> get
            pure $ SomeMessage msg
          _ -> fail "Invalid protocol state for MsgPushCase"
        0x05 -> getPushMsg n \k -> k do
          msg <- MsgPushLet <$> get <*> get
          pure $ SomeMessage msg
        0x06 -> getPushMsg n \k -> k do
          msg <- MsgPushAssert <$> get
          pure $ SomeMessage msg
        0x07 -> case n of
          Zero -> pure $ SomeMessage MsgRequestResume
          _ -> fail "Must push"
        0x08 -> pure $ SomeMessage MsgAbort
        _ -> fail $ "Invalid message tag " <> show tag
    ServerAgency TokComplete -> SomeMessage . MsgComplete <$> get
    -- unsafeIntToNat is actually safe here - why? Because we immediately
    -- forget the type because of existential types. The correct type will
    -- be brought into scope when pattern matching on the pattern synonyms
    -- `Zero` or `Succ n`.
    ServerAgency (TokProcessing _) -> SomeMessage . MsgResume . unsafeIntToNat <$> get

getPushMsg
  :: Nat n -> (forall n' r. (Get (SomeMessage ('StCanPush ('S n') node)) -> r) -> r) -> Get (SomeMessage ('StCanPush n node))
getPushMsg Zero _ = fail "No pushes left"
getPushMsg Succ{} handle = handle id

instance OTelProtocol MarloweLoad where
  protocolName _ = "marlowe_load"
  messageAttributes _ = \case
    MsgPushClose ->
      MessageAttributes
        { messageType = "push_close"
        , messageParameters = []
        }
    MsgPushPay accountId payee token value ->
      MessageAttributes
        { messageType = "push_pay"
        , messageParameters = jsonToPrimitiveAttribute <$> [toJSON accountId, toJSON payee, toJSON token, toJSON value]
        }
    MsgPushIf obs ->
      MessageAttributes
        { messageType = "push_if"
        , messageParameters = [jsonToPrimitiveAttribute $ toJSON obs]
        }
    MsgPushWhen timeout ->
      MessageAttributes
        { messageType = "push_when"
        , messageParameters = [IntAttribute $ fromIntegral timeout]
        }
    MsgPushCase action ->
      MessageAttributes
        { messageType = "push_case"
        , messageParameters = [jsonToPrimitiveAttribute $ toJSON action]
        }
    MsgPushLet valueId value ->
      MessageAttributes
        { messageType = "push_let"
        , messageParameters = jsonToPrimitiveAttribute <$> [toJSON valueId, toJSON value]
        }
    MsgPushAssert obs ->
      MessageAttributes
        { messageType = "push_assert"
        , messageParameters = [jsonToPrimitiveAttribute $ toJSON obs]
        }
    MsgResume n ->
      MessageAttributes
        { messageType = "resume"
        , messageParameters = [IntAttribute $ fromIntegral $ natToInt n]
        }
    MsgComplete hash ->
      MessageAttributes
        { messageType = "complete"
        , messageParameters = [fromString $ show hash]
        }
    MsgRequestResume ->
      MessageAttributes
        { messageType = "request_resume"
        , messageParameters = []
        }
    MsgAbort ->
      MessageAttributes
        { messageType = "abort"
        , messageParameters = []
        }

jsonToPrimitiveAttribute :: Aeson.Value -> PrimitiveAttribute
jsonToPrimitiveAttribute = \case
  Aeson.String s -> TextAttribute s
  Aeson.Bool b -> BoolAttribute b
  Aeson.Number n -> DoubleAttribute $ realToFrac n
  v -> TextAttribute $ toStrict $ decodeUtf8 $ encode v

instance MessageEq MarloweLoad where
  messageEq (AnyMessageAndAgency _ msg) (AnyMessageAndAgency _ msg') = case msg of
    MsgPushClose -> case msg' of
      MsgPushClose -> True
      _ -> False
    MsgPushPay accountId payee token value -> case msg' of
      MsgPushPay accountId' payee' token' value' ->
        (accountId, payee, token, value) == (accountId', payee', token', value')
      _ -> False
    MsgPushIf obs -> case msg' of
      MsgPushIf obs' ->
        obs == obs'
      _ -> False
    MsgPushWhen timeout -> case msg' of
      MsgPushWhen timeout' ->
        timeout == timeout'
      _ -> False
    MsgPushCase action -> case msg' of
      MsgPushCase action' ->
        action == action'
      _ -> False
    MsgPushLet valueId value -> case msg' of
      MsgPushLet valueId' value' ->
        (valueId, value) == (valueId', value')
      _ -> False
    MsgPushAssert obs -> case msg' of
      MsgPushAssert obs' ->
        obs == obs'
      _ -> False
    MsgResume n -> case msg' of
      MsgResume n' ->
        natToInt n == natToInt n'
      _ -> False
    MsgComplete hash -> case msg' of
      MsgComplete hash' ->
        hash == hash'
      _ -> False
    MsgRequestResume -> case msg' of
      MsgRequestResume -> True
      _ -> False
    MsgAbort -> case msg' of
      MsgAbort -> True
      _ -> False

instance MessageVariations MarloweLoad where
  messageVariations = \case
    ClientAgency (TokCanPush Succ{} node) ->
      join $
        NE.fromList
          [ pure $ SomeMessage MsgPushClose
          , SomeMessage
              <$> (MsgPushPay <$> variations `varyAp` variations `varyAp` variations `varyAp` variations)
          , SomeMessage . MsgPushIf <$> variations
          , case node of
              SWhenNode _ ->
                join $
                  NE.fromList
                    [ SomeMessage . MsgPushCase <$> variations
                    , SomeMessage . MsgPushWhen <$> variations
                    ]
              _ -> SomeMessage . MsgPushWhen <$> variations
          , SomeMessage <$> (MsgPushLet <$> variations `varyAp` variations)
          , SomeMessage . MsgPushAssert <$> variations
          ]
    ClientAgency (TokCanPush Zero _) ->
      NE.fromList
        [ SomeMessage MsgRequestResume
        , SomeMessage MsgAbort
        ]
    ServerAgency (TokProcessing _) -> pure $ SomeMessage $ MsgResume $ Succ Zero
    ServerAgency TokComplete -> SomeMessage . MsgComplete <$> variations

  agencyVariations =
    NE.fromList
      [ Codec.SomePeerHasAgency $ ClientAgency $ TokCanPush Zero $ SWhenNode SRootNode
      , Codec.SomePeerHasAgency $ ClientAgency $ TokCanPush (Succ Zero) $ SWhenNode SRootNode
      , Codec.SomePeerHasAgency $ ServerAgency $ TokProcessing SRootNode
      , Codec.SomePeerHasAgency $ ServerAgency TokComplete
      ]

instance ShowProtocol MarloweLoad where
  showsPrecMessage p = \case
    ClientAgency (TokCanPush _ _) -> \case
      MsgPushClose -> showString "MsgPushClose"
      MsgRequestResume -> showString "MsgRequestResume"
      MsgAbort -> showString "MsgAbort"
      MsgPushPay accountId payee token value ->
        showParen
          (p >= 11)
          ( showString "MsgPushPay"
              . showSpace
              . showsPrec 11 accountId
              . showSpace
              . showsPrec 11 payee
              . showSpace
              . showsPrec 11 token
              . showSpace
              . showsPrec 11 value
          )
      MsgPushIf obs ->
        showParen
          (p >= 11)
          ( showString "MsgPushIf"
              . showSpace
              . showsPrec 11 obs
          )
      MsgPushWhen timeout ->
        showParen
          (p >= 11)
          ( showString "MsgPushWhen"
              . showSpace
              . showsPrec 11 timeout
          )
      MsgPushCase action ->
        showParen
          (p >= 11)
          ( showString "MsgPushCase"
              . showSpace
              . showsPrec 11 action
          )
      MsgPushLet valueId value ->
        showParen
          (p >= 11)
          ( showString "MsgPushLet"
              . showSpace
              . showsPrec 11 valueId
              . showSpace
              . showsPrec 11 value
          )
      MsgPushAssert obs ->
        showParen
          (p >= 11)
          ( showString "MsgPushAssert"
              . showSpace
              . showsPrec 11 obs
          )
    ServerAgency (TokProcessing _) -> \case
      MsgResume n ->
        showParen
          (p >= 11)
          ( showString "MsgResume"
              . showSpace
              . showParen
                True
                ( showString "unsafeIntToNat"
                    . showSpace
                    . showsPrec 11 (natToInt n)
                )
          )
    ServerAgency TokComplete -> \case
      MsgComplete hash ->
        showParen
          (p >= 11)
          ( showString "MsgComplete"
              . showSpace
              . showsPrec 11 hash
          )

  showsPrecServerHasAgency p = \case
    TokProcessing node ->
      showParen
        (p >= 11)
        ( showString "TokProcessing"
            . showSpace
            . showsPrecSNode p node
        )
    TokComplete -> showString "TokComplete"
  showsPrecClientHasAgency p = \case
    TokCanPush n node ->
      showParen
        (p >= 11)
        ( showString "TokCanPush"
            . showSpace
            . showParen
              True
              ( showString "unsafeIntToNat"
                  . showSpace
                  . showsPrec 11 (natToInt n)
              )
            . showSpace
            . showsPrecSNode p node
        )

showsPrecSNode :: Int -> SNode node -> ShowS
showsPrecSNode p = \case
  SRootNode -> showString "SRootNode"
  SPayNode node ->
    showParen (p >= 11) (showString "SPayNode" . showSpace . showsPrecSNode 11 node)
  SIfLNode node ->
    showParen (p >= 11) (showString "SIfLNode" . showSpace . showsPrecSNode 11 node)
  SIfRNode node ->
    showParen (p >= 11) (showString "SIfRNode" . showSpace . showsPrecSNode 11 node)
  SWhenNode node ->
    showParen (p >= 11) (showString "SWhenNode" . showSpace . showsPrecSNode 11 node)
  SCaseNode node ->
    showParen (p >= 11) (showString "SCaseNode" . showSpace . showsPrecSNode 11 node)
  SLetNode node ->
    showParen (p >= 11) (showString "SLetNode" . showSpace . showsPrecSNode 11 node)
  SAssertNode node ->
    showParen (p >= 11) (showString "SAssertNode" . showSpace . showsPrecSNode 11 node)
