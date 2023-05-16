{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A server machine type for loading contracts incrementally.

module Language.Marlowe.Protocol.Load.Server
  where

import Cardano.Api (hashScriptData)
import Data.Kind (Type)
import Data.Type.Equality (type (:~:)(..))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Network.Protocol.Peer.Trace hiding (Close)
import Network.TypedProtocol
import Plutus.V2.Ledger.Api (toBuiltin)

-- A server of the MarloweLoad protocol.
newtype MarloweLoadServer m a = MarloweLoadServer
  { runMarloweLoadServer :: m (ServerStProcessing 'RootNode m a)
  }

-- A server in the Processing state.
data ServerStProcessing (node :: Node) m a where
  -- | Instruct the client to resume pushing contract nodes.
  SendMsgResume
    :: Nat ('S n) -- ^ The number of nodes the client is allowed to push.
    -> ServerStCanPush ('S n) node m a -- The next server to handle the next client push.
    -> ServerStProcessing node m a

-- A type family that computes the next server state when popping a node.
-- Corresponds to the @@Pop@@ type family.
type family ServerStPop (n :: N) (node :: Node) :: (Type -> Type) -> Type -> Type where
  ServerStPop n 'RootNode = ServerStComplete
  ServerStPop n ('PayNode node) = ServerStPop n node
  ServerStPop n ('IfLNode node) = ServerStCanPush n ('IfRNode node)
  ServerStPop n ('IfRNode node) = ServerStPop n node
  ServerStPop n ('WhenNode node) = ServerStPop n node
  ServerStPop n ('CaseNode node) = ServerStCanPush n ('WhenNode node)
  ServerStPop n ('LetNode node) = ServerStPop n node
  ServerStPop n ('AssertNode node) = ServerStPop n node

-- A server in the CanPush state.
data ServerStCanPush (n :: N) (node :: Node) m a where
  ServerStCanPush :: ServerStCanPushSucc n node m a -> ServerStCanPush ('S n) node m a
  ServerStPaused :: m (ServerStProcessing node m a) -> ServerStCanPush 'Z node m a

-- A server in the CanPush state.
data ServerStCanPushSucc (n :: N) (node :: Node) m a = ServerStCanPushSucc
  { recvClose :: m (ServerStPop n node m a)
    -- ^ Receive a close node, popping the current stack to the next incomplete
    -- contract location.
  , recvPay :: AccountId -> Payee -> Token -> Value Observation -> m (ServerStCanPush n ('PayNode node) m a)
    -- ^ Receive a pay node, then start receiving the sub-contract.
  , recvIf :: Observation -> m (ServerStCanPush n ('IfLNode node) m a)
    -- ^ Receive an if node, then start receiving the then clause.
  , recvWhen :: Timeout -> m (ServerStCanPush n ('WhenNode node) m a)
    -- ^ Receive a when node, then start receiving either the cases or the
    -- timeout continuation.
  , recvCase :: forall st'. node :~: 'WhenNode st' -> Action -> m (ServerStCanPush n ('CaseNode st') m a)
    -- ^ Receive a case node (will only be called when currently on a when node).
  , recvLet :: ValueId -> Value Observation -> m (ServerStCanPush n ('LetNode node) m a)
    -- ^ Receive a let node, then start receiving the sub-contract.
  , recvAssert :: Observation -> m (ServerStCanPush n ('AssertNode node) m a)
    -- ^ Receive an assert node, then start receiving the sub-contract.
  }

-- A server in the Complete state.
data ServerStComplete m a where
  -- | Send the hash of the merkleized root contract to the client.
  SendMsgComplete :: DatumHash -> m a -> ServerStComplete m a

-- ^ Interpret a client as a traced typed-protocols peer.
marloweLoadServerPeer
  :: forall m a
   . Functor m
  => MarloweLoadServer m a
  -> PeerTraced MarloweLoad 'AsServer ('StProcessing 'RootNode) m a
marloweLoadServerPeer = EffectTraced . fmap (peerProcessing SRootNode) . runMarloweLoadServer
  where
  peerProcessing :: SNode node -> ServerStProcessing node m a -> PeerTraced MarloweLoad 'AsServer ('StProcessing node) m a
  peerProcessing node processing = case responseProcessing node processing of
    Response msg peer -> YieldTraced (ServerAgency $ TokProcessing node) msg $ Cast peer

  responseProcessing :: SNode node -> ServerStProcessing node m a -> Response MarloweLoad 'AsServer ('StProcessing node) m a
  responseProcessing node = \case
    SendMsgResume n next ->
      Response (MsgResume n) $ peerCanPush n node next

  peerCanPush
    :: Nat n
    -> SNode node
    -> ServerStCanPush n node m a
    -> PeerTraced MarloweLoad 'AsServer ('StCanPush n node) m a
  peerCanPush Zero node (ServerStPaused recvRequestResume) = AwaitTraced (ClientAgency $ TokCanPush Zero node) \case
    MsgRequestResume -> Respond (ServerAgency $ TokProcessing node) $ responseProcessing node <$> recvRequestResume
  peerCanPush (Succ n) node (ServerStCanPush ServerStCanPushSucc{..}) = AwaitTraced (ClientAgency $ TokCanPush (Succ n) node) $ Receive . EffectTraced . \case
    MsgPushClose -> peerPop n node <$> recvClose
    MsgPushPay payor payee token value ->
      peerCanPush n (SPayNode node) <$> recvPay payor payee token value
    MsgPushIf cond ->
      peerCanPush n (SIfLNode node) <$> recvIf cond
    MsgPushWhen timeout ->
      peerCanPush n (SWhenNode node) <$> recvWhen timeout
    MsgPushCase action -> case node of
      SWhenNode st' -> peerCanPush n (SCaseNode st') <$> recvCase Refl action
    MsgPushLet valueId value ->
      peerCanPush n (SLetNode node) <$> recvLet valueId value
    MsgPushAssert obs ->
      peerCanPush n (SAssertNode node) <$> recvAssert obs

  peerPop
    :: Nat n
    -> SNode node
    -> ServerStPop n node m a
    -> PeerTraced MarloweLoad 'AsServer (Pop n node) m a
  peerPop n node client = case node of
    SRootNode -> peerComplete client
    SPayNode node' -> peerPop n node' client
    SIfLNode node' -> peerCanPush n (SIfRNode node') client
    SIfRNode node' -> peerPop n node' client
    SWhenNode node' -> peerPop n node' client
    SCaseNode node' -> peerCanPush n (SWhenNode node') client
    SLetNode node' -> peerPop n node' client
    SAssertNode node' -> peerPop n node' client

  peerComplete :: ServerStComplete m a -> PeerTraced MarloweLoad 'AsServer 'StComplete m a
  peerComplete (SendMsgComplete hash next) = YieldTraced (ServerAgency TokComplete) (MsgComplete hash)
    $ Cast
    $ EffectTraced
    $ DoneTraced TokDone <$> next

-- | A generic implementation of a MarloweLoad server with a configurable batch
-- size and storage mechanism.
pullContract
  :: forall m batchSize
   . Applicative m
  => Nat ('S batchSize) -- ^ The maximum number of contracts to accept before processing.
  -> (Contract -> (DatumHash, m ())) -- ^ A callback that computes the hash of a contract and returns an action to await its saving.
  -> MarloweLoadServer m Contract
pullContract batchSize save =
  MarloweLoadServer $ pure $ SendMsgResume batchSize $ pull (pure ()) batchSize StateRoot
  where
    pull
      :: m ()
      -> Nat n
      -> PeerState node
      -> ServerStCanPush n node m Contract
    pull awaitSaves Zero state = ServerStPaused do
      awaitSaves
      pure $ SendMsgResume batchSize $ pull (pure ()) batchSize state
    pull awaitSaves (Succ n) state = ServerStCanPush ServerStCanPushSucc
      { recvClose = popState awaitSaves n state Close
      , recvPay = \payor payee token value ->
          pure $ pull awaitSaves n (StatePay payor payee token value state)
      , recvIf = \cond ->
          pure $ pull awaitSaves n (StateIfL cond state)
      , recvWhen = \timeout ->
          pure $ pull awaitSaves n (StateWhen timeout [] state)
      , recvCase = \Refl action -> case state of
          StateWhen timeout cases st' ->
            pure $ pull awaitSaves n (StateCase action timeout cases st')
      , recvLet = \valueId value ->
          pure $ pull awaitSaves n (StateLet valueId value state)
      , recvAssert = \obs ->
          pure $ pull awaitSaves n (StateAssert obs state)
      }

    popState :: m () -> Nat n -> PeerState node -> Contract -> m (ServerStPop n node m Contract)
    popState awaitSaves n state contract = case state of
      StateRoot -> case contract of
        Close -> pure $ SendMsgComplete closeHash $ pure contract
        _ -> do
          let (hash, awaitSave) = save contract
          awaitSaves
          awaitSave
          pure $ SendMsgComplete hash $ pure contract
      StatePay payor payee token value st' ->
        popState awaitSaves n st' (Pay payor payee token value contract)
      StateIfL cond st' ->
        pure $ pull awaitSaves n (StateIfR cond contract st')
      StateIfR cond tru st' ->
        popState awaitSaves n st' (If cond tru contract)
      StateWhen timeout cases st' ->
        popState awaitSaves n st' (When (reverse cases) timeout contract)
      StateCase action timeout cases st' -> pure case contract of
        Close -> pull
          awaitSaves
          n
          (StateWhen timeout (Case action Close : cases) st')
        _ ->
          let (hash, awaitSave) = save contract
          in
            pull
              (awaitSaves *> awaitSave)
              n
              (StateWhen timeout (MerkleizedCase action (toBuiltin $ unDatumHash hash) : cases) st')
      StateLet valueId value st' ->
        popState awaitSaves n st' (Let valueId value contract)
      StateAssert obs st' ->
        popState awaitSaves n st' (Assert obs contract)

closeHash :: DatumHash
closeHash = fromCardanoDatumHash $ hashScriptData $ toCardanoScriptData $ toDatum Close

data PeerState (node :: Node) where
  StateRoot :: PeerState 'RootNode
  StatePay :: AccountId -> Payee -> Token -> Value Observation -> PeerState node -> PeerState ('PayNode node)
  StateIfL :: Observation -> PeerState node -> PeerState ('IfLNode node)
  StateIfR :: Observation -> Contract -> PeerState node -> PeerState ('IfRNode node)
  StateWhen :: Timeout -> [Case Contract] -> PeerState node -> PeerState ('WhenNode node)
  StateCase :: Action -> Timeout -> [Case Contract] -> PeerState node -> PeerState ('CaseNode node)
  StateLet :: ValueId -> Value Observation -> PeerState node -> PeerState ('LetNode node)
  StateAssert :: Observation -> PeerState node -> PeerState ('AssertNode node)
