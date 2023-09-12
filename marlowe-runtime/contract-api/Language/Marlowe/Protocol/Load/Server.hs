{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A server machine type for loading contracts incrementally.
module Language.Marlowe.Protocol.Load.Server where

import Data.Kind (Type)
import Data.Type.Equality (type (:~:) (..))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (..))
import Language.Marlowe.Util (dataHash)
import Network.Protocol.Peer.Trace hiding (Close)
import Network.TypedProtocol
import PlutusLedgerApi.V2 (fromBuiltin, toBuiltin)

-- A server of the MarloweLoad protocol.
newtype MarloweLoadServer m a = MarloweLoadServer
  { runMarloweLoadServer :: m (ServerStProcessing 'RootNode m a)
  }

instance (Functor m) => Functor (MarloweLoadServer m) where
  fmap :: forall a b. (a -> b) -> MarloweLoadServer m a -> MarloweLoadServer m b
  fmap f = MarloweLoadServer . fmap (mapProcessing SRootNode) . runMarloweLoadServer
    where
      mapProcessing :: SNode node -> ServerStProcessing node m a -> ServerStProcessing node m b
      mapProcessing node = \case
        SendMsgResume n canPush -> SendMsgResume n $ mapCanPush n node canPush

      mapCanPush :: Nat n -> SNode node -> ServerStCanPush n node m a -> ServerStCanPush n node m b
      mapCanPush Zero node (ServerStPaused ServerStCanPushZero{..}) =
        ServerStPaused
          ServerStCanPushZero
            { recvMsgRequestResume = mapProcessing node <$> recvMsgRequestResume
            , recvMsgAbort = f <$> recvMsgAbort
            }
      mapCanPush (Succ n) node (ServerStCanPush ServerStCanPushSucc{..}) =
        ServerStCanPush
          ServerStCanPushSucc
            { recvClose = fmap (mapPop n node) recvClose
            , recvPay = (fmap . fmap . fmap) (fmap (mapCanPush n (SPayNode node))) . recvPay
            , recvIf = fmap (mapCanPush n (SIfLNode node)) . recvIf
            , recvWhen = fmap (mapCanPush n (SWhenNode node)) . recvWhen
            , recvCase = \case
                Refl -> case node of
                  SWhenNode node' -> fmap (mapCanPush n (SCaseNode node')) . recvCase Refl
            , recvLet = fmap (fmap (mapCanPush n (SLetNode node))) . recvLet
            , recvAssert = fmap (mapCanPush n (SAssertNode node)) . recvAssert
            , recvMsgAbort = f <$> recvMsgAbort
            }

      mapPop :: Nat n -> SNode node -> ServerStPop n node m a -> ServerStPop n node m b
      mapPop n node = case node of
        SRootNode -> mapComplete
        SPayNode node' -> mapPop n node'
        SIfLNode node' -> mapCanPush n (SIfRNode node')
        SIfRNode node' -> mapPop n node'
        SWhenNode node' -> mapPop n node'
        SCaseNode node' -> mapCanPush n (SWhenNode node')
        SLetNode node' -> mapPop n node'
        SAssertNode node' -> mapPop n node'

      mapComplete :: ServerStComplete m a -> ServerStComplete m b
      mapComplete = \case
        SendMsgComplete hash a -> SendMsgComplete hash $ f <$> a

-- A server in the Processing state.
data ServerStProcessing (node :: Node) m a where
  -- | Instruct the client to resume pushing contract nodes.
  SendMsgResume
    :: Nat ('S n)
    -- ^ The number of nodes the client is allowed to push.
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
  ServerStPaused :: ServerStCanPushZero node m a -> ServerStCanPush 'Z node m a

-- A server in the CanPush state when the client has no pushes left.
data ServerStCanPushZero (node :: Node) m a = ServerStCanPushZero
  { recvMsgRequestResume :: m (ServerStProcessing node m a)
  , recvMsgAbort :: m a
  }

-- A server in the CanPush state where the client has pushes left.
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
  , recvMsgAbort :: m a
  }

-- A server in the Complete state.
data ServerStComplete m a where
  -- | Send the hash of the merkleized root contract to the client.
  SendMsgComplete :: DatumHash -> m a -> ServerStComplete m a

hoistMarloweLoadServer
  :: forall m n a
   . (Functor m)
  => (forall x. m x -> n x)
  -> MarloweLoadServer m a
  -> MarloweLoadServer n a
hoistMarloweLoadServer f = MarloweLoadServer . f . fmap (hoistProcessing SRootNode) . runMarloweLoadServer
  where
    hoistProcessing :: SNode node -> ServerStProcessing node m a -> ServerStProcessing node n a
    hoistProcessing node = \case
      SendMsgResume n canPush -> SendMsgResume n $ hoistCanPush node n canPush

    hoistCanPush :: SNode node -> Nat i -> ServerStCanPush i node m a -> ServerStCanPush i node n a
    hoistCanPush node Zero (ServerStPaused ServerStCanPushZero{..}) =
      ServerStPaused
        ServerStCanPushZero
          { recvMsgRequestResume = f $ hoistProcessing node <$> recvMsgRequestResume
          , recvMsgAbort = f recvMsgAbort
          }
    hoistCanPush node (Succ n) (ServerStCanPush ServerStCanPushSucc{..}) =
      ServerStCanPush
        ServerStCanPushSucc
          { recvClose = f $ fmap (hoistPop node n) recvClose
          , recvPay = (fmap . fmap . fmap) (f . fmap (hoistCanPush (SPayNode node) n)) . recvPay
          , recvIf = f . fmap (hoistCanPush (SIfLNode node) n) . recvIf
          , recvWhen = f . fmap (hoistCanPush (SWhenNode node) n) . recvWhen
          , recvCase = \case
              Refl -> case node of
                SWhenNode node' -> f . fmap (hoistCanPush (SCaseNode node') n) . recvCase Refl
          , recvLet = fmap (f . fmap (hoistCanPush (SLetNode node) n)) . recvLet
          , recvAssert = f . fmap (hoistCanPush (SAssertNode node) n) . recvAssert
          , recvMsgAbort = f recvMsgAbort
          }

    hoistPop :: SNode node -> Nat i -> ServerStPop i node m a -> ServerStPop i node n a
    hoistPop node n = case node of
      SRootNode -> hoistComplete
      SPayNode node' -> hoistPop node' n
      SIfLNode node' -> hoistCanPush (SIfRNode node') n
      SIfRNode node' -> hoistPop node' n
      SWhenNode node' -> hoistPop node' n
      SCaseNode node' -> hoistCanPush (SWhenNode node') n
      SLetNode node' -> hoistPop node' n
      SAssertNode node' -> hoistPop node' n

    hoistComplete :: ServerStComplete m a -> ServerStComplete n a
    hoistComplete = \case
      SendMsgComplete hash a -> SendMsgComplete hash $ f a
-- ^ Interpret a client as a traced typed-protocols peer.

marloweLoadServerPeer
  :: forall m a
   . (Functor m)
  => MarloweLoadServer m a
  -> PeerTraced MarloweLoad 'AsServer ('StProcessing 'RootNode) m a
marloweLoadServerPeer = EffectTraced . fmap (peerProcessing SRootNode) . runMarloweLoadServer
  where
    peerProcessing :: SNode node -> ServerStProcessing node m a -> PeerTraced MarloweLoad 'AsServer ('StProcessing node) m a
    peerProcessing node processing = case responseProcessing node processing of
      Response msg peer -> YieldTraced (ServerAgency $ TokProcessing node) msg $ Cast peer

    responseProcessing
      :: SNode node -> ServerStProcessing node m a -> Response MarloweLoad 'AsServer ('StProcessing node) m a
    responseProcessing node = \case
      SendMsgResume n next ->
        Response (MsgResume n) $ peerCanPush n node next

    peerCanPush
      :: Nat n
      -> SNode node
      -> ServerStCanPush n node m a
      -> PeerTraced MarloweLoad 'AsServer ('StCanPush n node) m a
    peerCanPush Zero node (ServerStPaused ServerStCanPushZero{..}) = AwaitTraced (ClientAgency $ TokCanPush Zero node) \case
      MsgAbort -> Closed TokDone recvMsgAbort
      MsgRequestResume -> Respond (ServerAgency $ TokProcessing node) $ responseProcessing node <$> recvMsgRequestResume
    peerCanPush (Succ n) node (ServerStCanPush ServerStCanPushSucc{..}) = AwaitTraced (ClientAgency $ TokCanPush (Succ n) node) $ \case
      MsgAbort -> Closed TokDone recvMsgAbort
      MsgPushClose -> peerPop n node recvClose
      MsgPushPay payor payee token value ->
        Receive $ EffectTraced $ peerCanPush n (SPayNode node) <$> recvPay payor payee token value
      MsgPushIf cond ->
        Receive $ EffectTraced $ peerCanPush n (SIfLNode node) <$> recvIf cond
      MsgPushWhen timeout ->
        Receive $ EffectTraced $ peerCanPush n (SWhenNode node) <$> recvWhen timeout
      MsgPushCase action -> case node of
        SWhenNode st' -> Receive $ EffectTraced $ peerCanPush n (SCaseNode st') <$> recvCase Refl action
      MsgPushLet valueId value ->
        Receive $ EffectTraced $ peerCanPush n (SLetNode node) <$> recvLet valueId value
      MsgPushAssert obs ->
        Receive $ EffectTraced $ peerCanPush n (SAssertNode node) <$> recvAssert obs

    peerPop
      :: Nat n
      -> SNode node
      -> m (ServerStPop n node m a)
      -> AwaitTraced MarloweLoad 'AsServer (Pop n node) m a
    peerPop n node client = case node of
      SRootNode -> Respond (ServerAgency TokComplete) $ peerComplete <$> client
      SPayNode node' -> peerPop n node' client
      SIfLNode node' -> Receive $ EffectTraced $ peerCanPush n (SIfRNode node') <$> client
      SIfRNode node' -> peerPop n node' client
      SWhenNode node' -> peerPop n node' client
      SCaseNode node' -> Receive $ EffectTraced $ peerCanPush n (SWhenNode node') <$> client
      SLetNode node' -> peerPop n node' client
      SAssertNode node' -> peerPop n node' client

    peerComplete :: ServerStComplete m a -> Response MarloweLoad 'AsServer 'StComplete m a
    peerComplete (SendMsgComplete hash next) =
      Response (MsgComplete hash) $
        EffectTraced $
          DoneTraced TokDone <$> next

-- | A generic implementation of a MarloweLoad server with a configurable batch
-- size and storage mechanism.
pullContract
  :: forall m batchSize
   . (Monad m)
  => Nat ('S batchSize)
  -- ^ The maximum number of contracts to accept before processing.
  -> (Contract -> m DatumHash)
  -- ^ A callback that computes the hash of a contract and stages it for saving.
  -> m ()
  -- ^ An action that flushes queued contracts.
  -> m ()
  -- ^ An action to perform upon completion
  -> MarloweLoadServer m (Maybe Contract)
pullContract batchSize stageContract flush complete =
  MarloweLoadServer $ pure $ SendMsgResume batchSize $ pull batchSize StateRoot
  where
    pull
      :: Nat n
      -> PeerState node
      -> ServerStCanPush n node m (Maybe Contract)
    pull Zero state =
      ServerStPaused
        ServerStCanPushZero
          { recvMsgAbort = pure Nothing
          , recvMsgRequestResume = do
              flush
              pure $ SendMsgResume batchSize $ pull batchSize state
          }
    pull (Succ n) state =
      ServerStCanPush
        ServerStCanPushSucc
          { recvClose = popState n state Close
          , recvPay = \payor payee token value ->
              pure $ pull n (StatePay payor payee token value state)
          , recvIf = \cond ->
              pure $ pull n (StateIfL cond state)
          , recvWhen = \timeout ->
              pure $ pull n (StateWhen timeout [] state)
          , recvCase = \Refl action -> case state of
              StateWhen timeout cases st' ->
                pure $ pull n (StateCase action timeout cases st')
          , recvLet = \valueId value ->
              pure $ pull n (StateLet valueId value state)
          , recvAssert = \obs ->
              pure $ pull n (StateAssert obs state)
          , recvMsgAbort = pure Nothing
          }

    popState :: Nat n -> PeerState node -> Contract -> m (ServerStPop n node m (Maybe Contract))
    popState n state contract = case state of
      StateRoot -> do
        hash <- case contract of
          Close -> pure closeHash
          _ -> do
            hash <- stageContract contract
            flush
            pure hash
        complete
        pure $ SendMsgComplete hash $ pure $ Just contract
      StatePay payor payee token value st' ->
        popState n st' (Pay payor payee token value contract)
      StateIfL cond st' ->
        pure $ pull n (StateIfR cond contract st')
      StateIfR cond tru st' ->
        popState n st' (If cond tru contract)
      StateWhen timeout cases st' ->
        popState n st' (When (reverse cases) timeout contract)
      StateCase action timeout cases st' -> case contract of
        Close ->
          pure $
            pull
              n
              (StateWhen timeout (Case action Close : cases) st')
        _ -> do
          hash <- stageContract contract
          pure $
            pull
              n
              (StateWhen timeout (MerkleizedCase action (toBuiltin $ unDatumHash hash) : cases) st')
      StateLet valueId value st' ->
        popState n st' (Let valueId value contract)
      StateAssert obs st' ->
        popState n st' (Assert obs contract)

closeHash :: DatumHash
closeHash = DatumHash $ fromBuiltin $ dataHash Close

data PeerState (node :: Node) where
  StateRoot :: PeerState 'RootNode
  StatePay :: AccountId -> Payee -> Token -> Value Observation -> PeerState node -> PeerState ('PayNode node)
  StateIfL :: Observation -> PeerState node -> PeerState ('IfLNode node)
  StateIfR :: Observation -> Contract -> PeerState node -> PeerState ('IfRNode node)
  StateWhen :: Timeout -> [Case Contract] -> PeerState node -> PeerState ('WhenNode node)
  StateCase :: Action -> Timeout -> [Case Contract] -> PeerState node -> PeerState ('CaseNode node)
  StateLet :: ValueId -> Value Observation -> PeerState node -> PeerState ('LetNode node)
  StateAssert :: Observation -> PeerState node -> PeerState ('AssertNode node)
