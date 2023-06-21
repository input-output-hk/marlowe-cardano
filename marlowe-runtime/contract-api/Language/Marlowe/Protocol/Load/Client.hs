{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A client machine type for loading contracts incrementally.

module Language.Marlowe.Protocol.Load.Client where

import Control.Monad (join)
import Data.Kind (Type)
import Data.Type.Equality (type (:~:)(Refl))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Server
  ( MarloweLoadServer(..)
  , ServerStCanPush(..)
  , ServerStCanPushSucc(..)
  , ServerStCanPushZero(..)
  , ServerStComplete(..)
  , ServerStPop
  , ServerStProcessing(..)
  )
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.Protocol.Peer.Trace hiding (Close)
import qualified Network.Protocol.Peer.Trace as Peer
import Network.TypedProtocol

-- A client of the MarloweLoad protocol.
newtype MarloweLoadClient m a = MarloweLoadClient
  { runMarloweLoadClient :: m (ClientStProcessing 'RootNode m a)
  }

instance Functor m => Functor (MarloweLoadClient m) where
  fmap :: forall a b. (a -> b) -> MarloweLoadClient m a -> MarloweLoadClient m b
  fmap f = MarloweLoadClient . fmap (mapProcessing SRootNode) . runMarloweLoadClient
    where
      mapProcessing :: SNode node -> ClientStProcessing node m a -> ClientStProcessing node m b
      mapProcessing node ClientStProcessing{..} = ClientStProcessing
        { recvMsgResume = \n -> mapCanPush n node <$> recvMsgResume n
        }

      mapCanPush :: Nat n -> SNode node -> ClientStCanPush n node m a -> ClientStCanPush n node m b
      mapCanPush Zero node = \case
        Abort a -> Abort $ f a
        RequestResume processing -> RequestResume $ mapProcessing node processing
      mapCanPush (Succ n) node = \case
        Abort a -> Abort $ f a
        PushClose next -> PushClose $ mapPop n node <$> next
        PushPay payor payee token value next -> PushPay payor payee token value $ mapCanPush n (SPayNode node) <$> next
        PushIf cond next -> PushIf cond $ mapCanPush n (SIfLNode node) <$> next
        PushWhen timeout next -> PushWhen timeout $ mapCanPush n (SWhenNode node) <$> next
        PushCase action next -> PushCase action case node of
          SWhenNode node' -> mapCanPush n (SCaseNode node') <$> next
        PushLet valueId value next -> PushLet valueId value $ mapCanPush n (SLetNode node) <$> next
        PushAssert obs next -> PushAssert obs $ mapCanPush n (SAssertNode node) <$> next

      mapPop :: Nat n -> SNode node -> ClientStPop n node m a -> ClientStPop n node m b
      mapPop n node = case node of
        SRootNode -> ClientStComplete . (fmap . fmap) f . recvMsgComplete
        SPayNode node' -> mapPop n node'
        SIfLNode node' -> mapCanPush n (SIfRNode node')
        SIfRNode node' -> mapPop n node'
        SWhenNode node' -> mapPop n node'
        SCaseNode node' -> mapCanPush n (SWhenNode node')
        SLetNode node' -> mapPop n node'
        SAssertNode node' -> mapPop n node'

-- A client in the Processing state.
newtype ClientStProcessing (node :: Node) m a = ClientStProcessing
  { recvMsgResume :: forall n. Nat ('S n) -> m (ClientStCanPush ('S n) node m a)
    -- ^ The server has instructed the client to resume pushing contract nodes.
  }

-- A type family that computes the next client state when popping a node.
-- Corresponds to the @@Pop@@ type family.
type family ClientStPop (n :: N) (node :: Node) :: (Type -> Type) -> Type -> Type where
  ClientStPop n 'RootNode = ClientStComplete
  ClientStPop n ('PayNode node) = ClientStPop n node
  ClientStPop n ('IfLNode node) = ClientStCanPush n ('IfRNode node)
  ClientStPop n ('IfRNode node) = ClientStPop n node
  ClientStPop n ('WhenNode node) = ClientStPop n node
  ClientStPop n ('CaseNode node) = ClientStCanPush n ('WhenNode node)
  ClientStPop n ('LetNode node) = ClientStPop n node
  ClientStPop n ('AssertNode node) = ClientStPop n node

-- A client in the CanPush state.
data ClientStCanPush (n :: N) (node :: Node) m a where
  -- | Push a close node to the stack. This closes the current stack and pops
  -- until the next incomplete location in the contract is reached.
  PushClose
    :: m (ClientStPop n node m a) -- ^ The next client to run after popping the current stack.
    -> ClientStCanPush ('S n) node m a
  -- | Push a pay node to the stack.
  PushPay
    :: AccountId -- ^ The account from which to withdraw the payment.
    -> Payee -- ^ To whom send the payment.
    -> Token -- ^ The token to be paid.
    -> Value Observation -- ^ The amount to be paid.
    -> m (ClientStCanPush n ('PayNode node) m a) -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Push an if node to the stack.
  PushIf
    :: Observation -- ^ The observation to choose which branch to follow.
    -> m (ClientStCanPush n ('IfLNode node) m a) -- ^ The next client to push the "then" sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Push a when node to the stack
  PushWhen
    :: Timeout -- ^ The time (in POSIX milliseconds) after which this contract will time out.
    -> m (ClientStCanPush n ('WhenNode node) m a) -- ^ The next client to push either the first case or the timeout continuation.
    -> ClientStCanPush ('S n) node m a
  -- | Push a case node to the stack. Only available if currently on a when node.
  PushCase
    :: Action -- ^ The action which will match this case.
    -> m (ClientStCanPush n ('CaseNode node) m a) -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) ('WhenNode node) m a
  -- | Push a let node to the stack.
  PushLet
    :: ValueId -- ^ The name that refers to the value.
    -> Value Observation -- ^ The value to bind to the name.
    -> m (ClientStCanPush n ('LetNode node) m a) -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Push an assert node to the stack.
  PushAssert
    :: Observation -- ^ The observation to assert.
    -> m (ClientStCanPush n ('AssertNode node) m a) -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Request to resume pushing.
  RequestResume
    :: ClientStProcessing node m a -- ^ The next client to run on resume
    -> ClientStCanPush 'Z node m a
  -- | Abort the load.
  Abort :: a -> ClientStCanPush n node m a

-- A client in the Complete state.
newtype ClientStComplete m a = ClientStComplete
  { recvMsgComplete :: DatumHash -> m a
    -- ^ The server sends the hash of the merkleized root contract.
  }

hoistMarloweLoadClient
  :: forall m n a
   . Functor m
  => (forall x. m x -> n x)
  -> MarloweLoadClient m a
  -> MarloweLoadClient n a
hoistMarloweLoadClient f = MarloweLoadClient . f . fmap (hoistProcessing SRootNode) . runMarloweLoadClient
  where
    hoistProcessing :: SNode node -> ClientStProcessing node m a -> ClientStProcessing node n a
    hoistProcessing node ClientStProcessing{..} =
      ClientStProcessing \n -> f $ fmap (hoistCanPush node n) $ recvMsgResume n

    hoistCanPush :: SNode node -> Nat i -> ClientStCanPush i node m a -> ClientStCanPush i node n a
    hoistCanPush node Zero = \case
      Abort a -> Abort a
      RequestResume processing -> RequestResume $ hoistProcessing node processing
    hoistCanPush node (Succ n) = \case
      Abort a -> Abort a
      PushClose next -> PushClose $ f $ hoistPop node n <$> next
      PushPay payor payee token value next -> PushPay payor payee token value $ f $ hoistCanPush (SPayNode node) n <$> next
      PushIf cond next -> PushIf cond $ f $ hoistCanPush (SIfLNode node) n <$> next
      PushWhen timeout next -> PushWhen timeout $ f $ hoistCanPush (SWhenNode node) n <$> next
      PushCase action next -> PushCase action case node of
        SWhenNode node' -> f $ hoistCanPush (SCaseNode node') n <$> next
      PushLet valueId value next -> PushLet valueId value $ f $ hoistCanPush (SLetNode node) n <$> next
      PushAssert obs next -> PushAssert obs $ f $ hoistCanPush (SAssertNode node) n <$> next

    hoistPop :: SNode node -> Nat i -> ClientStPop i node m a -> ClientStPop i node n a
    hoistPop node n = case node of
      SRootNode -> hoistComplete
      SPayNode node' -> hoistPop node' n
      SIfLNode node' -> hoistCanPush (SIfRNode node') n
      SIfRNode node' -> hoistPop node' n
      SWhenNode node' -> hoistPop node' n
      SCaseNode node' -> hoistCanPush (SWhenNode node') n
      SLetNode node' -> hoistPop node' n
      SAssertNode node' -> hoistPop node' n

    hoistComplete :: ClientStComplete m a -> ClientStComplete n a
    hoistComplete = ClientStComplete . fmap f . recvMsgComplete

-- ^ Interpret a client as a traced typed-protocols peer.
marloweLoadClientPeer
  :: forall m a
   . Functor m
  => MarloweLoadClient m a
  -> PeerTraced MarloweLoad 'AsClient ('StProcessing 'RootNode) m a
marloweLoadClientPeer = EffectTraced . fmap (peerProcessing SRootNode) . runMarloweLoadClient
  where
  peerProcessing :: SNode node -> ClientStProcessing node m a -> PeerTraced MarloweLoad 'AsClient ('StProcessing node) m a
  peerProcessing node ClientStProcessing{..} = AwaitTraced (ServerAgency $ TokProcessing node) \case
    MsgResume n -> Receive $ EffectTraced $ peerCanPush n node <$> recvMsgResume n

  peerCanPush :: Nat n -> SNode node -> ClientStCanPush n node m a -> PeerTraced MarloweLoad 'AsClient ('StCanPush n node) m a
  peerCanPush Zero node = \case
    Abort a ->
      YieldTraced (ClientAgency $ TokCanPush Zero node) MsgAbort
        $ Peer.Close TokDone a
    RequestResume ClientStProcessing{..} ->
      YieldTraced (ClientAgency $ TokCanPush Zero node) MsgRequestResume
        $ Call (ServerAgency $ TokProcessing node) \case
          MsgResume n -> EffectTraced $ peerCanPush n node <$> recvMsgResume n
  peerCanPush (Succ n) node = \case
    Abort a ->
      YieldTraced (ClientAgency $ TokCanPush (Succ n) node) MsgAbort
        $ Peer.Close TokDone a
    PushClose next ->
      YieldTraced tok MsgPushClose $ Cast $ EffectTraced $ peerPop n node <$> next
    PushPay payor payee token value next ->
      YieldTraced tok (MsgPushPay payor payee token value) $ Cast $ EffectTraced $ peerCanPush n (SPayNode node) <$> next
    PushIf cond next ->
      YieldTraced tok (MsgPushIf cond) $ Cast $ EffectTraced $ peerCanPush n (SIfLNode node) <$> next
    PushWhen timeout next ->
      YieldTraced tok (MsgPushWhen timeout) $ Cast $ EffectTraced $ peerCanPush n (SWhenNode node) <$> next
    PushCase action next -> case node of
      SWhenNode node' -> YieldTraced tok (MsgPushCase action) $ Cast $ EffectTraced $ peerCanPush n (SCaseNode node') <$> next
    PushLet valueId value next ->
      YieldTraced tok (MsgPushLet valueId value) $ Cast $ EffectTraced $ peerCanPush n (SLetNode node) <$> next
    PushAssert obs next ->
      YieldTraced tok (MsgPushAssert obs) $ Cast $ EffectTraced $ peerCanPush n (SAssertNode node) <$> next
    where
      tok = ClientAgency $ TokCanPush (Succ n) node

  peerPop
    :: Nat n
    -> SNode node
    -> ClientStPop n node m a
    -> PeerTraced MarloweLoad 'AsClient (Pop n node) m a
  peerPop n node client = case node of
    SRootNode -> peerComplete client
    SPayNode node' -> peerPop n node' client
    SIfLNode node' -> peerCanPush n (SIfRNode node') client
    SIfRNode node' -> peerPop n node' client
    SWhenNode node' -> peerPop n node' client
    SCaseNode node' -> peerCanPush n (SWhenNode node') client
    SLetNode node' -> peerPop n node' client
    SAssertNode node' -> peerPop n node' client

  peerComplete :: ClientStComplete m a -> PeerTraced MarloweLoad 'AsClient 'StComplete m a
  peerComplete ClientStComplete{..} = AwaitTraced (ServerAgency TokComplete) \case
    MsgComplete hash -> Closed TokDone $ recvMsgComplete hash

-- | Load a contract into the runtime in a space-efficient way. Note: this
-- relies on the input contract being lazily evaluated and garbage collected as
-- it is processed. This means that the caller of this function should avoid holding
-- onto a reference to the input contract (e.g. in a let-binding). Preferably,
-- the return value of a function that generates the contract should be passed
-- directly to this function.
--
-- Returns the hash of the merkleized contract, or nothing if the contract is
-- already merkleized.
pushContract
  :: forall m
   . Applicative m
  => Contract
  -- ^ The unmerkleized contract to load.
  -> MarloweLoadClient m (Maybe DatumHash)
pushContract root = MarloweLoadClient $ pure $ ClientStProcessing \n ->
  pure $ push n StateRoot root
  where
    push
      :: Nat n
      -> PeerState node
      -> Contract
      -> ClientStCanPush n node m (Maybe DatumHash)
    push Zero state = \contract -> RequestResume $ ClientStProcessing \n ->
      pure $ push n state contract
    push (Succ n) state = \case
      Close ->
        PushClose $ pure $ pop n state
      Pay payee payor token value next ->
        PushPay payee payor token value $ pure $ push n (StatePay state) next
      If obs tru fal ->
        PushIf obs $ pure $ push n (StateIfL fal state) tru
      When cases timeout fallback -> PushWhen timeout case cases of
        [] -> pure $ push n (StateWhen state) fallback
        (c : cs) -> pure $ pushCase n c cs fallback state
      Let valueId value next ->
        PushLet valueId value $ pure $ push n (StateLet state) next
      Assert obs next ->
        PushAssert obs $ pure $ push n (StateAssert state) next

    pop :: Nat n -> PeerState node -> ClientStPop n node m (Maybe DatumHash)
    pop n = \case
      StateRoot -> ClientStComplete $ pure . Just
      StatePay state -> pop n state
      StateIfL fal state -> push n (StateIfR state) fal
      StateIfR state -> pop n state
      StateWhen state -> pop n state
      StateCase [] fallback state -> push n (StateWhen state) fallback
      StateCase (c : cs) fallback state -> pushCase n c cs fallback state
      StateLet state -> pop n state
      StateAssert state -> pop n state

    pushCase
      :: Nat n
      -> Case Contract
      -> [Case Contract]
      -> Contract
      -> PeerState node
      -> ClientStCanPush n ('WhenNode node) m (Maybe DatumHash)
    pushCase n c cs fallback state = case n of
      Zero -> RequestResume $ ClientStProcessing \n' ->
        pure $ pushCase' n' c cs fallback state
      Succ n' -> pushCase' (Succ n') c cs fallback state

    pushCase'
      :: Nat ('S n)
      -> Case Contract
      -> [Case Contract]
      -> Contract
      -> PeerState node
      -> ClientStCanPush ('S n) ('WhenNode node) m (Maybe DatumHash)
    pushCase' (Succ n) c cs fallback state = case c of
      Case action next -> PushCase action $ pure $ push n (StateCase cs fallback state) next
      MerkleizedCase _ _ -> Abort Nothing

data PeerState (node :: Node) where
  StateRoot :: PeerState 'RootNode
  StatePay :: PeerState node -> PeerState ('PayNode node)
  StateIfL :: Contract -> PeerState node -> PeerState ('IfLNode node)
  StateIfR :: PeerState node -> PeerState ('IfRNode node)
  StateWhen :: PeerState node -> PeerState ('WhenNode node)
  StateCase :: [Case Contract] -> Contract -> PeerState node -> PeerState ('CaseNode node)
  StateLet :: PeerState node -> PeerState ('LetNode node)
  StateAssert :: PeerState node -> PeerState ('AssertNode node)

serveMarloweLoadClient
  :: forall m a b
   . Monad m
  => MarloweLoadServer m a
  -> MarloweLoadClient m b
  -> m (a, b)
serveMarloweLoadClient MarloweLoadServer{..} MarloweLoadClient{..} =
  join $ serveProcessing SRootNode <$> runMarloweLoadClient <*> runMarloweLoadServer
  where
    serveProcessing
      :: SNode node
      -> ClientStProcessing node m b
      -> ServerStProcessing node m a
      -> m (a, b)
    serveProcessing node ClientStProcessing{..} = \case
      SendMsgResume n push -> servePush n node push =<< recvMsgResume n

    servePush
      :: Nat n
      -> SNode node
      -> ServerStCanPush n node m a
      -> ClientStCanPush n node m b
      -> m (a, b)
    servePush Zero node (ServerStPaused ServerStCanPushZero{..}) = \case
      RequestResume processing -> serveProcessing node processing =<< recvMsgRequestResume
      Abort b -> (,b) <$> recvMsgAbort
    servePush (Succ n) node (ServerStCanPush ServerStCanPushSucc{..}) = \case
      PushClose mPop -> join $ servePop n node <$> recvClose <*> mPop
      PushPay a p t v mPush -> join $ servePush n (SPayNode node) <$> recvPay a p t v <*> mPush
      PushIf o mPush -> join $ servePush n (SIfLNode node) <$> recvIf o <*> mPush
      PushWhen t mPush -> join $ servePush n (SWhenNode node) <$> recvWhen t <*> mPush
      PushCase c mPush -> case node of
        SWhenNode node' -> join $ servePush n (SCaseNode node') <$> recvCase Refl c <*> mPush
      PushLet vid v mPush -> join $ servePush n (SLetNode node) <$> recvLet vid v <*> mPush
      PushAssert o mPush -> join $ servePush n (SAssertNode node) <$> recvAssert o <*> mPush
      Abort b -> (,b) <$> recvMsgAbort

    servePop
      :: Nat n
      -> SNode node
      -> ServerStPop n node m a
      -> ClientStPop n node m b
      -> m (a, b)
    servePop n = \case
      SRootNode -> flip \ClientStComplete{..} -> \case
        SendMsgComplete hash ma -> (,) <$> ma <*> recvMsgComplete hash
      SPayNode node -> servePop n node
      SIfLNode node -> servePush n (SIfRNode node)
      SIfRNode node -> servePop n node
      SWhenNode node -> servePop n node
      SCaseNode node -> servePush n (SWhenNode node)
      SLetNode node -> servePop n node
      SAssertNode node -> servePop n node
