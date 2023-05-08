{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A client machine type for loading contracts incrementally.

module Language.Marlowe.Protocol.Load.Client
  where

import Data.Kind (Type)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.Protocol.Peer.Trace hiding (Close)
import Network.TypedProtocol

-- A client of the MarloweLoad protocol.
newtype MarloweLoadClient m a = MarloweLoadClient
  { runMarloweLoadClient :: m (ClientStProcessing 'RootNode m a)
  }

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
    :: ClientStPop n node m a -- ^ The next client to run after popping the current stack.
    -> ClientStCanPush ('S n) node m a
  -- | Push a pay node to the stack.
  PushPay
    :: AccountId -- ^ The account from which to withdraw the payment.
    -> Payee -- ^ To whom send the payment.
    -> Token -- ^ The token to be paid.
    -> Value Observation -- ^ The amount to be paid.
    -> ClientStCanPush n ('PayNode node) m a -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Push an if node to the stack.
  PushIf
    :: Observation -- ^ The observation to choose which branch to follow.
    -> ClientStCanPush n ('IfLNode node) m a -- ^ The next client to push the "then" sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Push a when node to the stack
  PushWhen
    :: Timeout -- ^ The time (in POSIX milliseconds) after which this contract will time out.
    -> ClientStCanPush n ('WhenNode node) m a -- ^ The next client to push either the first case or the timeout continuation.
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
    -> ClientStCanPush n ('LetNode node) m a -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Push an assert node to the stack.
  PushAssert
    :: Observation -- ^ The observation to assert.
    -> ClientStCanPush n ('AssertNode node) m a -- ^ The next client to push the sub-contract.
    -> ClientStCanPush ('S n) node m a
  -- | Request to resume pushing.
  RequestResume
    :: ClientStProcessing node m a -- ^ The next client to run on resume
    -> ClientStCanPush 'Z node m a

-- A client in the Complete state.
newtype ClientStComplete m a = ClientStComplete
  { recvMsgComplete :: DatumHash -> m a
    -- ^ The server sends the hash of the merkleized root contract.
  }

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
    RequestResume ClientStProcessing{..} ->
      YieldTraced (ClientAgency $ TokCanPush Zero node) MsgRequestResume
        $ Call (ServerAgency $ TokProcessing node) \case
          MsgResume n -> EffectTraced $ peerCanPush n node <$> recvMsgResume n
  peerCanPush (Succ n) node = \case
    PushClose next ->
      YieldTraced tok MsgPushClose $ Cast $ peerPop n node next
    PushPay payor payee token value next ->
      YieldTraced tok (MsgPushPay payor payee token value) $ Cast $ peerCanPush n (SPayNode node) next
    PushIf cond next ->
      YieldTraced tok (MsgPushIf cond) $ Cast $ peerCanPush n (SIfLNode node) next
    PushWhen timeout next ->
      YieldTraced tok (MsgPushWhen timeout) $ Cast $ peerCanPush n (SWhenNode node) next
    PushCase action next -> case node of
      SWhenNode node' -> EffectTraced $ YieldTraced tok (MsgPushCase action) . Cast . peerCanPush n (SCaseNode node') <$> next
    PushLet valueId value next ->
      YieldTraced tok (MsgPushLet valueId value) $ Cast $ peerCanPush n (SLetNode node) next
    PushAssert obs next ->
      YieldTraced tok (MsgPushAssert obs) $ Cast $ peerCanPush n (SAssertNode node) next
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
pushContract
  :: forall m
   . MonadFail m
  -- The unmerkleized contract to load.
  => Contract
  -- A client that loads the entire contract into the runtime, returning the hash
  -- of the merkleized contract.
  -> MarloweLoadClient m DatumHash
pushContract root = MarloweLoadClient $ pure $ ClientStProcessing \n ->
  pure $ push n StateRoot root
  where
    push
      :: Nat n
      -> PeerState node
      -> Contract
      -> ClientStCanPush n node m DatumHash
    push Zero state = \contract -> RequestResume $ ClientStProcessing \n ->
      pure $ push n state contract
    push (Succ n) state = \case
      Close ->
        PushClose $ pop n state
      Pay payee payor token value next ->
        PushPay payee payor token value $ push n (StatePay state) next
      If obs tru fal ->
        PushIf obs $ push n (StateIfL fal state) tru
      When cases timeout fallback -> PushWhen timeout case cases of
        [] -> push n (StateWhen state) fallback
        (c : cs) -> pushCase n c cs fallback state
      Let valueId value next ->
        PushLet valueId value $ push n (StateLet state) next
      Assert obs next ->
        PushAssert obs $ push n (StateAssert state) next

    pop :: Nat n -> PeerState node -> ClientStPop n node m DatumHash
    pop n = \case
      StateRoot -> ClientStComplete pure
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
      -> ClientStCanPush n ('WhenNode node) m DatumHash
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
      -> ClientStCanPush ('S n) ('WhenNode node) m DatumHash
    pushCase' (Succ n) c cs fallback state = case c of
      Case action next -> PushCase action $ pure $ push n (StateCase cs fallback state) next
      MerkleizedCase action _ -> PushCase action $ fail "merkleized contract detected"

data PeerState (node :: Node) where
  StateRoot :: PeerState 'RootNode
  StatePay :: PeerState node -> PeerState ('PayNode node)
  StateIfL :: Contract -> PeerState node -> PeerState ('IfLNode node)
  StateIfR :: PeerState node -> PeerState ('IfRNode node)
  StateWhen :: PeerState node -> PeerState ('WhenNode node)
  StateCase :: [Case Contract] -> Contract -> PeerState node -> PeerState ('CaseNode node)
  StateLet :: PeerState node -> PeerState ('LetNode node)
  StateAssert :: PeerState node -> PeerState ('AssertNode node)
