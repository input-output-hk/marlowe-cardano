{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Client
  where

import Data.Kind (Type)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.TypedProtocol

newtype MarloweLoadClient m a = MarloweLoadClient
  { runMarloweLoadClient :: m (ClientStCanPush 'StRoot m a)
  }

type family ClientStPop (st :: MarloweLoad) = (c :: (Type -> Type) -> Type -> Type) | c -> st where
  ClientStPop ('StCanPush st) = ClientStCanPush st
  ClientStPop 'StComplete = ClientStComplete

data ClientStCanPush (st :: CanPush) m a where
  PushClose :: ClientStPop (Pop st) m a -> ClientStCanPush st m a
  PushPay
    :: AccountId
    -> Payee
    -> Token
    -> Value Observation
    -> ClientStCanPush ('StPay st) m a
    -> ClientStCanPush st m a
  PushIf
    :: Observation
    -> ClientStCanPush ('StIfL st) m a
    -> ClientStCanPush st m a
  PushWhen
    :: Timeout
    -> ClientStCanPush ('StWhen st) m a
    -> ClientStCanPush st m a
  PushCase
    :: Action
    -> m (ClientStCanPush ('StCase st) m a)
    -> ClientStCanPush ('StWhen st) m a
  PushLet
    :: ValueId
    -> Value Observation
    -> ClientStCanPush ('StLet st) m a
    -> ClientStCanPush st m a
  PushAssert
    :: Observation
    -> ClientStCanPush ('StAssert st) m a
    -> ClientStCanPush st m a

newtype ClientStComplete m a = ClientStComplete
  { recvMsgComplete :: DatumHash -> m a
  }

marloweLoadClientPeer
  :: forall m a
   . Functor m
  => MarloweLoadClient m a
  -> Peer MarloweLoad 'AsClient ('StCanPush 'StRoot) m a
marloweLoadClientPeer = Effect . fmap (peerCanPush TokRoot) . runMarloweLoadClient
  where
  peerCanPush :: StCanPush st -> ClientStCanPush st m a -> Peer MarloweLoad 'AsClient ('StCanPush st) m a
  peerCanPush st = \case
    PushClose next ->
      Yield tok MsgPushClose $ peerPop st next
    PushPay payor payee token value next ->
      Yield tok (MsgPushPay payor payee token value) $ peerCanPush (TokPay st) next
    PushIf cond next ->
      Yield tok (MsgPushIf cond) $ peerCanPush (TokIfL st) next
    PushWhen timeout next ->
      Yield tok (MsgPushWhen timeout) $ peerCanPush (TokWhen st) next
    PushCase action next -> case st of
      TokWhen st' -> Effect $ Yield tok (MsgPushCase action) . peerCanPush (TokCase st') <$> next
    PushLet valueId value next ->
      Yield tok (MsgPushLet valueId value) $ peerCanPush (TokLet st) next
    PushAssert obs next ->
      Yield tok (MsgPushAssert obs) $ peerCanPush (TokAssert st) next
    where
      tok = ClientAgency $ TokCanPush st

  peerPop
    :: StCanPush st
    -> ClientStPop (Pop st) m a
    -> Peer MarloweLoad 'AsClient (Pop st) m a
  peerPop st client = case stPop st of
    SomePeerHasAgency (ClientAgency (TokCanPush st')) -> peerCanPush st' client
    SomePeerHasAgency (ServerAgency TokComplete) -> peerComplete client

  peerComplete :: ClientStComplete m a -> Peer MarloweLoad 'AsClient 'StComplete m a
  peerComplete ClientStComplete{..} = Await (ServerAgency TokComplete) \case
    MsgComplete hash -> Effect $ Done TokDone <$> recvMsgComplete hash

-- | Load a contract into the runtime in a space-efficient way. Note: this
-- relies on the input contract being lazily evaluated and garbage collected as
-- it is processed. This means that the caller of this function should avoid holding
-- onto a reference to the input contract (e.g. in a let-binding). Preferably,
-- the return value of a function that generates the contract should be passed
-- directly to this function.
pushContract
  :: MonadFail m
  -- The unmerkleized contract to load.
  => Contract
  -- A client that loads the entire contract into the runtime, returning the hash
  -- of the merkleized contract.
  -> MarloweLoadClient m DatumHash
pushContract = MarloweLoadClient . pure . pushContract' StateRoot
  where
    pushContract'
      :: MonadFail m
      => PeerState st
      -> Contract
      -> ClientStCanPush st m DatumHash
    pushContract' st = \case
      Close ->
        PushClose $ popState st
      Pay payee payor token value next ->
        PushPay payee payor token value $ pushContract' (StatePay st) next
      If obs tru fal ->
        PushIf obs $ pushContract' (StateIfL fal st) tru
      When cases timeout fallback -> PushWhen timeout $ popState (StateCase cases fallback st)
      Let valueId value next ->
        PushLet valueId value $ pushContract' (StateLet st) next
      Assert obs next ->
        PushAssert obs $ pushContract' (StateAssert st) next

    popState :: MonadFail m => PeerState st -> ClientStPop (Pop st) m DatumHash
    popState = \case
      StateRoot -> ClientStComplete pure
      StatePay st -> popState st
      StateIfL fal st -> pushContract' (StateIfR st) fal
      StateIfR st -> popState st
      StateWhen st -> popState st
      StateCase [] fallback st -> pushContract' (StateWhen st) fallback
      StateCase (case' : cases) fallback st -> case case' of
        Case action next -> PushCase action $ pure $ pushContract' (StateCase cases fallback st) next
        MerkleizedCase action _ -> PushCase action $ fail "merkleized contract detected"
      StateLet st -> popState st
      StateAssert st -> popState st

data PeerState (st :: CanPush) where
  StateRoot :: PeerState 'StRoot
  StatePay :: PeerState st -> PeerState ('StPay st)
  StateIfL :: Contract -> PeerState st -> PeerState ('StIfL st)
  StateIfR :: PeerState st -> PeerState ('StIfR st)
  StateWhen :: PeerState st -> PeerState ('StWhen st)
  StateCase :: [Case Contract] -> Contract -> PeerState st -> PeerState ('StCase st)
  StateLet :: PeerState st -> PeerState ('StLet st)
  StateAssert :: PeerState st -> PeerState ('StAssert st)
