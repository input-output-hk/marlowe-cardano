{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Language.Marlowe.Protocol.Load.Server
  where

import Cardano.Api (hashScriptData)
import Data.Kind (Type)
import Data.Type.Equality (type (:~:)(..))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Network.TypedProtocol
import Plutus.V2.Ledger.Api (toBuiltin)

newtype MarloweLoadServer m a = MarloweLoadServer
  { runMarloweLoadServer :: m (ServerStProcessing 'RootNode m a)
  }

data ServerStProcessing (node :: Node) m a where
  SendMsgResume :: Nat ('S n) -> ServerStCanPush n node m a -> ServerStProcessing node m a

type family ServerStPop (n :: N) (node :: Node) :: (Type -> Type) -> Type -> Type where
  ServerStPop n 'RootNode = ServerStComplete
  ServerStPop n ('PayNode node) = ServerStPop n node
  ServerStPop n ('IfLNode node) = ServerStPush n ('IfRNode node)
  ServerStPop n ('IfRNode node) = ServerStPop n node
  ServerStPop n ('WhenNode node) = ServerStPop n node
  ServerStPop n ('CaseNode node) = ServerStPush n ('WhenNode node)
  ServerStPop n ('LetNode node) = ServerStPop n node
  ServerStPop n ('AssertNode node) = ServerStPop n node

type family ServerStPush (n :: N) (node :: Node) = (c :: (Type -> Type) -> Type -> Type) | c -> n node where
  ServerStPush 'Z node = ServerStProcessing node
  ServerStPush ('S n) node = ServerStCanPush n node

data ServerStCanPush (n :: N) (node :: Node) m a = ServerStCanPush
  { recvClose :: m (ServerStPop n node m a)
  , recvPay :: AccountId -> Payee -> Token -> Value Observation -> m (ServerStPush n ('PayNode node) m a)
  , recvIf :: Observation -> m (ServerStPush n ('IfLNode node) m a)
  , recvWhen :: Timeout -> m (ServerStPush n ('WhenNode node) m a)
  , recvCase :: forall st'. node :~: 'WhenNode st' -> Action -> m (ServerStPush n ('CaseNode st') m a)
  , recvLet :: ValueId -> Value Observation -> m (ServerStPush n ('LetNode node) m a)
  , recvAssert :: Observation -> m (ServerStPush n ('AssertNode node) m a)
  }

data ServerStComplete m a where
  SendMsgComplete :: DatumHash -> m a -> ServerStComplete m a

marloweLoadServerPeer
  :: forall m a
   . Functor m
  => MarloweLoadServer m a
  -> Peer MarloweLoad 'AsServer ('StProcessing 'RootNode) m a
marloweLoadServerPeer = Effect . fmap (peerProcessing SRootNode) . runMarloweLoadServer
  where
  peerProcessing :: SNode node -> ServerStProcessing node m a -> Peer MarloweLoad 'AsServer ('StProcessing node) m a
  peerProcessing node = \case
    SendMsgResume (Succ n) next ->
      Yield (ServerAgency $ TokProcessing node) (MsgResume (Succ n)) $ peerCanPush n node next

  peerCanPush
    :: Nat n
    -> SNode node
    -> ServerStCanPush n node m a
    -> Peer MarloweLoad 'AsServer ('StCanPush n node) m a
  peerCanPush n node ServerStCanPush{..} = Await (ClientAgency $ TokCanPush n node) $ Effect . \case
    MsgPushClose -> peerPop n node <$> recvClose
    MsgPushPay payor payee token value ->
      peerPush n (SPayNode node) <$> recvPay payor payee token value
    MsgPushIf cond ->
      peerPush n (SIfLNode node) <$> recvIf cond
    MsgPushWhen timeout ->
      peerPush n (SWhenNode node) <$> recvWhen timeout
    MsgPushCase action -> case node of
      SWhenNode st' -> peerPush n (SCaseNode st') <$> recvCase Refl action
    MsgPushLet valueId value ->
      peerPush n (SLetNode node) <$> recvLet valueId value
    MsgPushAssert obs ->
      peerPush n (SAssertNode node) <$> recvAssert obs

  peerPop
    :: Nat n
    -> SNode node
    -> ServerStPop n node m a
    -> Peer MarloweLoad 'AsServer (Pop n node) m a
  peerPop n node client = case node of
    SRootNode -> peerComplete client
    SPayNode node' -> peerPop n node' client
    SIfLNode node' -> peerPush n (SIfRNode node') client
    SIfRNode node' -> peerPop n node' client
    SWhenNode node' -> peerPop n node' client
    SCaseNode node' -> peerPush n (SWhenNode node') client
    SLetNode node' -> peerPop n node' client
    SAssertNode node' -> peerPop n node' client

  peerPush
    :: Nat n
    -> SNode node
    -> ServerStPush n node m a
    -> Peer MarloweLoad 'AsServer (Push n node) m a
  peerPush n node client = case n of
    Zero -> peerProcessing node client
    Succ n' -> peerCanPush n' node client

  peerComplete :: ServerStComplete m a -> Peer MarloweLoad 'AsServer 'StComplete m a
  peerComplete (SendMsgComplete hash next) = Yield (ServerAgency TokComplete) (MsgComplete hash)
    $ Effect
    $ Done TokDone <$> next

pullContract
  :: forall m batchSize
   . Applicative m
  => Nat ('S batchSize)
  -> (Contract -> m DatumHash)
  -> MarloweLoadServer m Contract
pullContract batchSize@(Succ batchSize') save =
  MarloweLoadServer $ pure $ SendMsgResume batchSize $ pullContract' batchSize' StateRoot
  where
    pullContract'
      :: Nat n
      -> PeerState node
      -> ServerStCanPush n node m Contract
    pullContract' n state = ServerStCanPush
      { recvClose = popState n state Close
      , recvPay = \payor payee token value ->
          pure $ pushState n (StatePay payor payee token value state)
      , recvIf = \cond ->
          pure $ pushState n (StateIfL cond state)
      , recvWhen = \timeout ->
          pure $ pushState n (StateWhen timeout [] state)
      , recvCase = \Refl action -> case state of
          StateWhen timeout cases st' ->
            pure $ pushState n (StateCase action timeout cases st')
      , recvLet = \valueId value ->
          pure $ pushState n (StateLet valueId value state)
      , recvAssert = \obs ->
          pure $ pushState n (StateAssert obs state)
      }

    popState :: Nat n -> PeerState node -> Contract -> m (ServerStPop n node m Contract)
    popState n state contract = case state of
      StateRoot -> case contract of
        Close -> pure $ SendMsgComplete closeHash $ pure contract
        _ -> do
          hash <- save contract
          pure $ SendMsgComplete hash $ pure contract
      StatePay payor payee token value st' ->
        popState n st' (Pay payor payee token value contract)
      StateIfL cond st' ->
        pure $ pushState n (StateIfR cond contract st')
      StateIfR cond tru st' ->
        popState n st' (If cond tru contract)
      StateWhen timeout cases st' ->
        popState n st' (When (reverse cases) timeout contract)
      StateCase action timeout cases st' -> case contract of
        Close -> pure $ pushState
          n
          (StateWhen timeout (Case action Close : cases) st')
        _ -> do
          hash <- save contract
          pure $ pushState
            n
            (StateWhen timeout (MerkleizedCase action (toBuiltin $ unDatumHash hash) : cases) st')
      StateLet valueId value st' ->
        popState n st' (Let valueId value contract)
      StateAssert obs st' ->
        popState n st' (Assert obs contract)

    pushState :: Nat n -> PeerState node -> ServerStPush n node m Contract
    pushState n state = case n of
      Zero -> SendMsgResume batchSize $ pullContract' batchSize' state
      Succ n' -> pullContract' n' state

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
