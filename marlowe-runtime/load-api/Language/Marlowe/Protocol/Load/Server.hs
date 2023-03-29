{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Server
  where

import Data.Kind (Type)
import Data.Type.Equality (type (:~:)(..))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..))
import Network.TypedProtocol
import Plutus.V2.Ledger.Api (toBuiltin)

newtype MarloweLoadServer m a = MarloweLoadServer
  { runMarloweLoadServer :: m (ServerStCanPush 'StRoot m a)
  }

type family ServerStPop (st :: MarloweLoad) = (c :: (Type -> Type) -> Type -> Type) | c -> st where
  ServerStPop ('StCanPush st) = ServerStCanPush st
  ServerStPop 'StComplete = ServerStComplete

data ServerStCanPush (st :: CanPush) m a = ServerStCanPush
  { pushClose :: m (ServerStPop (Pop st) m a)
  , pushPay :: AccountId -> Payee -> Token -> Value Observation -> m (ServerStCanPush ('StPay st) m a)
  , pushIf :: Observation -> m (ServerStCanPush ('StIfL st) m a)
  , pushWhen :: Timeout -> m (ServerStCanPush ('StWhen st) m a)
  , pushCase :: forall st'. st :~: 'StWhen st' -> Action -> m (ServerStCanPush ('StCase st') m a)
  , pushLet :: ValueId -> Value Observation -> m (ServerStCanPush ('StLet st) m a)
  , pushAssert :: Observation -> m (ServerStCanPush ('StAssert st) m a)
  }

data ServerStComplete m a where
  SendMsgComplete :: DatumHash -> m a -> ServerStComplete m a

marloweLoadServerPeer
  :: forall m a
   . Functor m
  => MarloweLoadServer m a
  -> Peer MarloweLoad 'AsServer ('StCanPush 'StRoot) m a
marloweLoadServerPeer = Effect . fmap (peerCanPush TokRoot) . runMarloweLoadServer
  where
  peerCanPush :: StCanPush st -> ServerStCanPush st m a -> Peer MarloweLoad 'AsServer ('StCanPush st) m a
  peerCanPush st ServerStCanPush{..} = Await (ClientAgency $ TokCanPush st) $ Effect . \case
    MsgPushClose -> peerPop st <$> pushClose
    MsgPushPay payor payee token value ->
      peerCanPush (TokPay st) <$> pushPay payor payee token value
    MsgPushIf cond ->
      peerCanPush (TokIfL st) <$> pushIf cond
    MsgPushWhen timeout ->
      peerCanPush (TokWhen st) <$> pushWhen timeout
    MsgPushCase action -> case st of
      TokWhen st' -> peerCanPush (TokCase st') <$> pushCase Refl action
    MsgPushLet valueId value ->
      peerCanPush (TokLet st) <$> pushLet valueId value
    MsgPushAssert obs ->
      peerCanPush (TokAssert st) <$> pushAssert obs

  peerPop
    :: StCanPush st
    -> ServerStPop (Pop st) m a
    -> Peer MarloweLoad 'AsServer (Pop st) m a
  peerPop st client = case stPop st of
    SomePeerHasAgency (ClientAgency (TokCanPush st')) -> peerCanPush st' client
    SomePeerHasAgency (ServerAgency TokComplete) -> peerComplete client

  peerComplete :: ServerStComplete m a -> Peer MarloweLoad 'AsServer 'StComplete m a
  peerComplete (SendMsgComplete hash next) = Yield (ServerAgency TokComplete) (MsgComplete hash)
    $ Effect
    $ Done TokDone <$> next

pullContract
  :: Applicative m
  => (Contract -> m DatumHash)
  -> MarloweLoadServer m Contract
pullContract = MarloweLoadServer . pure . pullContract' StateRoot
  where
    pullContract'
      :: Applicative m
      => PeerState st
      -> (Contract -> m DatumHash)
      -> ServerStCanPush st m Contract
    pullContract' st save = ServerStCanPush
      { pushClose = popState st save Close
      , pushPay = \payor payee token value -> pure $ pullContract'
          (StatePay payor payee token value st)
          save
      , pushIf = \cond -> pure $ pullContract'
          (StateIfL cond st)
          save
      , pushWhen = \timeout -> pure $ pullContract'
          (StateWhen timeout [] st)
          save
      , pushCase = \Refl action -> case st of
          StateWhen timeout cases st' -> pure $ pullContract'
            (StateCase action timeout cases st')
            save
      , pushLet = \valueId value -> pure $ pullContract'
          (StateLet valueId value st)
          save
      , pushAssert = \obs -> pure $ pullContract'
          (StateAssert obs st)
          save
      }

    popState
      :: Applicative m
      => PeerState st
      -> (Contract -> m DatumHash)
      -> Contract
      -> m (ServerStPop (Pop st) m Contract)
    popState st save contract = case st of
      StateRoot -> do
        hash <- save contract
        pure $ SendMsgComplete hash $ pure contract
      StatePay payor payee token value st' ->
        popState st' save (Pay payor payee token value contract)
      StateIfL cond st' ->
        pure $ pullContract' (StateIfR cond contract st') save
      StateIfR cond tru st' ->
        popState st' save (If cond tru contract)
      StateWhen timeout cases st' ->
        popState st' save (When (reverse cases) timeout contract)
      StateCase action timeout cases st' -> do
        hash <- save contract
        pure $ pullContract'
          (StateWhen timeout (MerkleizedCase action (toBuiltin $ unDatumHash hash) : cases) st')
          save
      StateLet valueId value st' ->
        popState st' save (Let valueId value contract)
      StateAssert obs st' ->
        popState st' save (Assert obs contract)

data PeerState (st :: CanPush) where
  StateRoot :: PeerState 'StRoot
  StatePay :: AccountId -> Payee -> Token -> Value Observation -> PeerState st -> PeerState ('StPay st)
  StateIfL :: Observation -> PeerState st -> PeerState ('StIfL st)
  StateIfR :: Observation -> Contract -> PeerState st -> PeerState ('StIfR st)
  StateWhen :: Timeout -> [Case Contract] -> PeerState st -> PeerState ('StWhen st)
  StateCase :: Action -> Timeout -> [Case Contract] -> PeerState st -> PeerState ('StCase st)
  StateLet :: ValueId -> Value Observation -> PeerState st -> PeerState ('StLet st)
  StateAssert :: Observation -> PeerState st -> PeerState ('StAssert st)
