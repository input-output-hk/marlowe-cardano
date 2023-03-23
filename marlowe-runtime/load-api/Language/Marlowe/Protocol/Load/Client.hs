{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Client
  where

import Data.ContractFragment (ContractFragment, ConvertedFragment(..), toFragment)
import Data.Vec (Vec(..))
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash)
import Network.TypedProtocol

newtype MarloweLoadClient m a = MarloweLoadClient { runMarloweLoadClient :: m (ClientStLoad '[ 'S 'Z ] m a) }

data ClientStLoad (state :: [N]) m a where
  Push
    :: ContractFragment n'
    -> ClientStLoad (n' ': 'S n ': state) m a
    -> ClientStLoad ('S n ': state) m a
  Pop
    :: (DatumHash -> Contract -> m (ClientStLoad (Fill state) m a))
    -> ClientStLoad ('Z ': state) m a
  Return :: m a -> ClientStLoad '[] m a

marloweLoadClientPeer :: forall m a. Functor m => MarloweLoadClient m a -> Peer MarloweLoad 'AsClient ('StLoad '[ 'S 'Z ]) m a
marloweLoadClientPeer = Effect . fmap peerLoad . runMarloweLoadClient
  where
  peerLoad :: ClientStLoad ns m a -> Peer MarloweLoad 'AsClient ('StLoad ns) m a
  peerLoad = \case
    Push fragment next ->
      Yield (ClientAgency TokLoadClient) (MsgPush fragment) $ peerLoad next
    Pop recvMsgPop ->
      Await (ServerAgency TokLoadServer) \(MsgPop hash contract) -> Effect $ peerLoad <$> recvMsgPop hash contract
    Return a -> Effect $ Done TokLoadNobody <$> a

loadContract
  :: MonadFail m
  => Contract
  -> (DatumHash -> Contract -> m ())
  -> MarloweLoadClient m DatumHash
loadContract rootContract = MarloweLoadClient . pure . loadContract' (StateCons (Cons rootContract Nil) StateNil)

loadContract'
  :: MonadFail m
  => PeerState (n ': ns)
  -> (DatumHash -> Contract -> m ())
  -> ClientStLoad (n ': ns) m DatumHash
loadContract' state handleContract = case state of
  StateCons Nil prevState ->
    Pop \hash contract -> do
      handleContract hash contract
      pure case prevState of
        StateNil -> Return $ pure hash
        StateCons Nil _ -> loadContract' prevState handleContract
        -- Important - do not force the first param to Cons, it will throw an
        -- exception.
        StateCons (Cons _ contracts) prevState' -> loadContract' (StateCons contracts prevState') handleContract
  StateCons (Cons contract contracts) prevState -> case toFragment contract of
    Nothing -> error "Merkleized contract detected"
    Just (ConvertedFragment fragment contracts') ->
      let
        nextState = StateCons contracts'
          -- Set this to error instead of contract so contract can be garbage
          -- collected (we never need to see it again)
          $ StateCons (Cons (error "already converted this contract") contracts) prevState
      in
        Push fragment $ loadContract' nextState handleContract

data PeerState (ns :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: Vec n Contract -> PeerState ns -> PeerState (n ': ns)
