{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
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
  deriving (Functor)

data ClientStLoad (state :: [N]) m a where
  Push
    :: ContractFragment n'
    -> ClientStLoad (n' ': 'S n ': state) m a
    -> ClientStLoad ('S n ': state) m a
  Pop
    :: (DatumHash -> Contract -> m (ClientStLoad (Fill state) m a))
    -> ClientStLoad ('Z ': state) m a
  Return :: a -> ClientStLoad '[] m a

deriving instance Functor m => Functor (ClientStLoad state m)

hoistMarloweLoadClient
  :: Functor m
  => (forall x. m x -> n x)
  -> MarloweLoadClient m a
  -> MarloweLoadClient n a
hoistMarloweLoadClient f = MarloweLoadClient . f . fmap (hoistClientStLoad f) . runMarloweLoadClient

hoistClientStLoad
  :: Functor m
  => (forall x. m x -> n x)
  -> ClientStLoad state m a
  -> ClientStLoad state n a
hoistClientStLoad f = \case
  Push fragment next -> Push fragment $ hoistClientStLoad f next
  Pop recvMsgPop -> Pop $ fmap (f . fmap (hoistClientStLoad f)) . recvMsgPop
  Return a -> Return a

marloweLoadClientPeer
  :: forall m a
   . Functor m
  => MarloweLoadClient m a
  -> Peer MarloweLoad 'AsClient ('StLoad '[ 'S 'Z ]) m a
marloweLoadClientPeer = Effect . fmap peerLoad . runMarloweLoadClient
  where
  peerLoad :: ClientStLoad state m a -> Peer MarloweLoad 'AsClient ('StLoad state) m a
  peerLoad = \case
    Push fragment next ->
      Yield (ClientAgency TokLoadClient) (MsgPush fragment) $ peerLoad next
    Pop recvMsgPop ->
      Await (ServerAgency TokLoadServer) \(MsgPop hash contract) -> Effect $ peerLoad <$> recvMsgPop hash contract
    Return a -> Done TokLoadNobody a

loadContractClient
  :: MonadFail m
  => Contract
  -> (DatumHash -> Contract -> m ())
  -> MarloweLoadClient m (DatumHash, Contract)
loadContractClient rootContract = MarloweLoadClient . pure . loadContractClient' (StateCons (Cons rootContract Nil) StateNil)

loadContractClient'
  :: MonadFail m
  => PeerState (n ': state)
  -> (DatumHash -> Contract -> m ())
  -> ClientStLoad (n ': state) m (DatumHash, Contract)
loadContractClient' state handleContract = case state of
  StateCons Nil prevState ->
    Pop \hash contract -> do
      handleContract hash contract
      pure case prevState of
        StateNil -> Return (hash, contract)
        StateCons Nil _ -> loadContractClient' prevState handleContract
        -- Important - do not force the first param to Cons, it will throw an
        -- exception.
        StateCons (Cons _ contracts) prevState' -> loadContractClient' (StateCons contracts prevState') handleContract
  StateCons (Cons contract contracts) prevState -> case toFragment contract of
    Nothing -> error "Merkleized contract detected"
    Just (ConvertedFragment fragment contracts') ->
      let
        nextState = StateCons contracts'
          -- Set this to error instead of contract so contract can be garbage
          -- collected (we never need to see it again)
          $ StateCons (Cons (error "already converted this contract") contracts) prevState
      in
        Push fragment $ loadContractClient' nextState handleContract

data PeerState (state :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: Vec n Contract -> PeerState state -> PeerState (n ': state)
