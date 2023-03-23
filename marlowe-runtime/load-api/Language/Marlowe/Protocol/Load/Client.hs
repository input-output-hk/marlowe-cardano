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

-- | Load a contract into the runtime in a space-efficient way. Note: this
-- relies on the input contract being lazily evaluated and garbage collected as
-- it is processed. This means that the caller of this function should avoid holding
-- onto a reference to the input contract (e.g. in a let-binding)
loadContractClient
  :: MonadFail m
  -- The unmerkleized contract to load.
  => Contract
  -- A callback to handle merkleized contracts and hashes sent back by the server.
  -> (DatumHash -> Contract -> m ())
  -- A client that loads the entire contract into the runtime, returning a hash
  -- of the merkleized contract and the merkleized version of the input
  -- contract.
  -> MarloweLoadClient m (DatumHash, Contract)
loadContractClient rootContract = MarloweLoadClient
  . pure
  . loadContractClient' (StateCons (QueueOne (toFragment rootContract) Nil) StateNil)

loadContractClient'
  :: MonadFail m
  => PeerState (n ': state)
  -> (DatumHash -> Contract -> m ())
  -> ClientStLoad (n ': state) m (DatumHash, Contract)
loadContractClient' state handleContract = case state of
  StateCons QueueNil prevState ->
    Pop \hash contract -> do
      handleContract hash contract
      pure case prevState of
        StateNil -> Return (hash, contract)
        StateCons QueueNil _ -> loadContractClient' prevState handleContract
        StateCons (QueueOne _ contracts) prevState' -> case contracts of
          Nil -> loadContractClient' (StateCons QueueNil prevState') handleContract
          Cons contract' contracts' -> loadContractClient' (StateCons (QueueOne (toFragment contract') contracts') prevState') handleContract
  StateCons (QueueOne contract contracts) prevState -> case contract of
    Nothing -> error "Merkleized contract detected"
    Just (ConvertedFragment fragment contracts') ->
      let
        nextQueue = case contracts' of
          Nil -> QueueNil
          Cons contract' contracts'' -> QueueOne (toFragment contract') contracts''
        nextState = StateCons nextQueue $ StateCons (QueueOne contract contracts) prevState
      in
        Push fragment $ loadContractClient' nextState handleContract

data PeerState (state :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: ContractQueue n -> PeerState state -> PeerState (n ': state)

data ContractQueue (state :: N) where
  QueueNil :: ContractQueue 'Z
  QueueOne :: Maybe ConvertedFragment -> Vec n Contract -> ContractQueue ('S n)
