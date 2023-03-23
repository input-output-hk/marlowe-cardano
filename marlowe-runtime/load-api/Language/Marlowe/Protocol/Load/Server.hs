{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Server
  where

import Cardano.Api (SerialiseAsRawBytes(serialiseToRawBytes), hashScriptData)
import Data.ContractFragment
  (ContractFragment, PartialContract(..), convertContract, fillNextHole, partialHoles, toPartial)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.Cardano.Api (toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Network.TypedProtocol

newtype MarloweLoadServer m a = MarloweLoadServer { runMarloweLoadServer :: m (ServerStLoad '[ 'S 'Z ] m a) }
  deriving (Functor)

data ServerStLoad (state :: [N]) m a where
  Push
    :: (forall n'. ContractFragment n' -> ServerStLoad (n' ': 'S n ': state) m a)
    -> ServerStLoad ('S n ': state) m a
  Pop
    :: DatumHash
    -> Contract
    -> m (ServerStLoad (Fill state) m a)
    -> ServerStLoad ('Z ': state) m a
  Return :: a -> ServerStLoad '[] m a

deriving instance Functor m => Functor (ServerStLoad state m)

hoistMarloweLoadServer
  :: Functor m
  => (forall x. m x -> n x)
  -> MarloweLoadServer m a
  -> MarloweLoadServer n a
hoistMarloweLoadServer f = MarloweLoadServer . f . fmap (hoistServerStLoad f) . runMarloweLoadServer

hoistServerStLoad
  :: Functor m
  => (forall x. m x -> n x)
  -> ServerStLoad state m a
  -> ServerStLoad state n a
hoistServerStLoad f = \case
  Push recvMsgPush -> Push $ hoistServerStLoad f . recvMsgPush
  Pop hash contract next -> Pop hash contract $ f $ hoistServerStLoad f <$> next
  Return a -> Return a

marloweLoadServerPeer
  :: forall m a
   . Functor m
  => MarloweLoadServer m a
  -> Peer MarloweLoad 'AsServer ('StLoad '[ 'S 'Z ]) m a
marloweLoadServerPeer = Effect . fmap peerLoad . runMarloweLoadServer
  where
  peerLoad :: ServerStLoad state m a -> Peer MarloweLoad 'AsServer ('StLoad state) m a
  peerLoad = \case
    Push recvMsgPush ->
      Await (ClientAgency TokLoadClient) \(MsgPush fragment) -> peerLoad $ recvMsgPush fragment
    Pop hash contract next ->
      Effect $ Yield (ServerAgency TokLoadServer) (MsgPop hash contract) . peerLoad <$> next
    Return a -> Done TokLoadNobody a

loadContractServer
  :: Monad m
  => (DatumHash -> Contract -> m ())
  -> MarloweLoadServer m (DatumHash, Contract)
loadContractServer = MarloweLoadServer . pure . loadContractServer' (StateCons (Succ Zero) Root StateNil)

loadContractServer'
  :: Monad m
  => PeerState (n ': state)
  -> (DatumHash -> Contract -> m ())
  -> ServerStLoad (n ': state) m (DatumHash, Contract)
loadContractServer' state save = case state of
  StateCons Zero partialContract prevState ->
    let
      contract = convertContract partialContract
      hash = DatumHash $ serialiseToRawBytes $ hashScriptData $ toCardanoScriptData $ toDatum contract
    in
      Pop hash contract do
        save hash contract
        pure case prevState of
          StateNil -> Return (hash, contract)
          StateCons Zero _ _  -> loadContractServer' prevState save
          StateCons (Succ n) prevPartialContract prevState' -> loadContractServer'
            (StateCons n (fillNextHole hash contract prevPartialContract) prevState')
            save
  StateCons Succ{} _ _ -> Push \template -> case toPartial template of
      partialContract' -> loadContractServer' (StateCons (partialHoles partialContract') partialContract' state) save

data PeerState (state :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: Nat n -> PartialContract n -> PeerState state -> PeerState (n ': state)
