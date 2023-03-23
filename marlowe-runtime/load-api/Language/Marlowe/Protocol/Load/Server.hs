{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Protocol.Load.Server
  where

import Cardano.Api (SerialiseAsRawBytes(serialiseToRawBytes), hashScriptData)
import Data.ContractFragment (PartialContract(..), convertContract, fillNextHole, partialHoles, toPartial)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.Cardano.Api (toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash(..), toDatum)
import Network.TypedProtocol

marloweLoadServerPeer
  :: Monad m
  => (DatumHash -> Contract -> m ())
  -> Peer MarloweLoad 'AsServer ('StLoad '[ 'S 'Z ]) m ()
marloweLoadServerPeer = peer' $ StateCons (Succ Zero) Root StateNil

peer'
  :: Monad m
  => PeerState ns
  -> (DatumHash -> Contract -> m ())
  -> Peer MarloweLoad 'AsServer ('StLoad ns) m ()
peer' state save = case state of
  StateNil -> Done TokLoadNobody ()
  StateCons Zero partialContract prevState -> Effect do
    let contract = convertContract partialContract
    let hash = DatumHash $ serialiseToRawBytes $ hashScriptData $ toCardanoScriptData $ toDatum contract
    save hash contract
    pure $ Yield (ServerAgency TokLoadServer) (MsgPop hash contract) case prevState of
      StateNil -> peer' prevState save
      StateCons Zero _ _  -> peer' prevState save
      StateCons (Succ n) prevPartialContract prevState' -> peer'
        (StateCons n (fillNextHole hash contract prevPartialContract) prevState')
        save
  StateCons Succ{} _ _ ->
    Await (ClientAgency TokLoadClient) \(MsgPush template) -> case toPartial template of
      partialContract' -> peer' (StateCons (partialHoles partialContract') partialContract' state) save

data PeerState (ns :: [N]) where
  StateNil :: PeerState '[]
  StateCons :: Nat n -> PartialContract n -> PeerState ns -> PeerState (n ': ns)
