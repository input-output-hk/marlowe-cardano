{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Protocol.LoadSpec where

import Cardano.Api (hashScriptData)
import Control.Monad (join)
import Control.Monad.Trans.Writer
import qualified Data.Map as Map
import Data.Void (Void, absurd)
import Language.Marlowe.Core.V1.Merkle (Continuations, deepMerkleize)
import Language.Marlowe.Core.V1.Semantics.Types (Contract (Close))
import Language.Marlowe.Protocol.Load.Client (marloweLoadClientPeer, pushContract)
import Language.Marlowe.Protocol.Load.Server (marloweLoadServerPeer, pullContract)
import Language.Marlowe.Protocol.Load.Types
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoDatumHash, toCardanoScriptData)
import Language.Marlowe.Runtime.ChainSync.Api (DatumHash (unDatumHash), toDatum)
import Language.Marlowe.Runtime.ChainSync.Gen (StructureType (..), oneofStructured, resized)
import Network.Protocol.Codec.Spec
import Network.Protocol.Handshake.Types (Handshake)
import Network.Protocol.Peer.Trace (peerTracedToPeer)
import Network.TypedProtocol
import Network.TypedProtocol.Codec
import qualified Plutus.V2.Ledger.Api as PV2
import Spec.Marlowe.Semantics.Arbitrary ()
import Test.Hspec (Spec, describe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck hiding (shrinkMap)

spec :: Spec
spec = do
  describe "MarloweLoad protocol" do
    prop "Has a lawful codec" $ checkPropCodec @(Handshake MarloweLoad)
    codecGoldenTests @MarloweLoad "MarloweLoad"
    prop "Merkleizes contract correctly" \contract ->
      merkleizeWithPeers contract === (Just <$> merkleizeWithLibrary contract)

merkleizeWithLibrary :: Contract -> Writer Continuations DatumHash
merkleizeWithLibrary = mapWriter (fmap removeClose . addRoot) . deepMerkleize
  where
    -- deepMerkleize doesn't add the root contract, but the peers do.
    addRoot (root, continuations) = (rootHash, Map.insert (PV2.DatumHash $ PV2.toBuiltin $ unDatumHash rootHash) root continuations)
      where
        rootHash = hashContract root

    -- deepMerkleize adds close to the continuations, but the peers don't (it
    -- is statically known).
    removeClose = Map.delete $ PV2.DatumHash $ PV2.toBuiltin $ unDatumHash $ hashContract Close

hashContract :: Contract -> DatumHash
hashContract = fromCardanoDatumHash . hashScriptData . toCardanoScriptData . toDatum

merkleizeWithPeers :: Contract -> Writer Continuations (Maybe DatumHash)
merkleizeWithPeers contract =
  go
    (peerTracedToPeer $ marloweLoadClientPeer $ pushContract contract)
    (peerTracedToPeer $ marloweLoadServerPeer $ pullContract (unsafeIntToNat 100) save (pure ()) $ pure ())
  where
    go
      :: Peer MarloweLoad 'AsClient st (Writer Continuations) (Maybe DatumHash)
      -> Peer MarloweLoad 'AsServer st (Writer Continuations) (Maybe Contract)
      -> Writer Continuations (Maybe DatumHash)
    go c s = case (c, s) of
      (Effect mc, Effect ms) -> join $ go <$> mc <*> ms
      (Effect mc, _) -> flip go s =<< mc
      (_, Effect ms) -> go c =<< ms
      (Done _ hash, Done _ _) -> pure hash
      (Done tok _, Yield tok' _ _) -> absurd $ nobodyAndSomebodyHaveAgency tok tok'
      (Done tok _, Await tok' _) -> absurd $ nobodyAndSomebodyHaveAgency tok tok'
      (Yield tok' _ _, Done tok _) -> absurd $ nobodyAndSomebodyHaveAgency tok tok'
      (Await tok' _, Done tok _) -> absurd $ nobodyAndSomebodyHaveAgency tok tok'
      (Yield _ msg c', Await _ k) -> go c' $ k msg
      (Await _ k, Yield _ msg s') -> go (k msg) s'
      (Await (ServerAgency tok) _, Await (ClientAgency tok') _) -> absurd $ exclusionLemma_ClientAndServerHaveAgency tok' tok
      (Yield (ClientAgency tok) _ _, Yield (ServerAgency tok') _ _g) -> absurd $ exclusionLemma_ClientAndServerHaveAgency tok tok'

    save :: Contract -> Writer Continuations DatumHash
    save c = do
      let hash = hashContract c
      tell $ Map.singleton (PV2.DatumHash $ PV2.toBuiltin $ unDatumHash hash) c
      pure hash

nobodyAndSomebodyHaveAgency :: forall (st :: MarloweLoad) pr. NobodyHasAgency st -> PeerHasAgency pr st -> Void
nobodyAndSomebodyHaveAgency tok = \case
  ClientAgency tok' -> exclusionLemma_NobodyAndClientHaveAgency tok tok'
  ServerAgency tok' -> exclusionLemma_NobodyAndServerHaveAgency tok tok'

instance ArbitraryMessage MarloweLoad where
  arbitraryMessage =
    oneof
      [ withArbitraryNodeAndNat \n node ->
          pure $ AnyMessageAndAgency (ServerAgency $ TokProcessing node) $ MsgResume $ Succ n
      , withArbitraryNodeAndNat \n node ->
          pure $ AnyMessageAndAgency (ClientAgency $ TokCanPush (Succ n) node) MsgPushClose
      , withArbitraryNodeAndNat \n node ->
          AnyMessageAndAgency (ClientAgency $ TokCanPush (Succ n) node) <$> do
            accountId <- arbitrary
            payee <- arbitrary
            token <- arbitrary
            value <- arbitrary
            pure $ MsgPushPay accountId payee token value
      , withArbitraryNodeAndNat \n node ->
          AnyMessageAndAgency (ClientAgency $ TokCanPush (Succ n) node) <$> do
            obs <- arbitrary
            pure $ MsgPushIf obs
      , withArbitraryNodeAndNat \n node ->
          AnyMessageAndAgency (ClientAgency $ TokCanPush (Succ n) node) <$> do
            timeout <- arbitrary
            pure $ MsgPushWhen timeout
      , withArbitraryNodeAndNat \n node ->
          AnyMessageAndAgency (ClientAgency $ TokCanPush (Succ n) $ SWhenNode node) <$> do
            action <- arbitrary
            pure $ MsgPushCase action
      , withArbitraryNodeAndNat \n node ->
          AnyMessageAndAgency (ClientAgency $ TokCanPush (Succ n) node) <$> do
            valueId <- arbitrary
            value <- arbitrary
            pure $ MsgPushLet valueId value
      , withArbitraryNodeAndNat \_ node ->
          pure $ AnyMessageAndAgency (ClientAgency $ TokCanPush Zero node) MsgRequestResume
      , withArbitraryNodeAndNat \n node ->
          pure $ AnyMessageAndAgency (ClientAgency $ TokCanPush n node) MsgAbort
      , AnyMessageAndAgency (ServerAgency TokComplete) . MsgComplete <$> arbitrary
      ]
    where
      withArbitraryNodeAndNat :: (forall n node. Nat n -> SNode node -> Gen a) -> Gen a
      withArbitraryNodeAndNat f = join $ f' <$> arbitrary <*> arbitrary
        where
          f' (SomeNat n) (SomeSNode node) = f n node

  shrinkMessage _ = \case
    MsgAbort -> []
    MsgRequestResume -> []
    MsgResume _ -> []
    MsgPushClose -> []
    MsgPushPay accountId payee token value -> MsgPushPay accountId payee token <$> shrink value
    MsgPushIf obs -> MsgPushIf <$> shrink obs
    MsgPushWhen _ -> []
    MsgPushCase action -> MsgPushCase <$> shrink action
    MsgPushLet valueId value -> MsgPushLet valueId <$> shrink value
    MsgPushAssert obs -> MsgPushAssert <$> shrink obs
    MsgComplete _ -> []

data SomeNat = forall n. SomeNat (Nat n)
data SomeSNode = forall node. SomeSNode (SNode node)

instance Arbitrary SomeNat where
  arbitrary = SomeNat . unsafeIntToNat <$> chooseInt (0, 100)
  shrink (SomeNat Zero) = []
  shrink (SomeNat (Succ n)) = [SomeNat n]

instance Arbitrary SomeSNode where
  arbitrary =
    oneofStructured
      [ (Leaf, pure $ SomeSNode SRootNode)
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SPayNode parent
        )
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SIfLNode parent
        )
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SIfRNode parent
        )
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SWhenNode parent
        )
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SCaseNode parent
        )
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SLetNode parent
        )
      ,
        ( Node
        , do
            SomeSNode parent <- resized (`div` 2) arbitrary
            pure $ SomeSNode $ SAssertNode parent
        )
      ]
  shrink (SomeSNode node) = case node of
    SRootNode -> []
    SPayNode parent -> [SomeSNode parent]
    SIfLNode parent -> [SomeSNode parent]
    SIfRNode parent -> [SomeSNode parent]
    SWhenNode parent -> [SomeSNode parent]
    SCaseNode parent -> [SomeSNode parent]
    SLetNode parent -> [SomeSNode parent]
    SAssertNode parent -> [SomeSNode parent]
