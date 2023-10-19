{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The type of the query protocol.
--
-- The query protocol is a simple call-and-response one with a polymorphic request type.
module Network.Protocol.Query.Types where

import Control.Monad (join)
import Data.Binary (Binary, Get, Put, get, getWord8, put, putWord8)
import Data.Data (type (:~:) (Refl))
import Data.Foldable (fold)
import Data.Function (on)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Type.Equality (TestEquality (..))
import GHC.Generics (Generic)
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Codec.Spec (
  ArbitraryMessage (..),
  MessageEq (..),
  MessageVariations (..),
  ShowProtocol (..),
  SomePeerHasAgency (..),
  TestMessageEquality (..),
  Variations (..),
  varyAp,
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Singleton
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import Test.QuickCheck (Arbitrary (..), Gen, oneof, resize, sized)
import Test.QuickCheck.Arbitrary (genericShrink)

data SomeTag k where
  SomeTag :: Tag (t :: k) -> SomeTag k

{-# RULES
"withSingTag" forall t a. withSingTag t a = a
  #-}

class TagKind k where
  data Tag :: k -> Type
  data Request :: k -> Type
  data Response :: k -> Type
  requestTag :: Request (t :: k) -> Tag t
  fromTag :: Tag (t :: k) -> k
  toTag :: k -> SomeTag k
  withSingTag :: Tag (t :: k) -> ((SingTag t) => a) -> a

class (TagKind k) => SingTag (t :: k) where
  singTag :: Tag t

fromSomeTag :: (TagKind k) => SomeTag k -> k
fromSomeTag (SomeTag t) = fromTag t

-- | A state kind for the query protocol.
data Query (k :: Type) where
  -- | The client can send a request.
  StReq :: Query k
  -- | The server is responding to a request.
  StRes :: k -> Query k
  -- | The terminal state of the protocol.
  StDone :: Query k

instance (HasSignature k) => HasSignature (Query k) where
  signature _ = T.intercalate " " ["Query", signature $ Proxy @k]

instance Protocol (Query k) where
  -- \| The type of messages in the protocol. Corresponds to state transition in
  -- the state machine diagram of the protocol.
  data Message (Query k) from to where
    -- \| Send a request. Transitions from the req state to the res state, with the result type being determined by the
    -- request.
    MsgRequest
      :: Request (t :: k)
      -> Message (Query k) 'StReq ('StRes t)
    -- \| Respond to a request. Transitions from the res state back to the req state.
    MsgRespond
      :: Response (t :: k)
      -> Message (Query k) ('StRes t) 'StReq
    -- \| End the session
    MsgDone
      :: Message (Query k) 'StReq 'StDone

  data ClientHasAgency st where
    -- \| Client has agency in the req state.
    TokReq :: ClientHasAgency 'StReq

  data ServerHasAgency st where
    -- \| Server has agency in the res state.
    TokRes :: Tag t -> ServerHasAgency ('StRes t)

  data NobodyHasAgency st where
    -- \| Nobody has agency in the done state.
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokReq = \case {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

-- deriving instance Show (ClientHasAgency (st :: Query k))
-- deriving instance forall k (st :: Query k). (forall a. Show (Tag k a)) => Show (ServerHasAgency st)
-- deriving instance Show (NobodyHasAgency (st :: Query k))

-- deriving instance (forall x. Show (req x)) => Show (SomeRequest req)

-- | One or more tags in a tree structure.
data Tree k
  = -- | A single tag at a leaf of a tree.
    Leaf k
  | -- | A request tree consisting of two subtrees.
    Bin (Tree k) (Tree k)
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

instance (Variations k) => Variations (Tree k) where
  variations =
    (Leaf <$> variations)
      <> (Bin <$> (Leaf <$> variations) <*> (Leaf <$> variations))

instance (TagKind k) => TagKind (Tree k) where
  data Tag t where
    TagLeaf :: Tag t -> Tag ('Leaf t)
    TagBin :: Tag l -> Tag r -> Tag ('Bin l r)

  data Request t where
    ReqLeaf :: Request t -> Request ('Leaf t)
    ReqBin :: Request l -> Request r -> Request ('Bin l r)

  data Response t where
    ResLeaf :: Response t -> Response ('Leaf t)
    ResBin :: Response l -> Response r -> Response ('Bin l r)

  fromTag = \case
    TagLeaf t -> Leaf $ fromTag t
    TagBin l r -> Bin (fromTag l) (fromTag r)

  toTag = \case
    Leaf t -> case toTag t of
      SomeTag t' -> SomeTag $ TagLeaf t'
    Bin l r -> case (toTag l, toTag r) of
      (SomeTag l', SomeTag r') -> SomeTag $ TagBin l' r'

  requestTag = \case
    ReqLeaf r -> TagLeaf $ requestTag r
    ReqBin l r -> TagBin (requestTag l) (requestTag r)

  withSingTag = \case
    TagLeaf t -> withSingTag t
    TagBin l r -> withSingTag l $ withSingTag r

instance (SingTag t) => SingTag ('Leaf t) where singTag = TagLeaf singTag
instance (SingTag l, SingTag r) => SingTag ('Bin l r) where singTag = TagBin singTag singTag

instance SingClientHasAgency 'StReq where singClientHasAgency = TokReq
instance (SingTag t) => SingServerHasAgency ('StRes t) where singServerHasAgency = TokRes singTag
instance SingNobodyHasAgency 'StDone where singNobodyHasAgency = TokDone

deriving instance (forall (x :: k). Show (Tag x)) => Show (Tag (t :: Tree k))
deriving instance (forall (x :: k). Eq (Tag x)) => Eq (Tag (t :: Tree k))
deriving instance (forall (x :: k). Ord (Tag x)) => Ord (Tag (t :: Tree k))
deriving instance (forall (x :: k). Show (Request x)) => Show (Request (t :: Tree k))
deriving instance (forall (x :: k). Eq (Request x)) => Eq (Request (t :: Tree k))
deriving instance (forall (x :: k). Ord (Request x)) => Ord (Request (t :: Tree k))
deriving instance (forall (x :: k). Show (Response x)) => Show (Response (t :: Tree k))
deriving instance (forall (x :: k). Eq (Response x)) => Eq (Response (t :: Tree k))
deriving instance (forall (x :: k). Ord (Response x)) => Ord (Response (t :: Tree k))

class (Binary k, TagKind k) => BinaryTagKind k where
  putRequest :: Request (t :: k) -> Put
  getRequest :: Tag (t :: k) -> Get (Request t)
  putResponse :: Response (t :: k) -> Put
  getResponse :: Tag (t :: k) -> Get (Response t)

instance (BinaryTagKind k) => Binary (SomeTag k) where
  put = put . fromSomeTag
  get = toTag <$> get

instance (BinaryTagKind k) => BinaryTagKind (Tree k) where
  putRequest = \case
    ReqLeaf t -> putRequest t
    ReqBin l r -> putRequest l *> putRequest r
  getRequest = \case
    TagLeaf t -> ReqLeaf <$> getRequest t
    TagBin l r -> ReqBin <$> getRequest l <*> getRequest r
  putResponse = \case
    ResLeaf t -> putResponse t
    ResBin l r -> putResponse l *> putResponse r
  getResponse = \case
    TagLeaf t -> ResLeaf <$> getResponse t
    TagBin l r -> ResBin <$> getResponse l <*> getResponse r

instance (BinaryTagKind k) => BinaryMessage (Query k) where
  putMessage = \case
    ClientAgency TokReq -> \case
      MsgDone -> putWord8 0x00
      MsgRequest req -> do
        putWord8 0x01
        put $ SomeTag $ requestTag req
        putRequest req
    ServerAgency (TokRes _) -> \case
      MsgRespond a -> putResponse a

  getMessage = \case
    ClientAgency TokReq -> do
      tag <- getWord8
      case tag of
        0x00 -> pure $ SomeMessage MsgDone
        0x01 -> do
          SomeTag qtag <- get
          SomeMessage . MsgRequest <$> getRequest qtag
        _ -> fail $ "Invalid message tag " <> show tag
    ServerAgency (TokRes tag) -> SomeMessage . MsgRespond <$> getResponse tag

class (Arbitrary k, TagKind k) => ArbitraryTagKind k where
  arbitraryRequest :: Tag (t :: k) -> Gen (Request t)
  arbitraryResponse :: Tag (t :: k) -> Gen (Response t)
  shrinkRequest :: Request (t :: k) -> [Request t]
  shrinkResponse :: Response (t :: k) -> [Response t]

instance (ArbitraryTagKind k) => Arbitrary (SomeTag k) where
  arbitrary = toTag <$> arbitrary
  shrink = fmap toTag . shrink . fromSomeTag

instance (Arbitrary k) => Arbitrary (Tree k) where
  arbitrary = sized \i ->
    if i <= 1
      then Leaf <$> arbitrary
      else
        oneof
          [ resize (i `div` 2) $ Bin <$> arbitrary <*> arbitrary
          , Leaf <$> arbitrary
          ]
  shrink = genericShrink

instance (ArbitraryTagKind k) => ArbitraryTagKind (Tree k) where
  arbitraryRequest = \case
    TagLeaf tag -> ReqLeaf <$> arbitraryRequest tag
    TagBin l r -> sized \i ->
      resize (i `div` 2) $
        ReqBin
          <$> arbitraryRequest l
          <*> arbitraryRequest r
  arbitraryResponse = \case
    TagLeaf tag -> ResLeaf <$> arbitraryResponse tag
    TagBin l r -> sized \i ->
      resize (i `div` 2) $ ResBin <$> arbitraryResponse l <*> arbitraryResponse r
  shrinkRequest = \case
    ReqLeaf req -> ReqLeaf <$> shrinkRequest req
    ReqBin req reqs ->
      fold
        [ ReqBin <$> shrinkRequest req <*> pure reqs
        , ReqBin req <$> shrinkRequest reqs
        ]
  shrinkResponse = \case
    ResLeaf res -> ResLeaf <$> shrinkResponse res
    ResBin l r ->
      fold
        [ ResBin <$> shrinkResponse l <*> pure r
        , ResBin l <$> shrinkResponse r
        ]

instance (ArbitraryTagKind k) => ArbitraryMessage (Query k) where
  arbitraryMessage = do
    SomeTag tag <- arbitrary
    oneof $
      catMaybes
        [ Just $ AnyMessageAndAgency (ClientAgency TokReq) . MsgRequest <$> arbitraryRequest tag
        , Just $ AnyMessageAndAgency (ServerAgency $ TokRes tag) . MsgRespond <$> arbitraryResponse tag
        , Just $ pure $ AnyMessageAndAgency (ClientAgency TokReq) MsgDone
        ]
  shrinkMessage agency = \case
    MsgRequest query -> MsgRequest <$> shrinkRequest query
    MsgRespond result -> case agency of
      (ServerAgency (TokRes{})) -> MsgRespond <$> shrinkResponse result
    _ -> []

class (Variations k, TagKind k) => VariationsTagKind k where
  requestVariations :: Tag (t :: k) -> NonEmpty (Request t)
  responseVariations :: Tag (t :: k) -> NonEmpty (Response t)

instance (VariationsTagKind k) => Variations (SomeTag k) where
  variations = toTag <$> variations

instance (VariationsTagKind k) => VariationsTagKind (Tree k) where
  requestVariations = \case
    TagLeaf tag -> ReqLeaf <$> requestVariations tag
    TagBin l r -> ReqBin <$> requestVariations l `varyAp` requestVariations r
  responseVariations = \case
    TagLeaf tag -> ResLeaf <$> responseVariations tag
    TagBin l r -> ResBin <$> responseVariations l `varyAp` responseVariations r

instance (VariationsTagKind k) => MessageVariations (Query k) where
  agencyVariations =
    join $
      NE.fromList
        [ pure $ SomePeerHasAgency $ ClientAgency TokReq
        , do
            SomeTag tag <- variations
            pure $ SomePeerHasAgency $ ServerAgency $ TokRes tag
        ]
  messageVariations = \case
    ClientAgency TokReq -> NE.cons (SomeMessage MsgDone) do
      SomeTag tag <- variations
      SomeMessage . MsgRequest <$> requestVariations tag
    ServerAgency (TokRes tag) -> SomeMessage . MsgRespond <$> responseVariations tag

class
  ( TagKind k
  , Eq k
  , TestEquality (Tag :: k -> Type)
  , forall (t :: k). Eq (Tag t)
  , forall (t :: k). Eq (Request t)
  , forall (t :: k). Eq (Response t)
  ) =>
  EqTagKind k

instance (EqTagKind k) => Eq (SomeTag k) where
  (==) = on (==) fromSomeTag

instance (TestEquality (Tag :: k -> Type)) => TestEquality (Tag :: Tree k -> Type) where
  testEquality = \case
    TagLeaf t -> \case
      TagLeaf t' -> do
        Refl <- testEquality t t'
        pure Refl
      _ -> Nothing
    TagBin l r -> \case
      TagBin l' r' -> do
        Refl <- testEquality l l'
        Refl <- testEquality r r'
        pure Refl
      _ -> Nothing

instance (EqTagKind k) => EqTagKind (Tree k)

deriving instance (EqTagKind k) => Eq (Message (Query k) st st')
deriving instance (EqTagKind k) => Eq (ClientHasAgency (st :: Query k))
deriving instance (EqTagKind k) => Eq (ServerHasAgency (st :: Query k))
deriving instance (EqTagKind k) => Eq (NobodyHasAgency (st :: Query k))

instance TestEquality (ClientHasAgency :: Query k -> Type) where
  testEquality TokReq TokReq = Just Refl

instance (EqTagKind k) => TestEquality (ServerHasAgency :: Query k -> Type) where
  testEquality (TokRes t) (TokRes t') = do
    Refl <- testEquality t t'
    pure Refl

instance TestEquality (NobodyHasAgency :: Query k -> Type) where
  testEquality TokDone TokDone = Just Refl

instance (EqTagKind k) => TestMessageEquality (Query k) where
  testMessageEquality (ClientAgency TokReq) (ClientAgency TokReq) msg msg' = case (msg, msg') of
    (MsgRequest r, MsgRequest r') -> do
      Refl <- testEquality (requestTag r) (requestTag r')
      pure (Refl, Refl, Refl)
    (MsgRequest{}, _) -> Nothing
    (MsgDone, MsgDone) -> Just (Refl, Refl, Refl)
    (MsgDone, _) -> Nothing
  testMessageEquality (ServerAgency (TokRes t)) (ServerAgency (TokRes t')) MsgRespond{} MsgRespond{} = do
    Refl <- testEquality t t'
    pure (Refl, Refl, Refl)
  testMessageEquality _ _ _ _ = Nothing

instance (EqTagKind k) => MessageEq (Query k)

class
  ( Show k
  , TagKind k
  , forall (t :: k). Show (Tag t)
  , forall (t :: k). Show (Request t)
  , forall (t :: k). Show (Response t)
  ) =>
  ShowTagKind k

deriving instance (ShowTagKind k) => Show (Message (Query k) st st')
deriving instance (ShowTagKind k) => Show (ClientHasAgency (st :: Query k))
deriving instance (ShowTagKind k) => Show (ServerHasAgency (st :: Query k))
deriving instance (ShowTagKind k) => Show (NobodyHasAgency (st :: Query k))

instance (ShowTagKind k) => ShowProtocol (Query k)
