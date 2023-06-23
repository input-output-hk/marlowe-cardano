{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The type of the query protocol.
--
-- The query protocol is a simple call-and-response one with a polymorphic request type.
module Network.Protocol.Query.Types where

import Control.Monad (join)
import Data.Binary (Binary, Get, Put, get, getWord8, put, putWord8)
import Data.Data (type (:~:) (Refl))
import Data.Foldable (fold)
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Show (showCommaSpace, showSpace)
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Codec.Spec (
  ArbitraryMessage (..),
  MessageEq (..),
  MessageVariations (..),
  ShowProtocol (..),
  SomePeerHasAgency (..),
  varyAp,
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Peer.Trace
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import OpenTelemetry.Attributes (toPrimitiveAttribute)
import Test.QuickCheck (Gen, oneof, resize, sized)

-- | A state kind for the query protocol.
data Query (req :: Type -> Type) where
  -- | The client can send a request.
  StReq :: Query req
  -- | The server is responding to a request.
  StRes :: Type -> Query req
  -- | The terminal state of the protocol.
  StDone :: Query req

instance (HasSignature req) => HasSignature (Query req) where
  signature _ = T.intercalate " " ["Query", signature $ Proxy @req]

instance Protocol (Query req) where
  -- \| The type of messages in the protocol. Corresponds to state transition in
  -- the state machine diagram of the protocol.
  data Message (Query req) from to where
    -- \| Send a request. Transitions from the req state to the res state, with the result type being determined by the
    -- request.
    MsgRequest
      :: ReqTree req a
      -> Message
          (Query req)
          'StReq
          ('StRes a)
    -- \| Respond to a request. Transitions from the res state back to the req state.
    MsgRespond
      :: a
      -> Message
          (Query req)
          ('StRes a)
          'StReq
    -- \| End the session
    MsgDone
      :: Message
          (Query query)
          'StReq
          'StDone

  data ClientHasAgency st where
    -- \| Client has agency in the req state.
    TokReq :: ClientHasAgency 'StReq

  data ServerHasAgency st where
    -- \| Server has agency in the res state.
    TokRes :: Tag (ReqTree req) a -> ServerHasAgency ('StRes a :: Query req)

  data NobodyHasAgency st where
    -- \| Nobody has agency in the done state.
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokReq = \case {}
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

deriving instance Show (ClientHasAgency (st :: Query req))
deriving instance forall req (st :: Query req). (forall a. Show (Tag req a)) => Show (ServerHasAgency st)
deriving instance Show (NobodyHasAgency (st :: Query req))

-- | Basic class for requests.
class Request (req :: * -> *) where
  -- | A data family that represents which request was issued, but lacks the data associated with the request.
  data Tag req :: * -> *

  -- | Given a request, provide a tag for the request.
  tagFromReq :: req a -> Tag req a

  -- | Propositional equality for request tags.
  tagEq :: Tag req a -> Tag req b -> Maybe (a :~: b)

-- | Existential version of a tag.
data SomeTag req = forall a. SomeTag (Tag req a)

-- | Existential version of a request.
data SomeRequest req where
  SomeRequest :: req a -> SomeRequest req

deriving instance (forall x. Show (req x)) => Show (SomeRequest req)

-- | One or more requests in a tree structure.
data ReqTree req a where
  -- | A single request at a leaf of a tree.
  ReqLeaf :: req a -> ReqTree req a
  -- | A request tree consisting of two subtrees.
  ReqBin :: ReqTree req a -> ReqTree req b -> ReqTree req (a, b)

deriving instance (forall x. Show (req x)) => Show (ReqTree req a)
deriving instance (forall x. Eq (req x)) => Eq (ReqTree req a)

instance (Request req) => Request (ReqTree req) where
  data Tag (ReqTree req) a where
    TagLeaf :: Tag req a -> Tag (ReqTree req) a
    TagBin :: Tag (ReqTree req) a -> Tag (ReqTree req) b -> Tag (ReqTree req) (a, b)
  tagFromReq = \case
    ReqLeaf req -> TagLeaf $ tagFromReq req
    ReqBin req reqs -> TagBin (tagFromReq req) $ tagFromReq reqs
  tagEq = \case
    TagLeaf req -> \case
      TagLeaf req' -> case tagEq req req' of
        Just Refl -> Just Refl
        _ -> Nothing
      _ -> Nothing
    TagBin req reqs -> \case
      TagBin req' reqs' -> case (tagEq req req', tagEq reqs reqs') of
        (Just Refl, Just Refl) -> Just Refl
        _ -> Nothing
      _ -> Nothing

deriving instance (forall x. Show (Tag req x)) => Show (Tag (ReqTree req) a)

class (Request req) => BinaryRequest (req :: * -> *) where
  putReq :: req a -> Put
  getReq :: Get (SomeRequest req)
  putResult :: Tag req a -> a -> Put
  getResult :: Tag req a -> Get a

instance (BinaryRequest req) => BinaryRequest (ReqTree req) where
  putReq = \case
    ReqLeaf req -> do
      put False
      putReq req
    ReqBin l r -> do
      put True
      putReq l
      putReq r
  getReq = do
    isBin <- get
    if isBin
      then do
        SomeRequest l <- getReq
        SomeRequest r <- getReq
        pure $ SomeRequest $ ReqBin l r
      else do
        SomeRequest req <- getReq
        pure $ SomeRequest $ ReqLeaf req

  putResult = \case
    TagLeaf tag -> putResult tag
    TagBin l r -> \(a, b) -> do
      putResult l a
      putResult r b
  getResult = \case
    TagLeaf tag -> getResult tag
    TagBin l r -> (,) <$> getResult l <*> getResult r

foldPutRequestList :: (BinaryRequest req) => ReqTree req a -> Put
foldPutRequestList = \case
  ReqLeaf req -> putReq req
  ReqBin req reqs -> putReq req *> foldPutRequestList reqs

-- reqAppend :: ReqTree req a -> ReqTree req b -> ReqTree req (a, b)
-- reqAppend acc = \case
--   ReqLeaf _ -> acc
--   ReqBin _ reqs -> reqLength (acc + 1) reqs

instance (BinaryRequest req) => Binary (SomeRequest req) where
  put (SomeRequest req) = putReq req
  get = getReq

instance (BinaryRequest req) => BinaryMessage (Query req) where
  putMessage = \case
    ClientAgency TokReq -> \case
      MsgDone -> putWord8 0x00
      MsgRequest req -> do
        putWord8 0x01
        put $ SomeRequest req
    ServerAgency (TokRes tag) -> \case
      MsgRespond a -> putResult tag a

  getMessage = \case
    ClientAgency TokReq -> do
      tag <- getWord8
      case tag of
        0x00 -> pure $ SomeMessage MsgDone
        0x01 -> do
          SomeRequest req <- get
          pure $ SomeMessage $ MsgRequest req
        _ -> fail $ "Invalid message tag " <> show tag
    ServerAgency (TokRes tag) -> SomeMessage . MsgRespond <$> getResult tag

class (ShowRequest req) => OTelRequest req where
  reqTypeName :: Proxy req -> Text
  reqName :: Tag req a -> Text

instance (OTelRequest req) => OTelRequest (ReqTree req) where
  reqTypeName _ = reqTypeName (Proxy @req)
  reqName = \case
    TagLeaf tag -> reqName tag
    TagBin l r -> "(" <> reqName l <> " AND " <> reqName r <> ")"

instance (OTelRequest req) => OTelProtocol (Query req) where
  protocolName _ = "query." <> reqTypeName (Proxy @req)
  messageAttributes = curry \case
    (_, MsgRequest req) ->
      MessageAttributes
        { messageType = "request/" <> reqName (tagFromReq req)
        , messageParameters = [toPrimitiveAttribute $ T.pack $ show req]
        }
    (ServerAgency (TokRes tag), MsgRespond a) ->
      MessageAttributes
        { messageType = "request/" <> reqName tag <> "/respond"
        , messageParameters = toPrimitiveAttribute . T.pack <$> [showsPrecResult 0 tag a ""]
        }
    (_, MsgDone) ->
      MessageAttributes
        { messageType = "done"
        , messageParameters = []
        }

class (Request req) => ArbitraryRequest req where
  arbitraryTag :: Gen (SomeTag req)
  arbitraryReq :: Tag req a -> Gen (req a)
  arbitraryResult :: Tag req a -> Gen a
  shrinkReq :: req a -> [req a]
  shrinkResult :: Tag req a -> a -> [a]

instance (ArbitraryRequest req) => ArbitraryRequest (ReqTree req) where
  arbitraryTag = sized \i ->
    if i <= 1
      then do
        SomeTag tag <- arbitraryTag
        pure $ SomeTag $ TagLeaf tag
      else
        oneof
          [ resize (i `div` 2) do
              SomeTag l <- arbitraryTag
              SomeTag r <- arbitraryTag
              pure $ SomeTag $ TagBin l r
          , do
              SomeTag tag <- arbitraryTag
              pure $ SomeTag $ TagLeaf tag
          ]
  arbitraryReq = \case
    TagLeaf tag -> ReqLeaf <$> arbitraryReq tag
    TagBin l r -> sized \i ->
      resize (i `div` 2) $
        ReqBin
          <$> arbitraryReq l
          <*> arbitraryReq r
  arbitraryResult = \case
    TagLeaf tag -> arbitraryResult tag
    TagBin l r -> sized \i ->
      resize (i `div` 2) $
        (,)
          <$> arbitraryResult l
          <*> arbitraryResult r
  shrinkReq = \case
    ReqLeaf req -> ReqLeaf <$> shrinkReq req
    ReqBin req reqs ->
      fold
        [ ReqBin <$> shrinkReq req <*> pure reqs
        , ReqBin req <$> shrinkReq reqs
        ]
  shrinkResult = \case
    TagLeaf tag -> shrinkResult tag
    TagBin l r -> \(a, b) ->
      fold
        [ (,) <$> shrinkResult l a <*> pure b
        , (,) a <$> shrinkResult r b
        ]

class (Request req) => RequestVariations req where
  tagVariations :: NonEmpty (SomeTag req)
  requestVariations :: Tag req a -> NonEmpty (req a)
  resultVariations :: Tag req a -> NonEmpty a

instance (RequestVariations req) => RequestVariations (ReqTree req) where
  tagVariations =
    NE.cons
      (NE.head [SomeTag $ TagBin (TagLeaf tag) (TagLeaf tag) | SomeTag tag <- tagVariations])
      [SomeTag $ TagLeaf tag | SomeTag tag <- tagVariations]
  requestVariations = \case
    TagLeaf tag -> ReqLeaf <$> requestVariations tag
    TagBin l r -> ReqBin <$> requestVariations l `varyAp` requestVariations r
  resultVariations = \case
    TagLeaf tag -> resultVariations tag
    TagBin l r -> (,) <$> resultVariations l `varyAp` resultVariations r

instance (ArbitraryRequest req) => ArbitraryMessage (Query req) where
  arbitraryMessage = do
    SomeTag tag <- arbitraryTag
    oneof $
      catMaybes
        [ Just $ AnyMessageAndAgency (ClientAgency TokReq) . MsgRequest <$> arbitraryReq tag
        , Just $ AnyMessageAndAgency (ServerAgency $ TokRes tag) . MsgRespond <$> arbitraryResult tag
        , Just $ pure $ AnyMessageAndAgency (ClientAgency TokReq) MsgDone
        ]
  shrinkMessage agency = \case
    MsgRequest query -> MsgRequest <$> shrinkReq query
    MsgRespond result -> case agency of
      (ServerAgency (TokRes tag)) -> MsgRespond <$> shrinkResult tag result
    _ -> []

instance (RequestVariations req) => MessageVariations (Query req) where
  agencyVariations =
    join $
      NE.fromList
        [ pure $ SomePeerHasAgency $ ClientAgency TokReq
        , do
            SomeTag tag <- tagVariations
            pure $ SomePeerHasAgency $ ServerAgency $ TokRes tag
        ]
  messageVariations = \case
    ClientAgency TokReq -> NE.cons (SomeMessage MsgDone) do
      SomeTag tag <- tagVariations
      SomeMessage . MsgRequest <$> requestVariations tag
    ServerAgency (TokRes tag) -> SomeMessage . MsgRespond <$> resultVariations tag

class (forall a. Eq (req a), Request req) => RequestEq req where
  resultEq :: Tag req a -> a -> a -> Bool

instance (RequestEq req) => RequestEq (ReqTree req) where
  resultEq = \case
    TagLeaf tag -> resultEq tag
    TagBin l r -> \(a, b) (a', b') -> resultEq l a a' && resultEq r b b'

instance (RequestEq req) => MessageEq (Query req) where
  messageEq (AnyMessageAndAgency agency msg) = case (agency, msg) of
    (_, MsgRequest req) -> \case
      AnyMessageAndAgency _ (MsgRequest req') ->
        case tagEq (tagFromReq req) (tagFromReq req') of
          Just Refl -> req == req'
          Nothing -> False
      _ -> False
    (ServerAgency (TokRes tag), MsgRespond a) -> \case
      AnyMessageAndAgency (ServerAgency (TokRes tag')) (MsgRespond a') ->
        case tagEq tag tag' of
          Just Refl -> resultEq tag a a'
          Nothing -> False
      _ -> False
    (_, MsgDone) -> \case
      AnyMessageAndAgency _ MsgDone -> True
      _ -> False

class (forall a. Show (req a), forall a. Show (Tag req a), Request req) => ShowRequest req where
  showsPrecResult :: Int -> Tag req a -> a -> ShowS

instance (ShowRequest req) => ShowRequest (ReqTree req) where
  showsPrecResult p = \case
    TagLeaf tag -> showsPrecResult p tag
    TagBin l r -> \(a, b) ->
      showParen
        True
        ( showsPrecResult 0 l a
            . showCommaSpace
            . showsPrecResult 0 r b
        )

instance (ShowRequest req) => ShowProtocol (Query req) where
  showsPrecMessage p agency = \case
    MsgRequest query ->
      showParen
        (p >= 11)
        ( showString "MsgRequest"
            . showSpace
            . showsPrec 11 query
        )
    MsgRespond a ->
      showParen
        (p >= 11)
        ( showString "MsgRespond"
            . showSpace
            . case agency of ServerAgency (TokRes tag) -> showsPrecResult 11 tag a
        )
    MsgDone -> showString "MsgDone"

  showsPrecServerHasAgency = showsPrec
  showsPrecClientHasAgency = showsPrec
