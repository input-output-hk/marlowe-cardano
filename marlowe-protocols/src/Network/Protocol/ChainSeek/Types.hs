{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The type of the chain sync protocol.
module Network.Protocol.ChainSeek.Types where

import Control.Monad (join, replicateM)
import Data.Binary (Binary (..), Get, Put, getWord8, putWord8)
import Data.Data (Proxy (..), type (:~:) (Refl))
import Data.Foldable (fold, for_)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import qualified Data.Text as T
import Data.Type.Equality (TestEquality (testEquality))
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Codec.Spec (
  ArbitraryMessage (..),
  MessageEq (..),
  MessageVariations (..),
  ShowProtocol (..),
  SomePeerHasAgency (..),
  TestMessageEquality (..),
  Variations (..),
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Singleton
import Network.TypedProtocol (PeerHasAgency (..), Protocol (..), SomeMessage (SomeMessage))
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import Test.QuickCheck (Arbitrary, Gen, arbitrary, listOf, oneof, shrink, shrinkList)

data SomeTag k where
  SomeTag :: Tag (t :: k) -> SomeTag k

class TagKind k where
  data Tag :: k -> Type
  data Move :: k -> Type
  data SeekResult :: k -> Type
  data SeekError :: k -> Type
  withSingTag :: Tag (t :: k) -> ((SingTag t) => a) -> a
  moveTag :: Move (t :: k) -> Tag t
  fromTag :: Tag (t :: k) -> k
  toTag :: k -> SomeTag k

class (TagKind k) => SingTag (t :: k) where
  singTag :: Tag t

fromSomeTag :: (TagKind k) => SomeTag k -> k
fromSomeTag (SomeTag t) = fromTag t

-- | The type of states in the protocol.
data ChainSeek (k :: Type) point tip where
  -- | The client and server are idle. The client can send a request.
  StIdle :: ChainSeek k point tip
  -- | The client has sent a next update request. The client is now waiting for
  -- a response, and the server is preparing to send a response. The server can
  -- respond immediately or it can send a 'Wait' message followed by a response
  -- at some point in the future.
  StNext :: k -> ChainSeek k point tip
  -- | The client has initiated a scan, which is equivalent to fixing the query and repeatedly collecting the next block
  -- that satisfies it.
  StScan :: k -> ChainSeek k point tip
  -- | The client has requested a batch of blocks from the server from a scan.
  StCollect :: k -> ChainSeek k point tip
  -- | The server has sent a ping to the client to determine if it is still
  -- connected and is waiting for a pong.
  StPoll :: k -> ChainSeek k point tip
  -- | The terminal state of the protocol.
  StDone :: ChainSeek k point tip

instance SingClientHasAgency 'StIdle where singClientHasAgency = TokIdle
instance (SingTag t) => SingClientHasAgency ('StScan t) where singClientHasAgency = TokScan singTag
instance (SingTag t) => SingClientHasAgency ('StPoll t) where singClientHasAgency = TokPoll singTag
instance (SingTag t) => SingServerHasAgency ('StNext t) where singServerHasAgency = TokNext singTag
instance (SingTag t) => SingServerHasAgency ('StCollect t) where singServerHasAgency = TokCollect singTag
instance SingNobodyHasAgency 'StDone where singNobodyHasAgency = TokDone

instance
  ( HasSignature k
  , HasSignature point
  , HasSignature tip
  )
  => HasSignature (ChainSeek k point tip)
  where
  signature _ =
    T.intercalate
      " "
      [ "ChainSeek"
      , signature $ Proxy @k
      , signature $ Proxy @point
      , signature $ Proxy @tip
      ]

instance Protocol (ChainSeek k point tip) where
  -- \| The type of messages in the protocol. Corresponds to the state
  -- transition in the state machine diagram.
  data Message (ChainSeek k point tip) from to where
    -- \| Request the next matching result for the given query from the client's
    -- position.
    QueryNext
      :: Move (t :: k)
      -> Message (ChainSeek k point tip) 'StIdle ('StNext t)
    -- \| Reject a query with an error message.
    RejectQuery
      :: SeekError (t :: k)
      -> tip
      -> Message (ChainSeek k point tip) ('StNext t) 'StIdle
    -- \| Send a response to a query and roll the client forward to a new point.
    RollForward
      :: SeekResult (t :: k)
      -> point
      -> tip
      -> Message (ChainSeek k point tip) ('StNext t) 'StIdle
    -- \| Roll the client backward.
    RollBackward
      :: point
      -> tip
      -> Message (ChainSeek k point tip) ('StNext (t :: k)) 'StIdle
    -- \| Inform the client they must wait indefinitely to receive a reply.
    Wait
      :: Message (ChainSeek k point tip) ('StNext (t :: k)) ('StPoll t)
    -- \| End the protocol
    Done
      :: Message (ChainSeek k point tip) 'StIdle 'StDone
    -- \| Ask the server if there have been any updates.
    Poll
      :: Message (ChainSeek k point tip) ('StPoll (t :: k)) ('StNext t)
    -- \| Cancel the polling loop.
    Cancel
      :: Message (ChainSeek k point tip) ('StPoll (t :: k)) 'StIdle
    Scan
      :: Move (t :: k)
      -> Message (ChainSeek k point tip) 'StIdle ('StScan t)
    Collect
      :: Message (ChainSeek k point tip) ('StScan (t :: k)) ('StCollect t)
    CancelScan
      :: Message (ChainSeek k point tip) ('StScan (t :: k)) 'StIdle
    Collected
      :: [(point, SeekResult (t :: k))]
      -> tip
      -> Message (ChainSeek k point tip) ('StCollect t) ('StScan t)
    CollectFailed
      :: SeekError (t :: k)
      -> tip
      -> Message (ChainSeek k point tip) ('StCollect t) 'StIdle
    CollectRollBackward
      :: point
      -> tip
      -> Message (ChainSeek k point tip) ('StCollect (t :: k)) 'StIdle
    CollectWait
      :: tip
      -> Message (ChainSeek k point tip) ('StCollect (t :: k)) ('StPoll t)

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokPoll :: Tag (t :: k) -> ClientHasAgency ('StPoll t :: ChainSeek k point tip)
    TokScan :: Tag (t :: k) -> ClientHasAgency ('StScan t :: ChainSeek k point tip)

  data ServerHasAgency st where
    TokNext :: Tag (t :: k) -> ServerHasAgency ('StNext t :: ChainSeek k point tip)
    TokCollect :: Tag (t :: k) -> ServerHasAgency ('StCollect t :: ChainSeek k point tip)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokPoll{} = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokScan{} = \case {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

class (Binary k, TagKind k) => BinaryTagKind k where
  putMove :: Move (t :: k) -> Put
  getMove :: Tag (t :: k) -> Get (Move t)
  putSeekResult :: SeekResult (t :: k) -> Put
  getSeekResult :: Tag (t :: k) -> Get (SeekResult t)
  putSeekError :: SeekError (t :: k) -> Put
  getSeekError :: Tag (t :: k) -> Get (SeekError t)

instance (BinaryTagKind k) => Binary (SomeTag k) where
  put = put . fromSomeTag
  get = toTag <$> get

instance
  ( BinaryTagKind k
  , Binary tip
  , Binary point
  )
  => BinaryMessage (ChainSeek k point tip)
  where
  putMessage (ClientAgency TokIdle) msg = case msg of
    QueryNext move -> do
      putWord8 0x01
      put $ SomeTag $ moveTag move
      putMove move
    Done -> putWord8 0x02
    Scan move -> do
      putWord8 0x09
      put $ SomeTag $ moveTag move
      putMove move
  putMessage (ServerAgency (TokNext{})) (RejectQuery err tip) = do
    putWord8 0x03
    putSeekError err
    put tip
  putMessage (ServerAgency (TokNext{})) (RollForward result pos tip) = do
    putWord8 0x04
    putSeekResult result
    put pos
    put tip
  putMessage (ServerAgency TokNext{}) (RollBackward pos tip) = do
    putWord8 0x05
    put pos
    put tip
  putMessage (ServerAgency TokNext{}) Wait = putWord8 0x06
  putMessage (ClientAgency TokPoll{}) Poll = putWord8 0x07
  putMessage (ClientAgency TokPoll{}) Cancel = putWord8 0x08
  putMessage (ClientAgency TokScan{}) msg = case msg of
    Collect -> putWord8 0x0a
    CancelScan -> putWord8 0x0b
  putMessage (ServerAgency (TokCollect{})) (CollectFailed err tip) = do
    putWord8 0x0c
    putSeekError err
    put tip
  putMessage (ServerAgency (TokCollect{})) (Collected results tip) = do
    putWord8 0x0d
    put $ toInteger $ length results
    for_ results \(point, result) -> do
      put point
      putSeekResult result
    put tip
  putMessage (ServerAgency TokCollect{}) (CollectRollBackward pos tip) = do
    putWord8 0x0e
    put pos
    put tip
  putMessage (ServerAgency TokCollect{}) (CollectWait tip) = do
    putWord8 0x0f
    put tip

  getMessage tok = do
    tag <- getWord8
    case (tag, tok) of
      (0x01, ClientAgency TokIdle) -> do
        SomeTag qtag <- get
        move <- getMove qtag
        pure $ SomeMessage $ QueryNext move
      (0x02, ClientAgency TokIdle) -> pure $ SomeMessage Done
      (0x03, ServerAgency (TokNext t)) -> do
        err <- getSeekError t
        tip <- get
        pure $ SomeMessage $ RejectQuery err tip
      (0x04, ServerAgency (TokNext t)) -> do
        result <- getSeekResult t
        point <- get
        tip <- get
        pure $ SomeMessage $ RollForward result point tip
      (0x05, ServerAgency (TokNext _)) -> do
        point <- get
        tip <- get
        pure $ SomeMessage $ RollBackward point tip
      (0x06, ServerAgency (TokNext _)) -> pure $ SomeMessage Wait
      (0x07, ClientAgency TokPoll{}) -> pure $ SomeMessage Poll
      (0x08, ClientAgency TokPoll{}) -> pure $ SomeMessage Cancel
      (0x09, ClientAgency TokIdle) -> do
        SomeTag qtag <- get
        move <- getMove qtag
        pure $ SomeMessage $ Scan move
      (0x0a, ClientAgency TokScan{}) -> pure $ SomeMessage Collect
      (0x0b, ClientAgency TokScan{}) -> pure $ SomeMessage CancelScan
      (0x0c, ServerAgency (TokCollect t)) -> do
        err <- getSeekError t
        tip <- get
        pure $ SomeMessage $ CollectFailed err tip
      (0x0d, ServerAgency (TokCollect t)) -> do
        len <- get
        results <- replicateM (fromInteger len) $ (,) <$> get <*> getSeekResult t
        tip <- get
        pure $ SomeMessage $ Collected results tip
      (0x0e, ServerAgency (TokCollect _)) -> do
        point <- get
        tip <- get
        pure $ SomeMessage $ CollectRollBackward point tip
      (0x0f, ServerAgency (TokCollect _)) -> do
        tip <- get
        pure $ SomeMessage $ CollectWait tip
      _ -> fail $ "Unexpected tag " <> show tag

class (Arbitrary k, TagKind k) => ArbitraryTagKind k where
  arbitraryMove :: Tag (t :: k) -> Gen (Move t)
  shrinkMove :: Move (t :: k) -> [Move t]
  arbitrarySeekError :: Tag (t :: k) -> Maybe (Gen (SeekError t))
  shrinkSeekError :: SeekError (t :: k) -> [SeekError t]
  arbitrarySeekResult :: Tag (t :: k) -> Gen (SeekResult t)
  shrinkSeekResult :: SeekResult (t :: k) -> [SeekResult t]

instance (ArbitraryTagKind k) => Arbitrary (SomeTag k) where
  arbitrary = toTag <$> arbitrary
  shrink = fmap toTag . shrink . fromSomeTag

instance
  ( Arbitrary point
  , Arbitrary tip
  , ArbitraryTagKind k
  )
  => ArbitraryMessage (ChainSeek k point tip)
  where
  arbitraryMessage = do
    SomeTag tag <- arbitrary
    let mGenSeekError = arbitrarySeekError tag
    oneof $
      catMaybes
        [ Just $ do
            move <- arbitraryMove tag
            pure $ AnyMessageAndAgency (ClientAgency TokIdle) $ QueryNext move
        , Just $ do
            move <- arbitraryMove tag
            pure $ AnyMessageAndAgency (ClientAgency TokIdle) $ Scan move
        , Just $ pure $ AnyMessageAndAgency (ClientAgency TokIdle) Done
        , mGenSeekError <&> \genErr -> do
            tip <- arbitrary
            err <- genErr
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) $ RejectQuery err tip
        , mGenSeekError <&> \genErr -> do
            tip <- arbitrary
            err <- genErr
            pure $ AnyMessageAndAgency (ServerAgency $ TokCollect tag) $ CollectFailed err tip
        , Just $ do
            result <- arbitrarySeekResult tag
            point <- arbitrary
            tip <- arbitrary
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) $ RollForward result point tip
        , Just $ do
            results <- listOf $ (,) <$> arbitrary <*> arbitrarySeekResult tag
            tip <- arbitrary
            pure $ AnyMessageAndAgency (ServerAgency $ TokCollect tag) $ Collected results tip
        , Just $ do
            point <- arbitrary
            tip <- arbitrary
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) $ RollBackward point tip
        , Just $ do
            point <- arbitrary
            tip <- arbitrary
            pure $ AnyMessageAndAgency (ServerAgency $ TokCollect tag) $ CollectRollBackward point tip
        , Just $ pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) Wait
        , Just $ AnyMessageAndAgency (ServerAgency $ TokCollect tag) . CollectWait <$> arbitrary
        , Just $ pure $ AnyMessageAndAgency (ClientAgency $ TokPoll tag) Poll
        , Just $ pure $ AnyMessageAndAgency (ClientAgency $ TokPoll tag) Cancel
        , Just $ pure $ AnyMessageAndAgency (ClientAgency $ TokScan tag) Collect
        , Just $ pure $ AnyMessageAndAgency (ClientAgency $ TokScan tag) CancelScan
        ]

  shrinkMessage _ = \case
    QueryNext move -> QueryNext <$> shrinkMove move
    RejectQuery err tip ->
      []
        <> [RejectQuery err' tip | err' <- shrinkSeekError err]
        <> [RejectQuery err tip' | tip' <- shrink tip]
    RollForward result point tip ->
      []
        <> [RollForward result' point tip | result' <- shrinkSeekResult result]
        <> [RollForward result point' tip | point' <- shrink point]
        <> [RollForward result point tip' | tip' <- shrink tip]
    RollBackward point tip ->
      []
        <> [RollBackward point' tip | point' <- shrink point]
        <> [RollBackward point tip' | tip' <- shrink tip]
    Scan move -> Scan <$> shrinkMove move
    Collected results tip ->
      []
        <> [Collected results' tip | results' <- shrinkList (shrinkTuple shrink shrinkSeekResult) results]
        <> [Collected results tip' | tip' <- shrink tip]
    CollectFailed err tip ->
      []
        <> [CollectFailed err' tip | err' <- shrinkSeekError err]
        <> [CollectFailed err tip' | tip' <- shrink tip]
    CollectRollBackward point tip ->
      []
        <> [CollectRollBackward point' tip | point' <- shrink point]
        <> [CollectRollBackward point tip' | tip' <- shrink tip]
    CollectWait tip -> CollectWait <$> shrink tip
    Wait -> []
    Done -> []
    Poll -> []
    Cancel -> []
    Collect -> []
    CancelScan -> []

shrinkTuple :: (a -> [a]) -> (b -> [b]) -> (a, b) -> [(a, b)]
shrinkTuple f g (a, b) = ((,b) <$> f a) <> ((a,) <$> g b)

class (Variations k, TagKind k) => VariationsTagKind k where
  moveVariations :: Tag (t :: k) -> NonEmpty (Move t)
  errorVariations :: Tag (t :: k) -> [SeekError t]
  resultVariations :: Tag (t :: k) -> NonEmpty (SeekResult t)

instance (VariationsTagKind k) => Variations (SomeTag k) where
  variations = toTag <$> variations

instance
  ( Variations point
  , Variations tip
  , VariationsTagKind k
  )
  => MessageVariations (ChainSeek k point tip)
  where
  agencyVariations =
    join
      [ pure $ SomePeerHasAgency $ ClientAgency TokIdle
      , do
          SomeTag tag <- variations
          [ SomePeerHasAgency $ ServerAgency $ TokNext tag
            , SomePeerHasAgency $ ServerAgency $ TokCollect tag
            , SomePeerHasAgency $ ClientAgency $ TokPoll tag
            , SomePeerHasAgency $ ClientAgency $ TokScan tag
            ]
      ]
  messageVariations = \case
    ClientAgency TokIdle ->
      join
        [ do
            SomeTag tag <- variations
            join
              [ SomeMessage . QueryNext <$> moveVariations tag
              , SomeMessage . Scan <$> moveVariations tag
              ]
        , pure $ SomeMessage Done
        ]
    ClientAgency (TokPoll{}) -> [SomeMessage Poll, SomeMessage Cancel]
    ClientAgency (TokScan{}) -> [SomeMessage Collect, SomeMessage CancelScan]
    ServerAgency (TokNext tag) -> do
      let point :| points = variations
          tip :| tips = variations
          result :| results = resultVariations tag
          errs = errorVariations tag
      join case errs of
        [] ->
          [ SomeMessage
              <$> RollForward result point tip
                :| fold @[]
                  [ RollForward <$> results <*> pure point <*> pure tip
                  , RollForward result <$> points <*> pure tip
                  , RollForward result point <$> tips
                  ]
          , SomeMessage
              <$> RollBackward point tip
                :| fold @[]
                  [ RollBackward <$> points <*> pure tip
                  , RollBackward point <$> tips
                  ]
          ]
        err : errs' ->
          [ SomeMessage
              <$> RollForward result point tip
                :| fold @[]
                  [ RollForward <$> results <*> pure point <*> pure tip
                  , RollForward result <$> points <*> pure tip
                  , RollForward result point <$> tips
                  ]
          , SomeMessage
              <$> RollBackward point tip
                :| fold @[]
                  [ RollBackward <$> points <*> pure tip
                  , RollBackward point <$> tips
                  ]
          , SomeMessage
              <$> RejectQuery err tip
                :| fold @[]
                  [ RejectQuery <$> errs' <*> pure tip
                  , RejectQuery err <$> tips
                  ]
          ]
    ServerAgency (TokCollect tag) -> do
      let point :| points = variations
          tip :| tips = variations
          result :| results = resultVariations tag
          errs = errorVariations tag
      join case errs of
        [] ->
          [ SomeMessage
              <$> Collected [(point, result)] tip
                :| fold @[]
                  [ pure $ Collected (zip points results) tip
                  , Collected [] <$> tips
                  ]
          , SomeMessage
              <$> CollectRollBackward point tip
                :| fold @[]
                  [ CollectRollBackward <$> points <*> pure tip
                  , CollectRollBackward point <$> tips
                  ]
          ]
        err : errs' ->
          [ SomeMessage
              <$> Collected [(point, result)] tip
                :| fold @[]
                  [ pure $ Collected (zip points results) tip
                  , Collected [] <$> tips
                  ]
          , SomeMessage
              <$> CollectRollBackward point tip
                :| fold @[]
                  [ CollectRollBackward <$> points <*> pure tip
                  , CollectRollBackward point <$> tips
                  ]
          , SomeMessage
              <$> CollectFailed err tip
                :| fold @[]
                  [ CollectFailed <$> errs' <*> pure tip
                  , CollectFailed err <$> tips
                  ]
          ]

class
  ( TagKind k
  , Eq k
  , TestEquality (Tag :: k -> Type)
  , forall (t :: k). Eq (Tag t)
  , forall (t :: k). Eq (Move t)
  , forall (t :: k). Eq (SeekResult t)
  , forall (t :: k). Eq (SeekError t)
  ) =>
  EqTagKind k

instance (EqTagKind k) => Eq (SomeTag k) where
  (==) = on (==) fromSomeTag

deriving instance (EqTagKind k, Eq point, Eq tip) => Eq (Message (ChainSeek k point tip) st st')
deriving instance (EqTagKind k) => Eq (ClientHasAgency (st :: ChainSeek k point tip))
deriving instance (EqTagKind k) => Eq (ServerHasAgency (st :: ChainSeek k point tip))
deriving instance (EqTagKind k) => Eq (NobodyHasAgency (st :: ChainSeek k point tip))

instance (EqTagKind k) => TestEquality (ClientHasAgency :: ChainSeek k point tip -> Type) where
  testEquality = \case
    TokIdle -> \case
      TokIdle -> Just Refl
      _ -> Nothing
    TokPoll t -> \case
      TokPoll t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing
    TokScan t -> \case
      TokScan t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing

instance (EqTagKind k) => TestEquality (ServerHasAgency :: ChainSeek k point tip -> Type) where
  testEquality = \case
    TokNext t -> \case
      TokNext t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing
    TokCollect t -> \case
      TokCollect t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing

instance TestEquality (NobodyHasAgency :: ChainSeek k point tip -> Type) where
  testEquality TokDone TokDone = Just Refl

instance (EqTagKind k) => TestMessageEquality (ChainSeek k point tip) where
  testMessageEquality (ClientAgency tok) (ClientAgency tok') msg msg' = case (tok, tok') of
    (TokIdle, TokIdle) -> case (msg, msg') of
      (QueryNext p, QueryNext p') -> do
        Refl <- testEquality (moveTag p) (moveTag p')
        pure (Refl, Refl, Refl)
      (QueryNext{}, _) -> Nothing
      (Scan p, Scan p') -> do
        Refl <- testEquality (moveTag p) (moveTag p')
        pure (Refl, Refl, Refl)
      (Scan{}, _) -> Nothing
      (Done, Done) -> Just (Refl, Refl, Refl)
      (Done, _) -> Nothing
    (TokIdle, _) -> Nothing
    (TokPoll t, TokPoll t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (Poll, Poll) -> Just (Refl, Refl, Refl)
        (Poll, _) -> Nothing
        (Cancel, Cancel) -> Just (Refl, Refl, Refl)
        (Cancel, _) -> Nothing
    (TokPoll{}, _) -> Nothing
    (TokScan t, TokScan t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (Collect, Collect) -> Just (Refl, Refl, Refl)
        (Collect, _) -> Nothing
        (CancelScan, CancelScan) -> Just (Refl, Refl, Refl)
        (CancelScan, _) -> Nothing
    (TokScan{}, _) -> Nothing
  testMessageEquality (ServerAgency tok) (ServerAgency tok') msg msg' = case (tok, tok') of
    (TokNext t, TokNext t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (RejectQuery{}, RejectQuery{}) -> Just (Refl, Refl, Refl)
        (RejectQuery{}, _) -> Nothing
        (RollForward{}, RollForward{}) -> Just (Refl, Refl, Refl)
        (RollForward{}, _) -> Nothing
        (RollBackward{}, RollBackward{}) -> Just (Refl, Refl, Refl)
        (RollBackward{}, _) -> Nothing
        (Wait, Wait) -> Just (Refl, Refl, Refl)
        (Wait, _) -> Nothing
    (TokNext{}, _) -> Nothing
    (TokCollect t, TokCollect t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (CollectFailed{}, CollectFailed{}) -> Just (Refl, Refl, Refl)
        (CollectFailed{}, _) -> Nothing
        (Collected{}, Collected{}) -> Just (Refl, Refl, Refl)
        (Collected{}, _) -> Nothing
        (CollectRollBackward{}, CollectRollBackward{}) -> Just (Refl, Refl, Refl)
        (CollectRollBackward{}, _) -> Nothing
        (CollectWait{}, CollectWait{}) -> Just (Refl, Refl, Refl)
        (CollectWait{}, _) -> Nothing
    (TokCollect{}, _) -> Nothing
  testMessageEquality _ _ _ _ = Nothing

instance (Eq point, Eq tip, EqTagKind k) => MessageEq (ChainSeek k point tip)

class
  ( Show k
  , TagKind k
  , forall (t :: k). Show (Tag t)
  , forall (t :: k). Show (Move t)
  , forall (t :: k). Show (SeekResult t)
  , forall (t :: k). Show (SeekError t)
  ) =>
  ShowTagKind k

deriving instance (ShowTagKind k, Show point, Show tip) => Show (Message (ChainSeek k point tip) st st')
deriving instance (ShowTagKind k) => Show (ClientHasAgency (st :: ChainSeek k point tip))
deriving instance (ShowTagKind k) => Show (ServerHasAgency (st :: ChainSeek k point tip))
deriving instance (ShowTagKind k) => Show (NobodyHasAgency (st :: ChainSeek k point tip))

instance (Show point, Show tip, ShowTagKind k) => ShowProtocol (ChainSeek k point tip)
