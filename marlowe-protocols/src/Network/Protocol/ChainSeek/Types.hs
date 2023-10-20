{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The type of the chain sync protocol.
module Network.Protocol.ChainSeek.Types where

import Control.Monad (join, replicateM)
import Data.Binary (Binary (..), Get, Put, getWord8, putWord8)
import Data.Data (type (:~:) (Refl))
import Data.Foldable (fold, for_)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Show (showList__, showSpace)
import Network.Protocol.Codec (BinaryMessage (..))
import Network.Protocol.Codec.Spec (
  ArbitraryMessage (..),
  MessageEq (..),
  MessageVariations (..),
  ShowProtocol (..),
  SomePeerHasAgency (..),
  Variations (..),
 )
import Network.Protocol.Handshake.Types (HasSignature (..))
import Network.Protocol.Peer.Trace (MessageAttributes (..), OTelProtocol (..))
import Network.TypedProtocol (PeerHasAgency (..), Protocol (..), SomeMessage (SomeMessage))
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import OpenTelemetry.Attributes (ToPrimitiveAttribute (toPrimitiveAttribute))
import Test.QuickCheck (Arbitrary, Gen, arbitrary, oneof, shrink)

data SomeTag q = forall err result. SomeTag (Tag q err result)

class Query (q :: Type -> Type -> Type) where
  data Tag q :: Type -> Type -> Type
  tagFromQuery :: q err result -> Tag q err result
  tagEq :: Tag q err result -> Tag q err' result' -> Maybe (err :~: err', result :~: result')
  putTag :: Tag q err result -> Put
  getTag :: Get (SomeTag q)
  putQuery :: q err result -> Put
  getQuery :: Tag q err result -> Get (q err result)
  putErr :: Tag q err result -> err -> Put
  getErr :: Tag q err result -> Get err
  putResult :: Tag q err result -> result -> Put
  getResult :: Tag q err result -> Get result

-- | The type of states in the protocol.
data ChainSeek (query :: Type -> Type -> Type) point tip where
  -- | The client and server are idle. The client can send a request.
  StIdle :: ChainSeek query point tip
  -- | The client has sent a next update request. The client is now waiting for
  -- a response, and the server is preparing to send a response. The server can
  -- respond immediately or it can send a 'Wait' message followed by a response
  -- at some point in the future.
  StNext :: Type -> Type -> ChainSeek query point tip
  -- | The client has initiated a scan, which is equivalent to fixing the query and repeatedly collecting the next block
  -- that satisfies it.
  StScan :: Type -> Type -> ChainSeek query point tip
  -- | The client has requested a batch of blocks from the server from a scan.
  StCollect :: Type -> Type -> ChainSeek query point tip
  -- | The server has sent a ping to the client to determine if it is still
  -- connected and is waiting for a pong.
  StPoll :: Type -> Type -> ChainSeek query point tip
  -- | The terminal state of the protocol.
  StDone :: ChainSeek query point tip

instance
  ( HasSignature query
  , HasSignature point
  , HasSignature tip
  )
  => HasSignature (ChainSeek query point tip)
  where
  signature _ =
    T.intercalate
      " "
      [ "ChainSeek"
      , signature $ Proxy @query
      , signature $ Proxy @point
      , signature $ Proxy @tip
      ]

instance Protocol (ChainSeek query point tip) where
  -- \| The type of messages in the protocol. Corresponds to the state
  -- transition in the state machine diagram.
  data Message (ChainSeek query point tip) from to where
    -- \| Request the next matching result for the given query from the client's
    -- position.
    MsgQueryNext
      :: query err result
      -> Message
          (ChainSeek query point tip)
          'StIdle
          ('StNext err result)
    -- \| Reject a query with an error message.
    MsgRejectQuery
      :: err
      -> tip
      -> Message
          (ChainSeek query point tip)
          ('StNext err result)
          'StIdle
    -- \| Send a response to a query and roll the client forward to a new point.
    MsgRollForward
      :: result
      -> point
      -> tip
      -> Message
          (ChainSeek query point tip)
          ('StNext err result)
          'StIdle
    -- \| Roll the client backward.
    MsgRollBackward
      :: point
      -> tip
      -> Message
          (ChainSeek query point tip)
          ('StNext err result)
          'StIdle
    -- \| Inform the client they must wait indefinitely to receive a reply.
    MsgWait
      :: Message
          (ChainSeek query point tip)
          ('StNext err result)
          ('StPoll err result)
    -- \| End the protocol
    MsgDone
      :: Message
          (ChainSeek query point tip)
          'StIdle
          'StDone
    -- \| Ask the server if there have been any updates.
    MsgPoll
      :: Message
          (ChainSeek query point tip)
          ('StPoll err result)
          ('StNext err result)
    -- \| Cancel the polling loop.
    MsgCancel
      :: Message
          (ChainSeek query point tip)
          ('StPoll err result)
          'StIdle
    MsgScan
      :: query err result
      -> Message
          (ChainSeek query point tip)
          'StIdle
          ('StScan err result)
    MsgCollect
      :: Message
          (ChainSeek query point tip)
          ('StScan err result)
          ('StCollect err result)
    MsgCancelScan
      :: Message
          (ChainSeek query point tip)
          ('StScan err result)
          'StIdle
    MsgCollected
      :: [(point, result)]
      -> tip
      -> Message
          (ChainSeek query point tip)
          ('StCollect err result)
          ('StScan err result)
    MsgCollectFailed
      :: err
      -> tip
      -> Message
          (ChainSeek query point tip)
          ('StCollect err result)
          'StIdle
    MsgCollectRollBackward
      :: point
      -> tip
      -> Message
          (ChainSeek query point tip)
          ('StCollect err result)
          'StIdle
    MsgCollectWait
      :: tip
      -> Message
          (ChainSeek query point tip)
          ('StCollect err result)
          ('StPoll err result)

  data ClientHasAgency st where
    TokIdle :: ClientHasAgency 'StIdle
    TokPoll :: ClientHasAgency ('StPoll err result)
    TokScan :: ClientHasAgency ('StScan err result)

  data ServerHasAgency st where
    TokNext :: Tag query err result -> ServerHasAgency ('StNext err result :: ChainSeek query point tip)
    TokCollect :: Tag query err result -> ServerHasAgency ('StCollect err result :: ChainSeek query point tip)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokIdle = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokPoll = \case {}
  exclusionLemma_ClientAndServerHaveAgency TokScan = \case {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

instance
  ( Query query
  , Binary tip
  , Binary point
  )
  => BinaryMessage (ChainSeek query point tip)
  where
  putMessage (ClientAgency TokIdle) msg = case msg of
    MsgQueryNext query -> do
      putWord8 0x01
      let tag = tagFromQuery query
      putTag tag
      putQuery query
    MsgDone -> putWord8 0x02
    MsgScan query -> do
      putWord8 0x09
      let tag = tagFromQuery query
      putTag tag
      putQuery query
  putMessage (ServerAgency (TokNext tag)) (MsgRejectQuery err tip) = do
    putWord8 0x03
    putTag tag
    putErr tag err
    put tip
  putMessage (ServerAgency (TokNext tag)) (MsgRollForward result pos tip) = do
    putWord8 0x04
    putTag tag
    putResult tag result
    put pos
    put tip
  putMessage (ServerAgency TokNext{}) (MsgRollBackward pos tip) = do
    putWord8 0x05
    put pos
    put tip
  putMessage (ServerAgency TokNext{}) MsgWait = putWord8 0x06
  putMessage (ClientAgency TokPoll) MsgPoll = putWord8 0x07
  putMessage (ClientAgency TokPoll) MsgCancel = putWord8 0x08
  putMessage (ClientAgency TokScan) msg = case msg of
    MsgCollect -> putWord8 0x0a
    MsgCancelScan -> putWord8 0x0b
  putMessage (ServerAgency (TokCollect tag)) (MsgCollectFailed err tip) = do
    putWord8 0x0c
    putTag tag
    putErr tag err
    put tip
  putMessage (ServerAgency (TokCollect tag)) (MsgCollected results tip) = do
    putWord8 0x0d
    putTag tag
    put $ length results
    for_ results \(point, result) -> do
      put point
      putResult tag result
    put tip
  putMessage (ServerAgency TokCollect{}) (MsgCollectRollBackward pos tip) = do
    putWord8 0x0e
    put pos
    put tip
  putMessage (ServerAgency TokCollect{}) (MsgCollectWait tip) = do
    putWord8 0x0f
    put tip

  getMessage tok = do
    tag <- getWord8
    case (tag, tok) of
      (0x01, ClientAgency TokIdle) -> do
        SomeTag qtag <- getTag
        SomeMessage . MsgQueryNext <$> getQuery qtag
      (0x02, ClientAgency TokIdle) -> pure $ SomeMessage MsgDone
      (0x03, ServerAgency (TokNext qtag)) -> do
        SomeTag qtag' :: SomeTag query <- getTag
        case tagEq qtag qtag' of
          Nothing -> fail "decoded query tag does not match expected query tag"
          Just (Refl, Refl) -> do
            err <- getErr qtag'
            tip <- get
            pure $ SomeMessage $ MsgRejectQuery err tip
      (0x04, ServerAgency (TokNext qtag)) -> do
        SomeTag qtag' :: SomeTag query <- getTag
        case tagEq qtag qtag' of
          Nothing -> fail "decoded query tag does not match expected query tag"
          Just (Refl, Refl) -> do
            result <- getResult qtag'
            point <- get
            tip <- get
            pure $ SomeMessage $ MsgRollForward result point tip
      (0x05, ServerAgency (TokNext _)) -> do
        point <- get
        tip <- get
        pure $ SomeMessage $ MsgRollBackward point tip
      (0x06, ServerAgency (TokNext _)) -> pure $ SomeMessage MsgWait
      (0x07, ClientAgency TokPoll) -> pure $ SomeMessage MsgPoll
      (0x08, ClientAgency TokPoll) -> pure $ SomeMessage MsgCancel
      (0x09, ClientAgency TokIdle) -> do
        SomeTag qtag <- getTag
        SomeMessage . MsgScan <$> getQuery qtag
      (0x0a, ClientAgency TokScan) -> pure $ SomeMessage MsgCollect
      (0x0b, ClientAgency TokScan) -> pure $ SomeMessage MsgCancelScan
      (0x0c, ServerAgency (TokCollect qtag)) -> do
        SomeTag qtag' :: SomeTag query <- getTag
        case tagEq qtag qtag' of
          Nothing -> fail "decoded query tag does not match expected query tag"
          Just (Refl, Refl) -> do
            err <- getErr qtag'
            tip <- get
            pure $ SomeMessage $ MsgCollectFailed err tip
      (0x0d, ServerAgency (TokCollect qtag)) -> do
        SomeTag qtag' :: SomeTag query <- getTag
        case tagEq qtag qtag' of
          Nothing -> fail "decoded query tag does not match expected query tag"
          Just (Refl, Refl) -> do
            len <- get
            results <- replicateM len $ (,) <$> get <*> getResult qtag'
            tip <- get
            pure $ SomeMessage $ MsgCollected results tip
      (0x0e, ServerAgency (TokCollect _)) -> do
        point <- get
        tip <- get
        pure $ SomeMessage $ MsgCollectRollBackward point tip
      (0x0f, ServerAgency (TokCollect _)) -> do
        tip <- get
        pure $ SomeMessage $ MsgCollectWait tip
      _ -> fail $ "Unexpected tag " <> show tag

class (ShowQuery query) => OTelQuery query where
  queryTypeName :: Proxy query -> Text
  queryName :: Tag query err result -> Text

instance
  ( OTelQuery query
  , Show point
  , Show tip
  )
  => OTelProtocol (ChainSeek query point tip)
  where
  protocolName _ = "chain_seek." <> queryTypeName (Proxy @query)
  messageAttributes = curry \case
    (_, MsgQueryNext query) ->
      MessageAttributes
        { messageType = "query/" <> queryName (tagFromQuery query)
        , messageParameters = [toPrimitiveAttribute $ T.pack $ showsPrecQuery 0 query ""]
        }
    (_, MsgScan query) ->
      MessageAttributes
        { messageType = "scan/" <> queryName (tagFromQuery query)
        , messageParameters = [toPrimitiveAttribute $ T.pack $ showsPrecQuery 0 query ""]
        }
    (ServerAgency (TokNext tag), MsgRejectQuery err tip) ->
      MessageAttributes
        { messageType = "query/" <> queryName tag <> "/reject"
        , messageParameters = toPrimitiveAttribute . T.pack <$> [showsPrecErr 0 tag err "", show tip]
        }
    (ServerAgency (TokNext tag), MsgRollForward result point tip) ->
      MessageAttributes
        { messageType = "query/" <> queryName tag <> "/roll_forward"
        , messageParameters = toPrimitiveAttribute . T.pack <$> [showsPrecResult 0 tag result "", show point, show tip]
        }
    (ServerAgency (TokNext tag), MsgRollBackward point tip) ->
      MessageAttributes
        { messageType = "query/" <> queryName tag <> "/roll_backward"
        , messageParameters = toPrimitiveAttribute . T.pack <$> [show point, show tip]
        }
    (ServerAgency (TokNext tag), MsgWait) ->
      MessageAttributes
        { messageType = "query/" <> queryName tag <> "/wait"
        , messageParameters = []
        }
    (ServerAgency (TokCollect tag), MsgCollectFailed err tip) ->
      MessageAttributes
        { messageType = "scan/" <> queryName tag <> "/failed"
        , messageParameters = toPrimitiveAttribute . T.pack <$> [showsPrecErr 0 tag err "", show tip]
        }
    (ServerAgency (TokCollect tag), MsgCollected _ _) ->
      MessageAttributes
        { messageType = "scan/" <> queryName tag <> "/collected"
        , messageParameters = []
        }
    (ServerAgency (TokCollect tag), MsgCollectRollBackward point tip) ->
      MessageAttributes
        { messageType = "scan/" <> queryName tag <> "/roll_backward"
        , messageParameters = toPrimitiveAttribute . T.pack <$> [show point, show tip]
        }
    (ServerAgency (TokCollect tag), MsgCollectWait _) ->
      MessageAttributes
        { messageType = "scan/" <> queryName tag <> "/wait"
        , messageParameters = []
        }
    (_, MsgDone) ->
      MessageAttributes
        { messageType = "done"
        , messageParameters = []
        }
    (_, MsgPoll) ->
      MessageAttributes
        { messageType = "poll"
        , messageParameters = []
        }
    (_, MsgCancel) ->
      MessageAttributes
        { messageType = "cancel"
        , messageParameters = []
        }
    (_, MsgCollect) ->
      MessageAttributes
        { messageType = "collect"
        , messageParameters = []
        }
    (_, MsgCancelScan) ->
      MessageAttributes
        { messageType = "collect_Cancel"
        , messageParameters = []
        }

class (Query query) => ArbitraryQuery query where
  arbitraryTag :: Gen (SomeTag query)
  arbitraryQuery :: Tag query err result -> Gen (query err result)
  arbitraryErr :: Tag query err result -> Maybe (Gen err)
  arbitraryResult :: Tag query err result -> Gen result
  shrinkQuery :: query err result -> [query err result]
  shrinkErr :: Tag query err result -> err -> [err]
  shrinkResult :: Tag query err result -> result -> [result]

class (Query query) => QueryVariations query where
  tags :: NonEmpty (SomeTag query)
  queryVariations :: Tag query err result -> NonEmpty (query err result)
  errVariations :: Tag query err result -> [err]
  resultVariations :: Tag query err result -> NonEmpty result

instance
  ( Arbitrary point
  , Arbitrary tip
  , ArbitraryQuery query
  )
  => ArbitraryMessage (ChainSeek query point tip)
  where
  arbitraryMessage = do
    SomeTag tag <- arbitraryTag
    let mGenError = arbitraryErr tag
    oneof $
      catMaybes
        [ Just $ do
            query <- arbitraryQuery tag
            pure $ AnyMessageAndAgency (ClientAgency TokIdle) $ MsgQueryNext query
        , Just $ pure $ AnyMessageAndAgency (ClientAgency TokIdle) MsgDone
        , mGenError <&> \genErr -> do
            tip <- arbitrary
            err <- genErr
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) $ MsgRejectQuery err tip
        , Just $ do
            result <- arbitraryResult tag
            point <- arbitrary
            tip <- arbitrary
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) $ MsgRollForward result point tip
        , Just $ do
            point <- arbitrary
            tip <- arbitrary
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) $ MsgRollBackward point tip
        , Just $ do
            pure $ AnyMessageAndAgency (ServerAgency $ TokNext tag) MsgWait
        , Just $ pure $ AnyMessageAndAgency (ClientAgency TokPoll) MsgPoll
        , Just $ pure $ AnyMessageAndAgency (ClientAgency TokPoll) MsgCancel
        ]
  shrinkMessage agency = \case
    MsgQueryNext query -> MsgQueryNext <$> shrinkQuery query
    MsgRejectQuery err tip ->
      []
        <> [MsgRejectQuery err' tip | err' <- case agency of ServerAgency (TokNext tag) -> shrinkErr tag err]
        <> [MsgRejectQuery err tip' | tip' <- shrink tip]
    MsgRollForward result point tip ->
      []
        <> [MsgRollForward result' point tip | result' <- case agency of ServerAgency (TokNext tag) -> shrinkResult tag result]
        <> [MsgRollForward result point' tip | point' <- shrink point]
        <> [MsgRollForward result point tip' | tip' <- shrink tip]
    MsgRollBackward point tip ->
      []
        <> [MsgRollBackward point' tip | point' <- shrink point]
        <> [MsgRollBackward point tip' | tip' <- shrink tip]
    _ -> []

instance
  ( Variations point
  , Variations tip
  , QueryVariations query
  )
  => MessageVariations (ChainSeek query point tip)
  where
  agencyVariations =
    join
      [ pure $ SomePeerHasAgency $ ClientAgency TokIdle
      , pure $ SomePeerHasAgency $ ClientAgency TokPoll
      , pure $ SomePeerHasAgency $ ClientAgency TokScan
      , do
          SomeTag tag <- tags
          [ SomePeerHasAgency $ ServerAgency $ TokNext tag
            , SomePeerHasAgency $ ServerAgency $ TokCollect tag
            ]
      ]
  messageVariations = \case
    ClientAgency TokIdle ->
      join
        [ do
            SomeTag tag <- tags
            join
              [ SomeMessage . MsgQueryNext <$> queryVariations tag
              , SomeMessage . MsgScan <$> queryVariations tag
              ]
        , pure $ SomeMessage MsgDone
        ]
    ClientAgency TokPoll -> [SomeMessage MsgPoll, SomeMessage MsgCancel]
    ClientAgency TokScan -> [SomeMessage MsgCollect, SomeMessage MsgCancelScan]
    ServerAgency (TokNext tag) -> do
      let point :| points = variations
          tip :| tips = variations
          result :| results = resultVariations tag
          errs = errVariations tag
      join case errs of
        [] ->
          [ SomeMessage
              <$> MsgRollForward result point tip
                :| fold @[]
                  [ MsgRollForward <$> results <*> pure point <*> pure tip
                  , MsgRollForward result <$> points <*> pure tip
                  , MsgRollForward result point <$> tips
                  ]
          , SomeMessage
              <$> MsgRollBackward point tip
                :| fold @[]
                  [ MsgRollBackward <$> points <*> pure tip
                  , MsgRollBackward point <$> tips
                  ]
          ]
        err : errs' ->
          [ SomeMessage
              <$> MsgRollForward result point tip
                :| fold @[]
                  [ MsgRollForward <$> results <*> pure point <*> pure tip
                  , MsgRollForward result <$> points <*> pure tip
                  , MsgRollForward result point <$> tips
                  ]
          , SomeMessage
              <$> MsgRollBackward point tip
                :| fold @[]
                  [ MsgRollBackward <$> points <*> pure tip
                  , MsgRollBackward point <$> tips
                  ]
          , SomeMessage
              <$> MsgRejectQuery err tip
                :| fold @[]
                  [ MsgRejectQuery <$> errs' <*> pure tip
                  , MsgRejectQuery err <$> tips
                  ]
          ]
    ServerAgency (TokCollect tag) -> do
      let point :| points = variations
          tip :| tips = variations
          result :| results = resultVariations tag
          errs = errVariations tag
      join case errs of
        [] ->
          [ SomeMessage
              <$> MsgCollected [(point, result)] tip
                :| fold @[]
                  [ pure $ MsgCollected (zip points results) tip
                  , MsgCollected [] <$> tips
                  ]
          , SomeMessage
              <$> MsgCollectRollBackward point tip
                :| fold @[]
                  [ MsgCollectRollBackward <$> points <*> pure tip
                  , MsgCollectRollBackward point <$> tips
                  ]
          ]
        err : errs' ->
          [ SomeMessage
              <$> MsgCollected [(point, result)] tip
                :| fold @[]
                  [ pure $ MsgCollected (zip points results) tip
                  , MsgCollected [] <$> tips
                  ]
          , SomeMessage
              <$> MsgCollectRollBackward point tip
                :| fold @[]
                  [ MsgCollectRollBackward <$> points <*> pure tip
                  , MsgCollectRollBackward point <$> tips
                  ]
          , SomeMessage
              <$> MsgCollectFailed err tip
                :| fold @[]
                  [ MsgCollectFailed <$> errs' <*> pure tip
                  , MsgCollectFailed err <$> tips
                  ]
          ]

class (Query query) => QueryEq query where
  queryEq :: query err result -> query err result -> Bool
  errEq :: Tag query err result -> err -> err -> Bool
  resultEq :: Tag query err result -> result -> result -> Bool

instance
  ( Eq point
  , Eq tip
  , QueryEq query
  )
  => MessageEq (ChainSeek query point tip)
  where
  messageEq (AnyMessageAndAgency agency msg) = case (agency, msg) of
    (_, MsgQueryNext query) -> \case
      AnyMessageAndAgency _ (MsgQueryNext query') ->
        case tagEq (tagFromQuery query) (tagFromQuery query') of
          Just (Refl, Refl) -> queryEq query query'
          Nothing -> False
      _ -> False
    (_, MsgScan query) -> \case
      AnyMessageAndAgency _ (MsgScan query') ->
        case tagEq (tagFromQuery query) (tagFromQuery query') of
          Just (Refl, Refl) -> queryEq query query'
          Nothing -> False
      _ -> False
    (ServerAgency (TokNext tag), MsgRejectQuery err tip) -> \case
      AnyMessageAndAgency (ServerAgency (TokNext tag')) (MsgRejectQuery err' tip') ->
        tip == tip' && case tagEq tag tag' of
          Just (Refl, Refl) -> errEq tag err err'
          Nothing -> False
      _ -> False
    (ServerAgency (TokNext tag), MsgRollForward result point tip) -> \case
      AnyMessageAndAgency (ServerAgency (TokNext tag')) (MsgRollForward result' point' tip') ->
        point == point' && tip == tip' && case tagEq tag tag' of
          Just (Refl, Refl) -> resultEq tag result result'
          Nothing -> False
      _ -> False
    (_, MsgRollBackward point tip) -> \case
      AnyMessageAndAgency _ (MsgRollBackward point' tip') ->
        point == point' && tip == tip'
      _ -> False
    (_, MsgWait) -> \case
      AnyMessageAndAgency _ MsgWait -> True
      _ -> False
    (ServerAgency (TokCollect tag), MsgCollectFailed err tip) -> \case
      AnyMessageAndAgency (ServerAgency (TokCollect tag')) (MsgCollectFailed err' tip') ->
        tip == tip' && case tagEq tag tag' of
          Just (Refl, Refl) -> errEq tag err err'
          Nothing -> False
      _ -> False
    (ServerAgency (TokCollect tag), MsgCollected results tip) -> \case
      AnyMessageAndAgency (ServerAgency (TokCollect tag')) (MsgCollected results' tip') ->
        tip == tip' && case tagEq tag tag' of
          Just (Refl, Refl) -> all (\((point, result), (point', result')) -> point == point' && resultEq tag result result') $ zip results results'
          Nothing -> False
      _ -> False
    (_, MsgCollectRollBackward point tip) -> \case
      AnyMessageAndAgency _ (MsgCollectRollBackward point' tip') ->
        point == point' && tip == tip'
      _ -> False
    (_, MsgCollectWait tip) -> \case
      AnyMessageAndAgency _ (MsgCollectWait tip') -> tip == tip'
      _ -> False
    (_, MsgDone) -> \case
      AnyMessageAndAgency _ MsgDone -> True
      _ -> False
    (_, MsgPoll) -> \case
      AnyMessageAndAgency _ MsgPoll -> True
      _ -> False
    (_, MsgCancel) -> \case
      AnyMessageAndAgency _ MsgCancel -> True
      _ -> False
    (_, MsgCollect) -> \case
      AnyMessageAndAgency _ MsgCollect -> True
      _ -> False
    (_, MsgCancelScan) -> \case
      AnyMessageAndAgency _ MsgCancelScan -> True
      _ -> False

class (Query query) => ShowQuery query where
  showsPrecTag :: Int -> Tag query err result -> ShowS
  showsPrecQuery :: Int -> query err result -> ShowS
  showsPrecErr :: Int -> Tag query err result -> err -> ShowS
  showsPrecResult :: Int -> Tag query err result -> result -> ShowS

instance
  ( Show point
  , Show tip
  , ShowQuery query
  )
  => ShowProtocol (ChainSeek query point tip)
  where
  showsPrecMessage p agency = \case
    MsgQueryNext query ->
      showParen
        (p >= 11)
        ( showString "MsgQueryNext"
            . showSpace
            . showsPrecQuery 11 query
        )
    MsgScan query ->
      showParen
        (p >= 11)
        ( showString "MsgScan"
            . showSpace
            . showsPrecQuery 11 query
        )
    MsgRejectQuery err tip ->
      showParen
        (p >= 11)
        ( showString "MsgRejectQuery"
            . showSpace
            . case agency of ServerAgency (TokNext tag) -> showsPrecErr 11 tag err
            . showSpace
            . showsPrec 11 tip
        )
    MsgRollForward result point tip ->
      showParen
        (p >= 11)
        ( showString "MsgRollForward"
            . showSpace
            . case agency of ServerAgency (TokNext tag) -> showsPrecResult 11 tag result
            . showSpace
            . showsPrec 11 point
            . showSpace
            . showsPrec 11 tip
        )
    MsgRollBackward point tip ->
      showParen
        (p >= 11)
        ( showString "MsgRollBackward"
            . showSpace
            . showsPrec 11 point
            . showSpace
            . showsPrec 11 tip
        )
    MsgWait -> showString "MsgWait"
    MsgCollectFailed err tip ->
      showParen
        (p >= 11)
        ( showString "MsgCollectFailed"
            . showSpace
            . case agency of ServerAgency (TokCollect tag) -> showsPrecErr 11 tag err
            . showSpace
            . showsPrec 11 tip
        )
    MsgCollected results tip ->
      showParen
        (p >= 11)
        ( showString "MsgCollected"
            . showSpace
            . case agency of
              ServerAgency (TokCollect tag) ->
                showList__
                  (\(point, result) -> showParen True $ shows point . showsPrecResult 0 tag result)
                  results
            . showSpace
            . showsPrec 11 tip
        )
    MsgCollectRollBackward point tip ->
      showParen
        (p >= 11)
        ( showString "MsgCollectRollBackward"
            . showSpace
            . showsPrec 11 point
            . showSpace
            . showsPrec 11 tip
        )
    MsgCollectWait tip ->
      showParen
        (p >= 11)
        ( showString "MsgCollectWait"
            . showSpace
            . showsPrec 11 tip
        )
    MsgDone -> showString "MsgDone"
    MsgPoll -> showString "MsgPoll"
    MsgCancel -> showString "MsgCancel"
    MsgCollect -> showString "MsgCollect"
    MsgCancelScan -> showString "MsgCancelScan"

  showsPrecServerHasAgency p = \case
    TokNext tag ->
      showParen
        (p >= 11)
        ( showString "TokNext"
            . showSpace
            . showsPrecTag p tag
        )
    TokCollect tag ->
      showParen
        (p >= 11)
        ( showString "TokCollect"
            . showSpace
            . showsPrecTag p tag
        )

  showsPrecClientHasAgency _ =
    showString . \case
      TokIdle -> "TokIdle"
      TokPoll -> "TokPoll"
      TokScan -> "TokScan"
