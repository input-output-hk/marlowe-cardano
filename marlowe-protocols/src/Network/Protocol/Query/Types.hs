{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The type of the query protocol.
--
-- The query protocol is used to stream paginated query results. A client can
-- query resources from a server and collect the results incrementally in pages.
-- The server is expected to provide transactional reads to the client while the
-- query is active.
module Network.Protocol.Query.Types
  where

import Control.Monad (join)
import Data.Aeson (Value(..), object, (.=))
import Data.Binary (Get, Put, getWord8, putWord8)
import Data.Data (type (:~:)(Refl))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import GHC.Show (showSpace)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Codec.Spec
  (ArbitraryMessage(..), MessageEq(..), MessageVariations(..), ShowProtocol(..), SomePeerHasAgency(..))
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event.Network.Protocol (MessageToJSON(..))
import Test.QuickCheck (Gen, oneof)

data SomeTag q = forall delimiter err result. SomeTag (Tag q delimiter err result)

class IsQuery (q :: * -> * -> * -> *) where
  data Tag q :: * -> * -> * -> *
  tagFromQuery :: q delimiter err result -> Tag q delimiter err result
  tagEq :: Tag q delimiter err result -> Tag q delimiter' err' result' -> Maybe (delimiter :~: delimiter', err :~: err', result :~: result')
  putTag :: Tag q delimiter err result -> Put
  getTag :: Get (SomeTag q)
  putQuery :: q delimiter err result -> Put
  getQuery :: Tag q delimiter err result -> Get (q delimiter err result)
  putDelimiter :: Tag q delimiter err result -> delimiter -> Put
  getDelimiter :: Tag q delimiter err result -> Get delimiter
  putErr :: Tag q delimiter err result -> err -> Put
  getErr :: Tag q delimiter err result -> Get err
  putResult :: Tag q delimiter err result -> result -> Put
  getResult :: Tag q delimiter err result -> Get result

class IsQuery q => QueryToJSON q where
  queryToJSON :: q delimiter err result -> Value
  errToJSON :: Tag q delimiter err result -> err -> Value
  resultToJSON :: Tag q delimiter err result -> result -> Value
  delimiterToJSON :: Tag q delimiter err result -> delimiter -> Value

-- | A state kind for the query protocol.
data Query (query :: * -> * -> * -> *) where

  -- | The initial state of the protocol.
  StInit :: Query query

  -- | In the 'StNext' state, the server has agency. It is loading a page of
  -- results and preparing to send them to the client. It may also reject the
  -- query.
  StNext :: StNextKind -> delimiter -> err -> results -> Query query

  -- | In the 'StPage' state, the client has agency. It is processing a page
  -- of results and preparing to load another page, or to exit early.
  StPage :: delimiter -> err -> results -> Query query

  -- | The terminal state of the protocol.
  StDone :: Query query

instance HasSignature query => HasSignature (Query query) where
  signature _ = T.intercalate " " ["Query", signature $ Proxy @query]

data StNextKind
  = CanReject
  | MustReply

instance Protocol (Query query) where

  -- | The type of messages in the protocol. Corresponds to state transition in
  -- the state machine diagram of the protocol.
  data Message (Query query) from to where

    -- | Query resources from the server.
    MsgRequest :: query delimiter err results -> Message (Query query)
      'StInit
      ('StNext 'CanReject delimiter err results)

    -- | Reject the query with an error message.
    MsgReject :: err -> Message (Query query)
      ('StNext 'CanReject delimiter err results)
      'StDone

    -- | Send the next page of results to the client
    MsgNextPage :: results -> Maybe delimiter -> Message (Query query)
      ('StNext nextKind delimiter err results)
      ('StPage delimiter err results)

    -- | Request the next page of results starting from the delimiter
    MsgRequestNext :: delimiter -> Message (Query query)
      ('StPage delimiter err results)
      ('StNext 'MustReply delimiter err results)

    -- | Stop collecting results.
    MsgDone :: Message (Query query)
      ('StPage delimiter err results)
      'StDone

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokPage :: Tag query delimiter err results -> ClientHasAgency ('StPage delimiter err results :: Query query)

  data ServerHasAgency st where
    TokNext :: TokNextKind k -> Tag query delimiter err results -> ServerHasAgency ('StNext k delimiter err results :: Query query)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit     = \case
  exclusionLemma_ClientAndServerHaveAgency (TokPage _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data TokNextKind k where
  TokCanReject :: TokNextKind 'CanReject
  TokMustReply :: TokNextKind 'MustReply

deriving instance Show (TokNextKind k)

instance IsQuery query => BinaryMessage (Query query) where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgRequest query -> do
        putWord8 0x01
        putTag (tagFromQuery query)
        putQuery query
    ServerAgency (TokNext _ tag) -> \case
      MsgReject err -> do
        putWord8 0x02
        putTag tag
        putErr tag err
      MsgNextPage results delimiter -> do
        putWord8 0x03
        putTag tag
        putResult tag results
        case delimiter of
          Nothing -> putWord8 0x01
          Just d  -> do
            putWord8 0x02
            putDelimiter tag d
    ClientAgency (TokPage tag) -> \case
      MsgRequestNext delimiter -> do
        putWord8 0x04
        putTag tag
        putDelimiter tag delimiter
      MsgDone -> putWord8 0x05

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> do
          SomeTag qtag <- getTag
          SomeMessage . MsgRequest <$> getQuery qtag
        _ -> fail "Invalid protocol state for MsgRequest"
      0x02 -> case tok of
        ServerAgency (TokNext TokCanReject qtag) -> do
          SomeTag qtag' :: SomeTag query <- getTag
          case tagEq qtag qtag' of
            Nothing                 -> fail "decoded query tag does not match expected query tag"
            Just (Refl, Refl, Refl) -> SomeMessage . MsgReject <$> getErr qtag'
        _ -> fail "Invalid protocol state for MsgReject"
      0x03 -> case tok of
        ServerAgency (TokNext _ qtag) -> do
          SomeTag qtag' :: SomeTag query <- getTag
          case tagEq qtag qtag' of
            Nothing   -> fail "decoded query tag does not match expected query tag"
            Just (Refl, Refl, Refl) -> do
              result <- getResult qtag'
              maybeTag <- getWord8
              delimiter <- case maybeTag of
                0x01 -> pure Nothing
                0x02 -> Just <$> getDelimiter qtag'
                _    -> fail $ "Invalid maybe tag: " <> show maybeTag
              pure $ SomeMessage $ MsgNextPage result delimiter
        _ -> fail "Invalid protocol state for MsgNextPage"
      0x04 -> case tok of
        ClientAgency (TokPage qtag) -> do
          SomeTag qtag' :: SomeTag query <- getTag
          case tagEq qtag qtag' of
            Nothing                 -> fail "decoded query tag does not match expected query tag"
            Just (Refl, Refl, Refl) -> SomeMessage . MsgRequestNext <$> getDelimiter qtag'
        _                            -> fail "Invalid protocol state for MsgRequestNext"
      0x05 -> case tok of
        ClientAgency (TokPage _) -> pure $ SomeMessage MsgDone
        _                        -> fail "Invalid protocol state for MsgDone"
      _ -> fail $ "Invalid msg tag " <> show tag

instance QueryToJSON query => MessageToJSON (Query query) where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgRequest q -> object [ "request" .= queryToJSON q ]
    ClientAgency (TokPage tag) -> \case
      MsgRequestNext d -> object [ "delimiter" .= delimiterToJSON tag d ]
      MsgDone -> String "done"
    ServerAgency (TokNext _ tag)-> \case
      MsgReject err -> object [ "reject" .= errToJSON tag err ]
      MsgNextPage results d -> object
        [ "reject" .= object
            [ "results" .= resultToJSON tag results
            , "next" .= (delimiterToJSON tag <$> d)
            ]
        ]

class IsQuery query => ArbitraryQuery query where
  arbitraryTag :: Gen (SomeTag query)
  arbitraryQuery :: Tag query delimiter err results -> Gen (query delimiter err results)
  arbitraryDelimiter :: Tag query delimiter err results -> Maybe (Gen delimiter)
  arbitraryErr :: Tag query delimiter err results -> Maybe (Gen err)
  arbitraryResults :: Tag query delimiter err results -> Gen results
  shrinkQuery :: query delimiter err results -> [query delimiter err results]
  shrinkErr :: Tag query delimiter err results -> err -> [err]
  shrinkResults :: Tag query delimiter err results -> results -> [results]
  shrinkDelimiter :: Tag query delimiter err results -> delimiter -> [delimiter]

class IsQuery query => QueryVariations query where
  tags :: NonEmpty (SomeTag query)
  queryVariations :: Tag query delimiter err results -> NonEmpty (query delimiter err results)
  delimiterVariations :: Tag query delimiter err results -> [delimiter]
  errVariations :: Tag query delimiter err results -> [err]
  resultsVariations :: Tag query delimiter err results -> NonEmpty results

instance ArbitraryQuery query => ArbitraryMessage (Query query) where
  arbitraryMessage = do
    SomeTag tag <- arbitraryTag
    let mGenError = arbitraryErr tag
    let mGenDelimiter = arbitraryDelimiter tag
    oneof $ catMaybes
      [ Just $ AnyMessageAndAgency (ClientAgency TokInit) . MsgRequest <$> arbitraryQuery tag
      , mGenError <&> \genErr -> do
          err <- genErr
          pure $ AnyMessageAndAgency (ServerAgency $ TokNext TokCanReject tag) $ MsgReject err
      , Just $ do
          results <- arbitraryResults tag
          AnyMessageAndAgency (ServerAgency $ TokNext TokCanReject tag) . MsgNextPage results
            <$> oneof [pure Nothing, maybe (pure Nothing) (fmap Just) mGenDelimiter]
      , mGenDelimiter <&> \genDelimiter -> do
          delimiter <- genDelimiter
          pure $ AnyMessageAndAgency (ClientAgency $ TokPage tag) $ MsgRequestNext delimiter
      , Just $ pure $ AnyMessageAndAgency (ClientAgency $ TokPage tag) MsgDone
      ]
  shrinkMessage agency = \case
    MsgRequest query -> MsgRequest <$> shrinkQuery query
    MsgReject err -> MsgReject <$> case agency of ServerAgency (TokNext _ tag) -> shrinkErr tag err
    MsgNextPage results mDelimiter -> []
      <> [ MsgNextPage results' mDelimiter | results' <- case agency of ServerAgency (TokNext _ tag) -> shrinkResults tag results ]
      <> case mDelimiter of
          Nothing -> []
          Just delimiter -> MsgNextPage results Nothing
            : [ MsgNextPage results (Just delimiter') | delimiter' <- case agency of ServerAgency (TokNext _ tag) -> shrinkDelimiter tag delimiter ]
    MsgRequestNext delimiter -> MsgRequestNext <$> case agency of ClientAgency (TokPage tag) -> shrinkDelimiter tag delimiter
    _ -> []

instance QueryVariations query => MessageVariations (Query query) where
  agencyVariations = join
    [ pure $ SomePeerHasAgency $ ClientAgency TokInit
    , do
        SomeTag tag <- tags
        pure $ SomePeerHasAgency $ ClientAgency $ TokPage tag
    , do
        SomeTag tag <- tags
        [
            SomePeerHasAgency $ ServerAgency $ TokNext TokCanReject tag
          , SomePeerHasAgency $ ServerAgency $ TokNext TokMustReply tag
          ]
    ]
  messageVariations = \case
    ClientAgency TokInit -> do
      SomeTag tag <- tags
      SomeMessage . MsgRequest <$> queryVariations tag
    ClientAgency (TokPage tag) ->
      SomeMessage MsgDone :| (SomeMessage . MsgRequestNext <$> delimiterVariations tag)
    ServerAgency (TokNext nextKind tag) -> case resultsVariations tag of
      result :| results -> SomeMessage (MsgNextPage result Nothing) :| join
        [ SomeMessage . MsgNextPage result . Just <$> delimiterVariations tag
        , SomeMessage . flip MsgNextPage Nothing <$> results
        , case nextKind of
            TokCanReject -> SomeMessage . MsgReject <$> errVariations tag
            TokMustReply -> []
        ]

class IsQuery query => QueryEq query where
  queryEq :: query delimiter err result -> query delimiter err result -> Bool
  delimiterEq :: Tag query delimiter err result -> delimiter -> delimiter -> Bool
  errEq :: Tag query delimiter err result -> err -> err -> Bool
  resultEq :: Tag query delimiter err result -> result -> result -> Bool

instance QueryEq query => MessageEq (Query query) where
  messageEq (AnyMessageAndAgency agency msg) = case (agency, msg) of
    (_, MsgRequest query) -> \case
      AnyMessageAndAgency _ (MsgRequest query') ->
        case tagEq (tagFromQuery query) (tagFromQuery query') of
          Just (Refl, Refl, Refl) -> queryEq query query'
          Nothing -> False
      _ -> False
    (ServerAgency (TokNext _ tag), MsgReject err) -> \case
      AnyMessageAndAgency (ServerAgency (TokNext _ tag')) (MsgReject err') ->
        case tagEq tag tag' of
          Just (Refl, Refl, Refl) -> errEq tag err err'
          Nothing -> False
      _ -> False
    (ServerAgency (TokNext _ tag), MsgNextPage result mDelimiter) -> \case
      AnyMessageAndAgency (ServerAgency (TokNext _ tag')) (MsgNextPage result' mDelimiter') ->
        case tagEq tag tag' of
          Just (Refl, Refl, Refl) -> resultEq tag result result' && case (mDelimiter, mDelimiter') of
            (Nothing, Nothing) -> True
            (Just delimiter, Just delimiter') -> delimiterEq tag delimiter delimiter'
            _ -> False
          Nothing -> False
      _ -> False
    (ClientAgency (TokPage tag), MsgRequestNext delimiter) -> \case
      AnyMessageAndAgency (ClientAgency (TokPage tag')) (MsgRequestNext delimiter') ->
        case tagEq tag tag' of
          Just (Refl, Refl, Refl) -> delimiterEq tag delimiter delimiter'
          Nothing -> False
      _ -> False
    (_, MsgDone) -> \case
      AnyMessageAndAgency _ MsgDone -> True
      _ -> False

class IsQuery query => ShowQuery query where
  showsPrecTag :: Int -> Tag query delimiter err result -> ShowS
  showsPrecQuery :: Int -> query delimiter err result -> ShowS
  showsPrecDelimiter :: Int -> Tag query delimiter err result -> delimiter -> ShowS
  showsPrecErr :: Int -> Tag query delimiter err result -> err -> ShowS
  showsPrecResult :: Int -> Tag query delimiter err result -> result -> ShowS

instance ShowQuery query => ShowProtocol (Query query) where
  showsPrecMessage p agency = \case
    MsgRequest query -> showParen (p >= 11)
      ( showString "MsgRequest"
      . showSpace
      . showsPrecQuery 11 query
      )
    MsgReject err -> showParen (p >= 11)
      ( showString "MsgReject"
      . showSpace
      . case agency of ServerAgency (TokNext _ tag) -> showsPrecErr 11 tag err
      )
    MsgNextPage result mDelimiter -> showParen (p >= 11)
      ( showString "MsgNextPage"
      . showSpace
      . case agency of ServerAgency (TokNext _ tag) -> showsPrecResult 11 tag result
      . showSpace
      . case mDelimiter of
          Nothing -> showString "Nothing"
          Just delimiter -> showParen True
            ( showString "Just"
            . showSpace
            . case agency of ServerAgency (TokNext _ tag) -> showsPrecDelimiter 11 tag delimiter
            )
      )
    MsgRequestNext delimiter -> showParen (p >= 11)
      ( showString "MsgRequestNext"
      . showSpace
      . case agency of ClientAgency (TokPage tag) -> showsPrecDelimiter 11 tag delimiter
      )
    MsgDone -> showString "MsgDone"

  showsPrecServerHasAgency p = \case
    TokNext k tag -> showParen (p >= 11)
      ( showString "TokNext"
      . showSpace
      . showsPrec 11 k
      . showSpace
      . showsPrecTag p tag
      )

  showsPrecClientHasAgency p = \case
    TokInit -> showString "TokInit"
    TokPage tag -> showParen (p >= 11)
      ( showString "TokPage"
      . showSpace
      . showsPrecTag p tag
      )
