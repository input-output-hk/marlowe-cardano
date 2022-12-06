{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
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

import Data.Aeson (Value(..), object, (.=))
import Data.Binary (Get, Put)
import Data.Type.Equality (type (:~:))
import GHC.Show (showSpace)
import Network.Protocol.Driver (MessageToJSON(..), ShowMessage(..))
import Network.TypedProtocol

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

class IsQuery q => ShowQuery q where
  showsPrecQuery :: Int -> q delimiter err result -> ShowS
  showsPrecErr :: Int -> Tag q delimiter err result -> err -> ShowS
  showsPrecResult :: Int -> Tag q delimiter err result -> result -> ShowS
  showsPrecDelimiter :: Int -> Tag q delimiter err result -> delimiter -> ShowS

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

instance ShowQuery query => ShowMessage (Query query) where
  showsPrecMessage p = \case
    ClientAgency TokInit -> \case
      MsgRequest q -> showParen (p >= 11)
        ( showString "MsgRequest"
        . showSpace
        . showsPrecQuery 11 q
        )
    ClientAgency (TokPage tag) -> \case
      MsgRequestNext d -> showParen (p >= 11)
        ( showString "MsgRequestNext"
        . showSpace
        . showsPrecDelimiter 11 tag d
        )
      MsgDone -> showString "MsgDone"
    ServerAgency (TokNext _ tag)-> \case
      MsgReject err -> showParen (p >= 11)
        ( showString "MsgReject"
        . showSpace
        . showsPrecErr 11 tag err
        )
      MsgNextPage results d -> showParen (p >= 11)
        ( showString "MsgNextPage"
        . showSpace
        . showsPrecResult 11 tag results
        . showSpace
        . case d of
            Nothing -> showString "Nothing"
            Just d' -> showParen True
              ( showString "Just"
              . showSpace
              . showsPrecDelimiter 11 tag d'
              )
        )
