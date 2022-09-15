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
module Network.Protocol.Query.Types where

import Data.Binary (Get, Put)
import Data.Type.Equality (type (:~:))
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
    TokPage :: Tag query delimiter err results -> ClientHasAgency ('StPage delimiter err results)

  data ServerHasAgency st where
    TokNext :: TokNextKind k -> Tag query delimiter err results -> ServerHasAgency ('StNext k delimiter err results)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit     = \case
  exclusionLemma_ClientAndServerHaveAgency (TokPage _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data TokNextKind k where
  TokCanReject :: TokNextKind 'CanReject
  TokMustReply :: TokNextKind 'MustReply
