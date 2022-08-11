{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE EmptyCase      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds      #-}
{-# LANGUAGE TypeFamilies   #-}

-- | The type of the job protocol.
--
-- The job protocol is used to execute commands as jobs. Job status can be
-- polled by the client while it is running. A client can also attach to a
-- running job and poll its status. When a job completes, it must either report
-- a success or failure.

module Network.Protocol.Job.Types where

import Network.TypedProtocol

-- | A typeclass for commands. Defines associated types and conversion
-- functions needed to run the protocol.
class Command (cmd :: * -> * -> * -> *) where

  -- | The type of job IDs for this command type.
  data JobId cmd :: * -> * -> * -> *

  -- | The type of tokens for this command type. Used exclusively for GADT
  -- pattern matching.
  data TokCommand cmd :: * -> * -> * -> *

  -- | Obtain a token from a command.
  tokFromCmd :: cmd status err result -> TokCommand cmd status err result

  -- | Obtain a token from a command ID.
  tokFromId :: JobId cmd status err result -> TokCommand cmd status err result

-- | A state kind for the job protocol.
data Job (cmd :: * -> * -> * -> *) where

  -- | The initial state of the protocol.
  StInit :: Job cmd

  -- | In the 'StCmd' state, the server has agency. It is preparing to send the
  -- status of the job associated with a command.
  StCmd :: status -> err -> result -> Job cmd

  -- | In the 'StAwait state, the client has agency. It has been previously
  -- told to await a job execution and can either poll the status or detach.
  StAwait :: status -> err -> result -> Job cmd

  -- | The terminal state of the protocol.
  StDone :: Job cmd

instance Protocol (Job cmd) where

  -- | The type of messages in the protocol. Corresponds to state transition in
  -- the state machine diagram of the protocol.
  data Message (Job cmd) from to where

    -- | Tell the server to execute a command in a new job.
    MsgExec :: cmd status err result -> Message (Job cmd)
      'StInit
      ('StCmd status err result)

    -- | Attach to the job of previously executed command.
    MsgAttach :: JobId cmd status err result -> Message (Job cmd)
      'StInit
      ('StCmd status err result)

    -- | Tell the client the job failed.
    MsgFail :: TokCommand cmd status err result -> err -> Message (Job cmd)
      ('StCmd status err result)
      'StDone

    -- | Tell the client the job succeeded.
    MsgSucceed :: TokCommand cmd status err result -> result -> Message (Job cmd)
      ('StCmd status err result)
      'StDone

    -- | Tell the client the job is in progress.
    MsgAwait :: status -> JobId cmd status err result -> Message (Job cmd)
      ('StCmd status err result)
      ('StAwait status err result)

    -- | Ask the server for the current status of the job.
    MsgPoll :: Message (Job cmd)
      ('StAwait status err result)
      ('StCmd status err result)

    -- | Detach from the session and close the protocol.
    MsgDetach :: Message (Job cmd)
      ('StAwait status err result)
      'StDone

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokAwait :: TokCommand cmd status err result -> ClientHasAgency ('StAwait status err result)

  data ServerHasAgency st where
    TokCmd :: TokCommand cmd status err result -> ServerHasAgency ('StCmd status err result)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit      = \case
  exclusionLemma_ClientAndServerHaveAgency (TokAwait _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case
