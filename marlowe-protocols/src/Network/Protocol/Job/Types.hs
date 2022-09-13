{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | The type of the job protocol.
--
-- The job protocol is used to execute commands as jobs. Job status can be
-- polled by the client while it is running. A client can also attach to a
-- running job and poll its status. When a job completes, it must either report
-- a success or failure.

module Network.Protocol.Job.Types
  where

import Data.Binary (Put)
import Data.Binary.Get (Get)
import Data.Type.Equality (type (:~:))
import Network.TypedProtocol

data SomeTag cmd = forall status err result. SomeTag (Tag cmd status err result)

-- | A class for commands. Defines associated types and conversion
-- functions needed to run the protocol.
class Command (cmd :: * -> * -> * -> *) where

  -- | The type of job IDs for this command type.
  data JobId cmd :: * -> * -> * -> *

  -- | The type of tags for this command type. Used exclusively for GADT
  -- pattern matching.
  data Tag cmd :: * -> * -> * -> *

  -- | Obtain a token from a command.
  tagFromCommand :: cmd status err result -> Tag cmd status err result

  -- | Obtain a token from a command ID.
  tagFromJobId :: JobId cmd status err result -> Tag cmd status err result

  -- | Prove that two tags are the same.
  tagEq :: Tag cmd status err result -> Tag cmd status' err' result' -> Maybe (status :~: status', err :~: err', result :~: result')

  putTag :: Tag cmd status err result -> Put
  getTag :: Get (SomeTag cmd)
  putJobId :: JobId cmd status err result -> Put
  getJobId :: Tag cmd status err result -> Get (JobId cmd status err result)
  putCommand :: cmd status err result -> Put
  getCommand :: Tag cmd status err result -> Get (cmd status err result)
  putStatus :: Tag cmd status err result -> status -> Put
  getStatus :: Tag cmd status err result -> Get status
  putErr :: Tag cmd status err result -> err -> Put
  getErr :: Tag cmd status err result -> Get err
  putResult :: Tag cmd status err result -> result -> Put
  getResult :: Tag cmd status err result -> Get result

-- | A state kind for the job protocol.
data Job (cmd :: * -> * -> * -> *) where

  -- | The initial state of the protocol.
  StInit :: Job cmd

  -- | In the 'StCmd' state, the server has agency. It is running a command
  -- sent by the client and starting the job.
  StCmd :: status -> err -> result -> Job cmd

  -- | In the 'StAttach' state, the server has agency. It is looking up the job
  -- associated with the given job ID.
  StAttach :: status -> err -> result -> Job cmd

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
      ('StAttach status err result)

    -- | Attaching to the job succeeded.
    MsgAttached :: Message (Job cmd)
      ('StAttach status err result)
      ('StCmd status err result)

    -- | Attaching to the job failed.
    MsgAttachFailed :: Message (Job cmd)
      ('StAttach status err result)
      'StDone

    -- | Tell the client the job failed.
    MsgFail :: err -> Message (Job cmd)
      ('StCmd status err result)
      'StDone

    -- | Tell the client the job succeeded.
    MsgSucceed :: result -> Message (Job cmd)
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
    TokAwait :: Tag cmd status err result -> ClientHasAgency ('StAwait status err result)

  data ServerHasAgency st where
    TokCmd :: Tag cmd status err result -> ServerHasAgency ('StCmd status err result)
    TokAttach :: Tag cmd status err result -> ServerHasAgency ('StAttach status err result)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit      = \case
  exclusionLemma_ClientAndServerHaveAgency (TokAwait _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case
