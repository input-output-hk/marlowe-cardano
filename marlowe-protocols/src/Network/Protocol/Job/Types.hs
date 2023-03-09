{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
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

import Control.Monad (join)
import Data.Aeson (Value(..), object, (.=))
import Data.Binary (Put, getWord8, putWord8)
import Data.Binary.Get (Get)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import Data.List.NonEmpty
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy(..))
import qualified Data.Text as T
import Data.Type.Equality (type (:~:)(Refl))
import GHC.Show (showSpace)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Codec.Spec
  (ArbitraryMessage(..), MessageEq(..), MessageVariations(..), ShowProtocol(..), SomePeerHasAgency(..))
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event.Network.Protocol (MessageToJSON(..))
import Test.QuickCheck (Gen, oneof)

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

class Command cmd => CommandToJSON cmd where
  commandToJSON :: cmd status err result -> Value
  jobIdToJSON :: JobId cmd status err result -> Value
  errToJSON :: Tag cmd status err result -> err -> Value
  resultToJSON :: Tag cmd status err result -> result -> Value
  statusToJSON :: Tag cmd status err result -> status -> Value

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

instance HasSignature cmd => HasSignature (Job cmd) where
  signature _ = T.intercalate " " ["Job", signature $ Proxy @cmd]

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
    TokAwait :: Tag cmd status err result -> ClientHasAgency ('StAwait status err result :: Job cmd)

  data ServerHasAgency st where
    TokCmd :: Tag cmd status err result -> ServerHasAgency ('StCmd status err result :: Job cmd)
    TokAttach :: Tag cmd status err result -> ServerHasAgency ('StAttach status err result :: Job cmd)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit      = \case
  exclusionLemma_ClientAndServerHaveAgency (TokAwait _) = \case

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

instance Command cmd => BinaryMessage (Job cmd) where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgExec cmd -> do
        putWord8 0x01
        let tag = tagFromCommand cmd
        putTag tag
        putCommand cmd
      MsgAttach jobId -> do
        putWord8 0x02
        let tag = tagFromJobId jobId
        putTag tag
        putJobId jobId
    ServerAgency (TokCmd tag) -> \case
      MsgFail err -> do
        putWord8 0x03
        putTag tag
        putErr tag err
      MsgSucceed result -> do
        putWord8 0x04
        putTag tag
        putResult tag result
      MsgAwait status jobId -> do
        putWord8 0x05
        putTag tag
        putStatus tag status
        putJobId jobId
    ClientAgency (TokAwait _) -> \case
      MsgPoll   -> putWord8 0x06
      MsgDetach -> putWord8 0x07
    ServerAgency (TokAttach _) -> \case
      MsgAttached     -> putWord8 0x08
      MsgAttachFailed -> putWord8 0x09

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> do
          SomeTag ctag <- getTag
          SomeMessage . MsgExec <$> getCommand ctag
        _ -> fail "Invalid protocol state for MsgExec"
      0x02 -> case tok of
        ClientAgency TokInit -> do
          SomeTag ctag <- getTag
          SomeMessage . MsgAttach <$> getJobId ctag
        _ -> fail "Invalid protocol state for MsgAttach"
      0x03 -> case tok of
        ServerAgency (TokCmd ctag) -> do
          SomeTag ctag' <- getTag
          case tagEq ctag ctag' of
            Nothing                 -> fail "decoded command tag does not match expected command tag"
            Just (Refl, Refl, Refl) -> SomeMessage . MsgFail <$> getErr ctag'
        _ -> fail "Invalid protocol state for MsgFail"
      0x04 -> case tok of
        ServerAgency (TokCmd ctag) -> do
          SomeTag ctag' <- getTag
          case tagEq ctag ctag' of
            Nothing                 -> fail "decoded command tag does not match expected command tag"
            Just (Refl, Refl, Refl) -> SomeMessage . MsgSucceed <$> getResult ctag'
        _ -> fail "Invalid protocol state for MsgSucceed"
      0x05 -> case tok of
        ServerAgency (TokCmd ctag) -> do
          SomeTag ctag' <- getTag
          case tagEq ctag ctag' of
            Nothing                 -> fail "decoded command tag does not match expected command tag"
            Just (Refl, Refl, Refl) -> SomeMessage <$> (MsgAwait <$> getStatus ctag' <*> getJobId ctag')
        _ -> fail "Invalid protocol state for MsgAwait"
      0x06 -> case tok of
        ClientAgency (TokAwait _) -> pure $ SomeMessage MsgPoll
        _                         -> fail "Invalid protocol state for MsgPoll"
      0x07 -> case tok of
        ClientAgency (TokAwait _) -> pure $ SomeMessage MsgDetach
        _                         -> fail "Invalid protocol state for MsgDetach"
      0x08 -> case tok of
        ServerAgency (TokAttach _) -> pure $ SomeMessage MsgAttached
        _                          -> fail "Invalid protocol state for MsgAttached"
      0x09 -> case tok of
        ServerAgency (TokAttach _) -> pure $ SomeMessage MsgAttachFailed
        _                          -> fail "Invalid protocol state for MsgAttachFailed"
      _ -> fail $ "Invalid msg tag " <> show tag

instance CommandToJSON cmd => MessageToJSON (Job cmd) where
  messageToJSON = \case
    ClientAgency TokInit -> \case
      MsgExec cmd -> object [ "exec" .= commandToJSON cmd ]
      MsgAttach jobId -> object [ "attach" .= jobIdToJSON jobId ]
    ClientAgency (TokAwait _) -> String . \case
      MsgPoll -> "poll"
      MsgDetach -> "detach"
    ServerAgency (TokCmd tag)-> \case
      MsgFail err -> object [ "fail" .= errToJSON tag err ]
      MsgSucceed result -> object [ "succeed" .= resultToJSON tag result ]
      MsgAwait status jobId -> object
        [ "await" .= object
            [ "status" .= statusToJSON tag status
            , "jobId" .= jobIdToJSON jobId
            ]
        ]
    ServerAgency (TokAttach _) -> String . \case
      MsgAttached -> "attached"
      MsgAttachFailed -> "attachFailed"

class Command cmd => ArbitraryCommand cmd where
  arbitraryTag :: Gen (SomeTag cmd)
  arbitraryCmd :: Tag cmd status err result -> Gen (cmd status err result)
  arbitraryJobId :: Tag cmd status err result -> Maybe (Gen (JobId cmd status err result))
  arbitraryStatus :: Tag cmd status err result -> Maybe (Gen status)
  arbitraryErr :: Tag cmd status err result -> Maybe (Gen err)
  arbitraryResult :: Tag cmd status err result -> Gen result
  shrinkCommand :: cmd status err result -> [cmd status err result]
  shrinkJobId :: JobId cmd status err result -> [JobId cmd status err result]
  shrinkErr :: Tag cmd status err result -> err -> [err]
  shrinkResult :: Tag cmd status err result -> result -> [result]
  shrinkStatus :: Tag cmd status err result -> status -> [status]

class Command cmd => CommandVariations cmd where
  tags :: NonEmpty (SomeTag cmd)
  cmdVariations :: Tag cmd status err result -> NonEmpty (cmd status err result)
  jobIdVariations :: Tag cmd status err result -> [JobId cmd status err result]
  statusVariations :: Tag cmd status err result -> [status]
  errVariations :: Tag cmd status err result -> [err]
  resultVariations :: Tag cmd status err result -> NonEmpty result

instance ArbitraryCommand cmd => ArbitraryMessage (Job cmd) where
  arbitraryMessage = do
    SomeTag tag <- arbitraryTag
    let mError = arbitraryErr tag
    let mStatus = arbitraryStatus tag
    let mJobId = arbitraryJobId tag
    oneof $ catMaybes
      [ Just $ AnyMessageAndAgency (ClientAgency TokInit) . MsgExec <$> arbitraryCmd tag
      , mJobId <&> \genJobId -> do
          jobId <- genJobId
          pure $ AnyMessageAndAgency (ClientAgency TokInit) $ MsgAttach jobId
      , Just $ pure $ AnyMessageAndAgency (ServerAgency (TokAttach tag)) MsgAttached
      , Just $ pure $ AnyMessageAndAgency (ServerAgency (TokAttach tag)) MsgAttachFailed
      , mError <&> \genErr -> do
          err <- genErr
          pure $ AnyMessageAndAgency (ServerAgency $ TokCmd tag) $ MsgFail err
      , Just $ AnyMessageAndAgency (ServerAgency $ TokCmd tag) . MsgSucceed <$> arbitraryResult tag
      , ((,) <$> mStatus <*> mJobId) <&> \(genStatus, genJobId) -> do
          status <- genStatus
          jobId <- genJobId
          pure $ AnyMessageAndAgency (ServerAgency $ TokCmd tag) $ MsgAwait status jobId
      , Just $ pure $ AnyMessageAndAgency (ClientAgency (TokAwait tag)) MsgPoll
      , Just $ pure $ AnyMessageAndAgency (ClientAgency (TokAwait tag)) MsgDetach
      ]
  shrinkMessage agency = \case
    MsgExec cmd -> MsgExec <$> shrinkCommand cmd
    MsgAttach jobId -> MsgAttach <$> shrinkJobId jobId
    MsgFail err -> MsgFail <$> case agency of ServerAgency (TokCmd tag) -> shrinkErr tag err
    MsgSucceed result -> MsgSucceed <$> case agency of ServerAgency (TokCmd tag) -> shrinkResult tag result
    MsgAwait status jobId -> []
      <> [ MsgAwait status' jobId | status' <- case agency of ServerAgency (TokCmd tag) -> shrinkStatus tag status ]
      <> [ MsgAwait status jobId' | jobId' <- shrinkJobId jobId ]
    _ -> []

class Command cmd => CommandEq cmd where
  commandEq :: cmd status err result -> cmd status err result -> Bool
  jobIdEq :: JobId cmd status err result -> JobId cmd status err result -> Bool
  statusEq :: Tag cmd status err result -> status -> status -> Bool
  errEq :: Tag cmd status err result -> err -> err -> Bool
  resultEq :: Tag cmd status err result -> result -> result -> Bool

instance CommandEq cmd => MessageEq (Job cmd) where
  messageEq (AnyMessageAndAgency agency msg) = case (agency, msg) of
    (_, MsgExec cmd) -> \case
      AnyMessageAndAgency _ (MsgExec cmd') ->
        case tagEq (tagFromCommand cmd) (tagFromCommand cmd') of
          Just (Refl, Refl, Refl) -> commandEq cmd cmd'
          Nothing -> False
      _ -> False
    (_, MsgAttach jobId) -> \case
      AnyMessageAndAgency _ (MsgAttach jobId') ->
        case tagEq (tagFromJobId jobId) (tagFromJobId jobId') of
          Just (Refl, Refl, Refl) -> jobIdEq jobId jobId'
          Nothing -> False
      _ -> False
    (_, MsgAttached) -> \case
      AnyMessageAndAgency _ MsgAttached -> True
      _ -> False
    (_, MsgAttachFailed) -> \case
      AnyMessageAndAgency _ MsgAttachFailed -> True
      _ -> False
    (ServerAgency (TokCmd tag), MsgFail err) -> \case
      AnyMessageAndAgency (ServerAgency (TokCmd tag')) (MsgFail err') ->
        case tagEq tag tag' of
          Just (Refl, Refl, Refl) -> errEq tag err err'
          Nothing -> False
      _ -> False
    (ServerAgency (TokCmd tag), MsgSucceed result) -> \case
      AnyMessageAndAgency (ServerAgency (TokCmd tag')) (MsgSucceed result') ->
        case tagEq tag tag' of
          Just (Refl, Refl, Refl) -> resultEq tag result result'
          Nothing -> False
      _ -> False
    (ServerAgency (TokCmd tag), MsgAwait status jobId) -> \case
      AnyMessageAndAgency (ServerAgency (TokCmd tag')) (MsgAwait status' jobId') ->
        case tagEq tag tag' of
          Just (Refl, Refl, Refl) -> statusEq tag status status' && jobIdEq jobId jobId'
          Nothing -> False
      _ -> False
    (_, MsgPoll) -> \case
      AnyMessageAndAgency _ MsgPoll -> True
      _ -> False
    (_, MsgDetach) -> \case
      AnyMessageAndAgency _ MsgDetach -> True
      _ -> False

instance CommandVariations cmd => MessageVariations (Job cmd) where
  agencyVariations = join
    [ pure $ SomePeerHasAgency $ ClientAgency TokInit
    , do
        SomeTag tag <- tags
        pure $ SomePeerHasAgency $ ClientAgency $ TokAwait tag
    , do
        SomeTag tag <- tags
        pure $ SomePeerHasAgency $ ServerAgency $ TokCmd tag
    , do
        SomeTag tag <- tags
        pure $ SomePeerHasAgency $ ServerAgency $ TokAttach tag
    ]
  messageVariations = \case
    ClientAgency TokInit ->
      let
        execs = do
          SomeTag tag <- tags
          cmd <- cmdVariations tag
          pure $ SomeMessage $ MsgExec cmd
      in
        case execs of
          msg :| msgs -> msg :| msgs <> do
            SomeTag tag <- toList tags
            jobId <- jobIdVariations tag
            pure $ SomeMessage $ MsgAttach jobId
    ClientAgency (TokAwait _) -> [SomeMessage MsgPoll, SomeMessage MsgDetach]
    ServerAgency (TokAttach _) -> [SomeMessage MsgAttached, SomeMessage MsgAttachFailed]
    ServerAgency (TokCmd tag) -> case SomeMessage . MsgSucceed <$> resultVariations tag of
      msg :| msgs -> msg :| msgs <> join
        [ SomeMessage . MsgFail <$> errVariations tag
        , SomeMessage <$> case (statusVariations tag, jobIdVariations tag) of
            (status : statuses, jobId : jobIds) -> MsgAwait status jobId : fold @[]
              [ MsgAwait status <$> jobIds
              , flip MsgAwait jobId <$> statuses
              ]
            _ -> []
        ]

class Command cmd => ShowCommand cmd where
  showsPrecTag :: Int -> Tag cmd status err result -> ShowS
  showsPrecCommand :: Int -> cmd status err result -> ShowS
  showsPrecJobId :: Int -> JobId cmd status err result -> ShowS
  showsPrecStatus :: Int -> Tag cmd status err result -> status -> ShowS
  showsPrecErr :: Int -> Tag cmd status err result -> err -> ShowS
  showsPrecResult :: Int -> Tag cmd status err result -> result -> ShowS

instance ShowCommand cmd => ShowProtocol (Job cmd) where
  showsPrecMessage p agency = \case
    MsgExec cmd -> showParen (p >= 11)
      ( showString "MsgExec"
      . showSpace
      . showsPrecCommand 11 cmd
      )
    MsgAttach jobId -> showParen (p >= 11)
      ( showString "MsgAttach"
      . showSpace
      . showsPrecJobId 11 jobId
      )
    MsgAttached -> showString "MsgAttached"
    MsgAttachFailed -> showString "MsgAttachFailed"
    MsgFail err -> showParen (p >= 11)
      ( showString "MsgFail"
      . showSpace
      . case agency of ServerAgency (TokCmd tag) -> showsPrecErr 11 tag err
      )
    MsgSucceed result -> showParen (p >= 11)
      ( showString "MsgSucceed"
      . showSpace
      . case agency of ServerAgency (TokCmd tag) -> showsPrecResult 11 tag result
      )
    MsgAwait status jobId -> showParen (p >= 11)
      ( showString "MsgAwait"
      . showSpace
      . case agency of ServerAgency (TokCmd tag) -> showsPrecStatus 11 tag status
      . showSpace
      . showsPrecJobId 11 jobId
      )
    MsgPoll -> showString "MsgPoll"
    MsgDetach -> showString "MsgDetach"

  showsPrecServerHasAgency p = \case
    TokCmd tag -> showParen (p >= 11)
      ( showString "TokCmd"
      . showSpace
      . showsPrecTag p tag
      )
    TokAttach tag -> showParen (p >= 11)
      ( showString "TokAttach"
      . showSpace
      . showsPrecTag p tag
      )

  showsPrecClientHasAgency p = \case
    TokInit -> showString "TokInit"
    TokAwait tag -> showParen (p >= 11)
      ( showString "TokAwait"
      . showSpace
      . showsPrecTag p tag
      )
