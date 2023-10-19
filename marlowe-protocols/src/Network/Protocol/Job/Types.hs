{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | The type of the job protocol.
--
-- The job protocol is used to execute commands as jobs. Job status can be
-- polled by the client while it is running. A client can also attach to a
-- running job and poll its status. When a job completes, it must either report
-- a success or failure.
module Network.Protocol.Job.Types where

import Control.Monad (join)
import Data.Binary (Binary (..), Put, getWord8, putWord8)
import Data.Binary.Get (Get)
import Data.Foldable (fold)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.Kind (Type)
import Data.List.NonEmpty
import Data.Maybe (catMaybes)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Data.Type.Equality (TestEquality (..), type (:~:) (Refl))
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
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency (..))
import Test.QuickCheck (Arbitrary (..), Gen, oneof)

data SomeTag k where
  SomeTag :: Tag (t :: k) -> SomeTag k

class TagKind k where
  data Tag :: k -> Type
  data JobId :: k -> Type
  data Command :: k -> Type
  data Status :: k -> Type
  data JobResult :: k -> Type
  data JobError :: k -> Type
  withSingTag :: Tag (t :: k) -> ((SingTag t) => a) -> a
  commandTag :: Command (t :: k) -> Tag t
  jobIdTag :: JobId (t :: k) -> Tag t
  fromTag :: Tag (t :: k) -> k
  toTag :: k -> SomeTag k

class (TagKind k) => SingTag (t :: k) where
  singTag :: Tag t

fromSomeTag :: (TagKind k) => SomeTag k -> k
fromSomeTag (SomeTag t) = fromTag t

-- | A state kind for the job protocol.
data Job k where
  -- | The initial state of the protocol.
  StInit :: Job k
  -- | In the 'StCmd' state, the server has agency. It is running a command
  -- sent by the client and starting the job.
  StCmd :: k -> Job k
  -- | In the 'StAttach' state, the server has agency. It is looking up the job
  -- associated with the given job ID.
  StAttach :: k -> Job k
  -- | In the 'StAwait state, the client has agency. It has been previously
  -- told to await a job execution and can either poll the status or detach.
  StAwait :: k -> Job k
  -- | The terminal state of the protocol.
  StDone :: Job k

instance SingClientHasAgency 'StInit where singClientHasAgency = TokInit
instance (SingTag t) => SingClientHasAgency ('StAwait t) where singClientHasAgency = TokAwait singTag
instance (SingTag t) => SingServerHasAgency ('StCmd t) where singServerHasAgency = TokCmd singTag
instance (SingTag t) => SingServerHasAgency ('StAttach t) where singServerHasAgency = TokAttach singTag
instance SingNobodyHasAgency 'StDone where singNobodyHasAgency = TokDone

instance (HasSignature k) => HasSignature (Job k) where
  signature _ = T.intercalate " " ["Job", signature $ Proxy @k]

instance Protocol (Job k) where
  -- \| The type of messages in the protocol. Corresponds to state transition in
  -- the state machine diagram of the protocol.
  data Message (Job k) from to where
    -- \| Tell the server to execute a command in a new job.
    MsgExec
      :: Command (t :: k)
      -> Message (Job k) 'StInit ('StCmd t)
    -- \| Attach to the job of previously executed command.
    MsgAttach
      :: JobId (t :: k)
      -> Message (Job k) 'StInit ('StAttach t)
    -- \| Attaching to the job succeeded.
    MsgAttached
      :: Message (Job k) ('StAttach (t :: k)) ('StCmd t)
    -- \| Attaching to the job failed.
    MsgAttachFailed
      :: Message (Job k) ('StAttach (t :: k)) 'StDone
    -- \| Tell the client the job failed.
    MsgFail
      :: JobError (t :: k)
      -> Message (Job k) ('StCmd t) 'StDone
    -- \| Tell the client the job succeeded.
    MsgSucceed
      :: JobResult (t :: k)
      -> Message (Job k) ('StCmd t) 'StDone
    -- \| Tell the client the job is in progress.
    MsgAwait
      :: Status (t :: k)
      -> JobId t
      -> Message (Job k) ('StCmd t) ('StAwait t)
    -- \| Ask the server for the current status of the job.
    MsgPoll
      :: Message (Job k) ('StAwait (t :: k)) ('StCmd t)
    -- \| Detach from the session and close the protocol.
    MsgDetach
      :: Message (Job k) ('StAwait (t :: k)) 'StDone

  data ClientHasAgency st where
    TokInit :: ClientHasAgency 'StInit
    TokAwait :: Tag (t :: k) -> ClientHasAgency ('StAwait t :: Job k)

  data ServerHasAgency st where
    TokCmd :: Tag (t :: k) -> ServerHasAgency ('StCmd t :: Job k)
    TokAttach :: Tag (t :: k) -> ServerHasAgency ('StAttach t :: Job k)

  data NobodyHasAgency st where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokInit = \case {}
  exclusionLemma_ClientAndServerHaveAgency (TokAwait _) = \case {}

  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case {}

  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case {}

class (Binary k, TagKind k) => BinaryTagKind k where
  putCommand :: Command (t :: k) -> Put
  getCommand :: Tag (t :: k) -> Get (Command t)
  putJobResult :: JobResult (t :: k) -> Put
  getJobResult :: Tag (t :: k) -> Get (JobResult t)
  putJobError :: JobError (t :: k) -> Put
  getJobError :: Tag (t :: k) -> Get (JobError t)
  putStatus :: Status (t :: k) -> Put
  getStatus :: Tag (t :: k) -> Get (Status t)
  putJobId :: JobId (t :: k) -> Put
  getJobId :: Tag (t :: k) -> Get (JobId t)

instance (BinaryTagKind k) => Binary (SomeTag k) where
  put = put . fromSomeTag
  get = toTag <$> get

instance (BinaryTagKind k) => BinaryMessage (Job k) where
  putMessage = \case
    ClientAgency TokInit -> \case
      MsgExec cmd -> do
        putWord8 0x01
        put $ SomeTag $ commandTag cmd
        putCommand cmd
      MsgAttach jobId -> do
        putWord8 0x02
        put $ SomeTag $ jobIdTag jobId
        putJobId jobId
    ServerAgency TokCmd{} -> \case
      MsgFail err -> do
        putWord8 0x03
        putJobError err
      MsgSucceed result -> do
        putWord8 0x04
        putJobResult result
      MsgAwait status jobId -> do
        putWord8 0x05
        putStatus status
        putJobId jobId
    ClientAgency (TokAwait _) -> \case
      MsgPoll -> putWord8 0x06
      MsgDetach -> putWord8 0x07
    ServerAgency (TokAttach _) -> \case
      MsgAttached -> putWord8 0x08
      MsgAttachFailed -> putWord8 0x09

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokInit -> do
          SomeTag ctag <- get
          SomeMessage . MsgExec <$> getCommand ctag
        _ -> fail "Invalid protocol state for MsgExec"
      0x02 -> case tok of
        ClientAgency TokInit -> do
          SomeTag ctag <- get
          SomeMessage . MsgAttach <$> getJobId ctag
        _ -> fail "Invalid protocol state for MsgAttach"
      0x03 -> case tok of
        ServerAgency (TokCmd ctag) -> SomeMessage . MsgFail <$> getJobError ctag
        _ -> fail "Invalid protocol state for MsgFail"
      0x04 -> case tok of
        ServerAgency (TokCmd ctag) -> SomeMessage . MsgSucceed <$> getJobResult ctag
        _ -> fail "Invalid protocol state for MsgSucceed"
      0x05 -> case tok of
        ServerAgency (TokCmd ctag) -> SomeMessage <$> (MsgAwait <$> getStatus ctag <*> getJobId ctag)
        _ -> fail "Invalid protocol state for MsgAwait"
      0x06 -> case tok of
        ClientAgency (TokAwait _) -> pure $ SomeMessage MsgPoll
        _ -> fail "Invalid protocol state for MsgPoll"
      0x07 -> case tok of
        ClientAgency (TokAwait _) -> pure $ SomeMessage MsgDetach
        _ -> fail "Invalid protocol state for MsgDetach"
      0x08 -> case tok of
        ServerAgency (TokAttach _) -> pure $ SomeMessage MsgAttached
        _ -> fail "Invalid protocol state for MsgAttached"
      0x09 -> case tok of
        ServerAgency (TokAttach _) -> pure $ SomeMessage MsgAttachFailed
        _ -> fail "Invalid protocol state for MsgAttachFailed"
      _ -> fail $ "Invalid msg tag " <> show tag

class (Arbitrary k, TagKind k) => ArbitraryTagKind k where
  arbitraryCommand :: Tag (t :: k) -> Gen (Command t)
  shrinkCommand :: Command (t :: k) -> [Command t]
  arbitraryJobError :: Tag (t :: k) -> Maybe (Gen (JobError t))
  shrinkJobError :: JobError (t :: k) -> [JobError t]
  arbitraryJobResult :: Tag (t :: k) -> Gen (JobResult t)
  shrinkJobResult :: JobResult (t :: k) -> [JobResult t]
  arbitraryStatus :: Tag (t :: k) -> Maybe (Gen (Status t))
  shrinkStatus :: Status (t :: k) -> [Status t]
  arbitraryJobId :: Tag (t :: k) -> Maybe (Gen (JobId t))
  shrinkJobId :: JobId (t :: k) -> [JobId t]

instance (ArbitraryTagKind k) => Arbitrary (SomeTag k) where
  arbitrary = toTag <$> arbitrary
  shrink = fmap toTag . shrink . fromSomeTag

instance (ArbitraryTagKind k) => ArbitraryMessage (Job k) where
  arbitraryMessage = do
    SomeTag tag <- arbitrary
    let mJobError = arbitraryJobError tag
    let mStatus = arbitraryStatus tag
    let mJobId = arbitraryJobId tag
    oneof $
      catMaybes
        [ Just $ AnyMessageAndAgency (ClientAgency TokInit) . MsgExec <$> arbitraryCommand tag
        , mJobId <&> \genJobId -> do
            jobId <- genJobId
            pure $ AnyMessageAndAgency (ClientAgency TokInit) $ MsgAttach jobId
        , Just $ pure $ AnyMessageAndAgency (ServerAgency (TokAttach tag)) MsgAttached
        , Just $ pure $ AnyMessageAndAgency (ServerAgency (TokAttach tag)) MsgAttachFailed
        , mJobError <&> \genErr -> do
            err <- genErr
            pure $ AnyMessageAndAgency (ServerAgency $ TokCmd tag) $ MsgFail err
        , Just $ AnyMessageAndAgency (ServerAgency $ TokCmd tag) . MsgSucceed <$> arbitraryJobResult tag
        , ((,) <$> mStatus <*> mJobId) <&> \(genStatus, genJobId) -> do
            status <- genStatus
            jobId <- genJobId
            pure $ AnyMessageAndAgency (ServerAgency $ TokCmd tag) $ MsgAwait status jobId
        , Just $ pure $ AnyMessageAndAgency (ClientAgency (TokAwait tag)) MsgPoll
        , Just $ pure $ AnyMessageAndAgency (ClientAgency (TokAwait tag)) MsgDetach
        ]
  shrinkMessage _ = \case
    MsgExec cmd -> MsgExec <$> shrinkCommand cmd
    MsgAttach jobId -> MsgAttach <$> shrinkJobId jobId
    MsgFail err -> MsgFail <$> shrinkJobError err
    MsgSucceed result -> MsgSucceed <$> shrinkJobResult result
    MsgAwait status jobId ->
      []
        <> [MsgAwait status' jobId | status' <- shrinkStatus status]
        <> [MsgAwait status jobId' | jobId' <- shrinkJobId jobId]
    _ -> []

class (Variations k, TagKind k) => VariationsTagKind k where
  commandVariations :: Tag (t :: k) -> NonEmpty (Command t)
  errorVariations :: Tag (t :: k) -> [JobError t]
  resultVariations :: Tag (t :: k) -> NonEmpty (JobResult t)
  statusVariations :: Tag (t :: k) -> [Status t]
  jobIdVariations :: Tag (t :: k) -> [JobId t]

instance (VariationsTagKind k) => Variations (SomeTag k) where
  variations = toTag <$> variations

instance (VariationsTagKind k) => MessageVariations (Job k) where
  agencyVariations =
    join
      [ pure $ SomePeerHasAgency $ ClientAgency TokInit
      , do
          SomeTag tag <- variations
          pure $ SomePeerHasAgency $ ClientAgency $ TokAwait tag
      , do
          SomeTag tag <- variations
          pure $ SomePeerHasAgency $ ServerAgency $ TokCmd tag
      , do
          SomeTag tag <- variations
          pure $ SomePeerHasAgency $ ServerAgency $ TokAttach tag
      ]
  messageVariations = \case
    ClientAgency TokInit ->
      let execs = do
            SomeTag tag <- variations
            cmd <- commandVariations tag
            pure $ SomeMessage $ MsgExec cmd
       in case execs of
            msg :| msgs ->
              msg
                :| msgs <> do
                  SomeTag tag <- toList variations
                  jobId <- jobIdVariations tag
                  pure $ SomeMessage $ MsgAttach jobId
    ClientAgency (TokAwait _) -> [SomeMessage MsgPoll, SomeMessage MsgDetach]
    ServerAgency (TokAttach _) -> [SomeMessage MsgAttached, SomeMessage MsgAttachFailed]
    ServerAgency (TokCmd tag) -> case SomeMessage . MsgSucceed <$> resultVariations tag of
      msg :| msgs ->
        msg
          :| msgs
            <> join
              [ SomeMessage . MsgFail <$> errorVariations tag
              , SomeMessage <$> case (statusVariations tag, jobIdVariations tag) of
                  (status : statuses, jobId : jobIds) ->
                    MsgAwait status jobId
                      : fold @[]
                        [ MsgAwait status <$> jobIds
                        , flip MsgAwait jobId <$> statuses
                        ]
                  _ -> []
              ]

class
  ( TagKind k
  , Eq k
  , TestEquality (Tag :: k -> Type)
  , forall (t :: k). Eq (Tag t)
  , forall (t :: k). Eq (Command t)
  , forall (t :: k). Eq (JobResult t)
  , forall (t :: k). Eq (JobError t)
  , forall (t :: k). Eq (Status t)
  , forall (t :: k). Eq (JobId t)
  ) =>
  EqTagKind k

instance (EqTagKind k) => Eq (SomeTag k) where
  (==) = on (==) fromSomeTag

deriving instance (EqTagKind k) => Eq (Message (Job k) st st')
deriving instance (EqTagKind k) => Eq (ClientHasAgency (st :: Job k))
deriving instance (EqTagKind k) => Eq (ServerHasAgency (st :: Job k))
deriving instance (EqTagKind k) => Eq (NobodyHasAgency (st :: Job k))

instance (EqTagKind k) => TestEquality (ClientHasAgency :: Job k -> Type) where
  testEquality = \case
    TokInit -> \case
      TokInit -> Just Refl
      _ -> Nothing
    TokAwait t -> \case
      TokAwait t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing

instance (EqTagKind k) => TestEquality (ServerHasAgency :: Job k -> Type) where
  testEquality = \case
    TokCmd t -> \case
      TokCmd t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing
    TokAttach t -> \case
      TokAttach t' -> case testEquality t t' of
        Just Refl -> Just Refl
        Nothing -> Nothing
      _ -> Nothing

instance TestEquality (NobodyHasAgency :: Job k -> Type) where
  testEquality TokDone TokDone = Just Refl

instance (EqTagKind k) => TestMessageEquality (Job k) where
  testMessageEquality (ClientAgency tok) (ClientAgency tok') msg msg' = case (tok, tok') of
    (TokInit, TokInit) -> case (msg, msg') of
      (MsgExec cmd, MsgExec cmd') -> do
        Refl <- testEquality (commandTag cmd) (commandTag cmd')
        pure (Refl, Refl, Refl)
      (MsgExec{}, _) -> Nothing
      (MsgAttach jobId, MsgAttach jobId') -> do
        Refl <- testEquality (jobIdTag jobId) (jobIdTag jobId')
        pure (Refl, Refl, Refl)
      (MsgAttach{}, _) -> Nothing
    (TokInit, _) -> Nothing
    (TokAwait t, TokAwait t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (MsgPoll, MsgPoll) -> Just (Refl, Refl, Refl)
        (MsgPoll, _) -> Nothing
        (MsgDetach, MsgDetach) -> Just (Refl, Refl, Refl)
        (MsgDetach, _) -> Nothing
    (TokAwait{}, _) -> Nothing
  testMessageEquality (ServerAgency tok) (ServerAgency tok') msg msg' = case (tok, tok') of
    (TokCmd t, TokCmd t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (MsgFail{}, MsgFail{}) -> Just (Refl, Refl, Refl)
        (MsgFail{}, MsgSucceed{}) -> Just (Refl, Refl, Refl)
        (MsgFail{}, _) -> Nothing
        (MsgSucceed{}, MsgSucceed{}) -> Just (Refl, Refl, Refl)
        (MsgSucceed{}, MsgFail{}) -> Just (Refl, Refl, Refl)
        (MsgSucceed{}, _) -> Nothing
        (MsgAwait{}, MsgAwait{}) -> Just (Refl, Refl, Refl)
        (MsgAwait{}, _) -> Nothing
    (TokCmd{}, _) -> Nothing
    (TokAttach t, TokAttach t') -> do
      Refl <- testEquality t t'
      case (msg, msg') of
        (MsgAttached{}, MsgAttached{}) -> Just (Refl, Refl, Refl)
        (MsgAttached{}, _) -> Nothing
        (MsgAttachFailed{}, MsgAttachFailed{}) -> Just (Refl, Refl, Refl)
        (MsgAttachFailed{}, _) -> Nothing
    (TokAttach{}, _) -> Nothing
  testMessageEquality _ _ _ _ = Nothing

instance (EqTagKind k) => MessageEq (Job k)

class
  ( Show k
  , TagKind k
  , forall (t :: k). Show (Tag t)
  , forall (t :: k). Show (Command t)
  , forall (t :: k). Show (JobResult t)
  , forall (t :: k). Show (JobError t)
  , forall (t :: k). Show (Status t)
  , forall (t :: k). Show (JobId t)
  ) =>
  ShowTagKind k

deriving instance (ShowTagKind k) => Show (Message (Job k) st st')
deriving instance (ShowTagKind k) => Show (ClientHasAgency (st :: Job k))
deriving instance (ShowTagKind k) => Show (ServerHasAgency (st :: Job k))
deriving instance (ShowTagKind k) => Show (NobodyHasAgency (st :: Job k))

instance (ShowTagKind k) => ShowProtocol (Job k)
