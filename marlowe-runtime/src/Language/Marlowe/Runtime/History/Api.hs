{-# LANGUAGE GADTs        #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Runtime.History.Api where

import Data.Binary (Binary, get, getWord8, put, putWord8)
import qualified Data.ByteString.Lazy as LBS
import Data.Map (Map)
import Data.Void (Void, absurd)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.ChainSync.Api (TxError, TxOutRef, UTxOError)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Core.Api (ContractId, SomeMarloweVersion)
import Network.Protocol.ChainSeek.Codec (DeserializeError)
import Network.Protocol.Job.Client
import Network.Protocol.Job.Codec
import Network.Protocol.Job.Server
import Network.Protocol.Job.Types
import Network.TypedProtocol.Codec

data ContractHistoryError
  = HansdshakeFailed
  | FindTxFailed TxError
  | ExtractContractFailed ExtractCreationError
  | FollowScriptUTxOFailed UTxOError
  | FollowPayoutUTxOsFailed (Map Chain.TxOutRef UTxOError)
  | ExtractMarloweTransactionFailed ExtractMarloweTransactionError
  | PayoutUTxONotFound Chain.TxOutRef
  | CreateTxRolledBack
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Binary

data ExtractCreationError
  = TxIxNotFound
  | ByronAddress
  | NonScriptAddress
  | InvalidScriptHash
  | NoCreateDatum
  | InvalidCreateDatum
  | NotCreationTransaction
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Binary

data ExtractMarloweTransactionError
  = TxInNotFound
  | NoRedeemer
  | InvalidRedeemer
  | NoTransactionDatum
  | InvalidTransactionDatum
  | NoPayoutDatum TxOutRef
  | InvalidPayoutDatum TxOutRef
  | InvalidValidityRange
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass Binary

data FollowerStatus
  = Pending
  | Following SomeMarloweVersion
  | Failed ContractHistoryError
  deriving (Eq, Ord, Show, Generic)
  deriving anyclass Binary

data HistoryCommand status err result where
  FollowContract :: ContractId -> HistoryCommand Void ContractHistoryError Bool
  StopFollowingContract :: ContractId -> HistoryCommand Void Void Bool

instance Command HistoryCommand where
  data JobId HistoryCommand status err result where

  data Tag HistoryCommand status err result where
    TagFollowContract :: Tag HistoryCommand Void ContractHistoryError Bool
    TagStopFollowingContract :: Tag HistoryCommand Void Void Bool

  tagFromCommand = \case
    FollowContract _        -> TagFollowContract
    StopFollowingContract _ -> TagStopFollowingContract

  tagFromJobId = \case

  tagEq = curry \case
    (TagFollowContract, TagFollowContract)               -> Just Refl
    (TagFollowContract, _)                               -> Nothing
    (TagStopFollowingContract, TagStopFollowingContract) -> Just Refl
    (TagStopFollowingContract, _)                        -> Nothing

  putTag = \case
    TagFollowContract        -> putWord8 0x01
    TagStopFollowingContract -> putWord8 0x02

  getTag = do
    tag <- getWord8
    case tag of
      0x01 -> pure $ SomeTag TagFollowContract
      0x02 -> pure $ SomeTag TagStopFollowingContract
      _    -> fail $ "Invalid HistoryCommand tag: " <> show tag

  putCommand = \case
    FollowContract contractId        -> put contractId

    StopFollowingContract contractId -> put contractId

  getCommand = \case
    TagFollowContract        -> FollowContract <$> get
    TagStopFollowingContract -> StopFollowingContract <$> get

  putJobId = \case

  getJobId _ = fail "History commands have no job IDs"

  putStatus = \case
    TagFollowContract        -> absurd
    TagStopFollowingContract -> absurd

  getStatus _ = fail "History commands have no statuses"

  putErr = \case
    TagFollowContract        -> put
    TagStopFollowingContract -> absurd

  getErr = \case
    TagFollowContract        -> get
    TagStopFollowingContract -> fail "StopFollowingContract has no error"

  putResult = \case
    TagFollowContract        -> put
    TagStopFollowingContract -> put

  getResult = \case
    TagFollowContract        -> get
    TagStopFollowingContract -> get

type RuntimeHistoryJob = Job HistoryCommand

type RuntimeHistoryJobClient = JobClient HistoryCommand

type RuntimeHistoryJobServer = JobServer HistoryCommand

type RuntimeHistoryJobCodec m = Codec RuntimeHistoryJob DeserializeError m LBS.ByteString

historyJobCodec :: Applicative m => RuntimeHistoryJobCodec m
historyJobCodec = codecJob
