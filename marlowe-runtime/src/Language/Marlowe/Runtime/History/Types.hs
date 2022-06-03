{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE EmptyDataDeriving          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StrictData                 #-}
module Language.Marlowe.Runtime.History.Types where

import Cardano.Api (AsType (AsHash, AsScriptData, AsScriptHash), Hash, ScriptData, ScriptHash, Value, serialiseToJSON)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary (..))
import GHC.Generics (Generic)
import Language.Marlowe (Contract)
import Language.Marlowe.Runtime.Chain.Types (MarloweAddress (..), MarloweBlockHeader, MarlowePolicyId, MarloweTxId,
                                             MarloweTxOut, TxOutRef, getFromRawBytes, putToRawBytes)
import Language.Marlowe.Semantics (MarloweData)
import Type.Reflection (Typeable)

data Event = Event
  { contractId   :: ContractId
  , blockHeader  :: MarloweBlockHeader
  , txId         :: MarloweTxId
  , historyEvent :: HistoryEvent
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary Event

data HistoryEvent
  = ContractWasCreated ContractCreationTxOut
  | InputsWereApplied
      { appTxOut :: Maybe AppTxOutRef
      , inputs   :: [Input]
      }
  | RoleWasPaidOut
      { tokenName   :: String
      , assets      :: Assets
      , payoutTxOut :: TxOutRef
      }
  deriving (Generic, Typeable, Show, Eq)

instance Binary HistoryEvent

data Input
  = AssetsWereDeposited
      { continuation :: Maybe ContractContinuation
      , intoAccount  :: Account
      , fromParty    :: Participant
      , assets       :: Assets
      }
  | ChoiceWasMade
      { continuation :: Maybe ContractContinuation
      , choice       :: Choice
      , selection    :: ChoiceSelection
      }
  | NotifyWasMade
      { continuation :: Maybe ContractContinuation
      }
  deriving (Generic, Typeable, Show, Eq)

instance Binary Input

data ContractCreationTxOut = ContractCreationTxOut
  { contractId           :: ContractId
  , datum                :: Datum
  , txOut                :: MarloweTxOut
  , header               :: MarloweBlockHeader
  , roleValidatorAddress :: MarloweAddress
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ContractCreationTxOut

data AppTxOutRef = AppTxOutRef
  { txOutRef :: TxOutRef
  , datum    :: Datum
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary AppTxOutRef

data Choice = Choice
  { name        :: String
  , participant :: Participant
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary Choice

newtype ChoiceSelection = ChoiceSelection Integer
  deriving (Generic, Typeable, Show, Eq)

instance Binary ChoiceSelection

newtype Datum = Datum MarloweData
  deriving (Generic, Typeable, Show, Eq)

instance Binary Datum where
  put (Datum datum) = put $ serialiseToJSON  datum
  get = do
    bytes <- get
    Datum <$> case Aeson.eitherDecode bytes of
      Left err -> fail err
      Right a  -> pure a

data Participant
  = WalletParticipant MarloweAddress
  | RoleParticipant String
  deriving (Generic, Typeable, Show, Eq)

instance Binary Participant

data Account
  = AddressAccount MarloweAddress
  | RoleAccount String
  deriving (Generic, Typeable, Show, Eq)

instance Binary Account

data ContractContinuation = ContractContinuation
  { continuationHash :: ContinuationHash
  , continuation     :: Contract
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary ContractContinuation where
  put ContractContinuation{..} = do
    put continuationHash
    put $ serialiseToJSON continuation
  get = do
    continuationHash <- get
    continuationBytes <- get
    continuation <- case Aeson.eitherDecode continuationBytes of
      Left err -> fail err
      Right a  -> pure a
    pure ContractContinuation{..}

data ContractId = ContractId
  { currencySymbol      :: MarlowePolicyId
  , payoutValidatorHash :: ValidatorHash
  }
  deriving (Generic, Typeable, Show, Eq, Ord)

instance Binary ContractId

newtype ContinuationHash = ContinuationHash (Hash ScriptData)
  deriving (Generic, Typeable, Show, Eq)

instance Binary ContinuationHash where
  put (ContinuationHash hash) = putToRawBytes hash
  get = ContinuationHash <$> getFromRawBytes (AsHash AsScriptData)

newtype Assets = Assets Value
  deriving (Generic, Typeable, Show, Eq)

instance Binary Assets  where
  put (Assets value) = put $ serialiseToJSON value
  get = do
    bytes <- get
    Assets <$> case Aeson.eitherDecode bytes of
      Left err -> fail err
      Right a  -> pure a

newtype ValidatorHash = ValidatorHash ScriptHash
  deriving (Generic, Typeable, Show, Eq, Ord)

instance Binary ValidatorHash where
  put (ValidatorHash hash) = putToRawBytes hash
  get = ValidatorHash <$> getFromRawBytes AsScriptHash
