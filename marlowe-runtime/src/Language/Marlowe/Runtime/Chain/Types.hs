module Language.Marlowe.Runtime.Chain.Types where

import Cardano.Api (AddressInEra, BlockHeader, BlockInMode, CardanoMode, ChainPoint, ChainSyncClient, ChainTip,
                    PolicyId, ScriptData, TxId, TxIn, Value)
import qualified Data.Aeson as Aeson
import Data.Time (NominalDiffTime, UTCTime)

data MarloweChainEvent era
  = MarloweRollForward BlockHeader [MarloweTx era] ChainTip
  | MarloweRollBackward ChainPoint ChainTip

data MarloweTx era = MarloweTx
  { marloweTx_id       :: TxId
  , marloweTx_policies :: [PolicyId]
  , marloweTx_interval :: Maybe MarloweValidityInterval
  , marloweTx_metadata :: Maybe Aeson.Value
  , marloweTx_inputs   :: [TxIn]
  , marloweTx_outputs  :: [MarloweTxOut era]
  }
  deriving (Show, Eq)

data MarloweTxOut era = MarloweTxOut
  { marloweTxOut_txIn    :: TxIn
  , marloweTxOut_address :: AddressInEra era
  , marloweTxOut_value   :: Value
  , marloweTxOut_datum   :: Maybe ScriptData
  }
  deriving (Show, Eq)

type MarloweChainSyncClient = ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip

data MarloweValidityInterval = MarloweValidityInterval
  { startTime :: UTCTime
  , duration  :: NominalDiffTime
  }
  deriving (Show, Eq)
