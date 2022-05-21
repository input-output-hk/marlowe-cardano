{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StrictData                 #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Language.Marlowe.Runtime.Chain.Types where

import Cardano.Api (AddressAny, AsType (..), BlockInMode, CardanoMode, ChainPoint, ChainSyncClient, ChainTip, PolicyId,
                    ScriptData, SerialiseAsCBOR (..), SerialiseAsRawBytes (..), TxId, TxIx (..), Value, serialiseToJSON)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary (..), Word64)
import Data.ByteString.Short (ShortByteString)
import Data.Data (Typeable)
import Data.Time (Day (..), NominalDiffTime, UTCTime (..), diffTimeToPicoseconds, nominalDiffTimeToSeconds,
                  picosecondsToDiffTime, secondsToNominalDiffTime)
import GHC.Generics (Generic)

type MarloweChainSyncClient = ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip

data MarloweChainEvent
  = MarloweRollForward MarloweBlockHeader [MarloweTx] MarloweChainTip
  | MarloweRollBackward MarloweChainPoint MarloweChainTip
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweChainEvent

data MarloweBlockHeader = MarloweBlockHeader MarloweSlotNo MarloweBlockHeaderHash MarloweBlockNo
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweBlockHeader

newtype MarloweSlotNo = MarloweSlotNo Word64
  deriving (Typeable, Show, Eq, Binary)

newtype MarloweBlockNo = MarloweBlockNo Word64
  deriving (Typeable, Show, Eq, Binary)

newtype MarloweBlockHeaderHash = MarloweBlockHeaderHash ShortByteString
  deriving (Typeable, Show, Eq, Binary)

data MarloweChainTip
  = MarloweChainTipAtGenesis
  | MarloweChainTip MarloweSlotNo MarloweBlockHeaderHash MarloweBlockNo
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweChainTip

data MarloweChainPoint
  = MarloweChainPointAtGenesis
  | MarloweChainPoint MarloweSlotNo MarloweBlockHeaderHash
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweChainPoint

data MarloweTxIn = MarloweTxIn MarloweTxId TxIx
  deriving (Typeable, Show, Eq, Ord)

instance Binary MarloweTxIn where
  put (MarloweTxIn tid (TxIx ix)) = do
    put tid
    put ix
  get = do
    tid <- get
    ix <- get
    pure $ MarloweTxIn tid $ TxIx ix

newtype MarloweTxId = MarloweTxId TxId
  deriving (Typeable, Show, Eq, Ord)

instance Binary MarloweTxId where
  put (MarloweTxId tid) = do
    put $ serialiseToRawBytes tid
  get = do
    tidBytes <- get
    tid <- case deserialiseFromRawBytes AsTxId tidBytes of
      Nothing -> fail "Idvalid txId bytes"
      Just a  -> pure a
    pure $ MarloweTxId tid

newtype MarlowePolicyId = MarlowePolicyId PolicyId
  deriving (Typeable, Show, Eq, Ord)

instance Binary MarlowePolicyId where
  put (MarlowePolicyId tid) = do
    put $ serialiseToRawBytes tid
  get = do
    tidBytes <- get
    tid <- case deserialiseFromRawBytes AsPolicyId tidBytes of
      Nothing -> fail "Idvalid policyId bytes"
      Just a  -> pure a
    pure $ MarlowePolicyId tid

data MarloweTx = MarloweTx
  { marloweTx_id       :: MarloweTxId
  , marloweTx_policies :: [MarlowePolicyId]
  , marloweTx_interval :: Maybe MarloweValidityInterval
  , marloweTx_metadata :: Maybe Aeson.Value
  , marloweTx_inputs   :: [MarloweTxIn]
  , marloweTx_outputs  :: [MarloweTxOut]
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweTx where
  put (MarloweTx tid policies interval metadata inputs outputs) = do
    put tid
    put policies
    put interval
    put $ Aeson.encode <$> metadata
    put inputs
    put outputs
  get = do
    tid <- get
    policies <- get
    interval <- get
    metadataBytes <- get
    metadata <- forM metadataBytes \bytes -> case Aeson.eitherDecode bytes of
      Left err -> fail err
      Right a  -> pure a
    inputs <- get
    outputs <- get
    pure $ MarloweTx tid policies interval metadata inputs outputs

newtype MarloweAddress = MarloweAddress AddressAny
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweAddress where
  put (MarloweAddress address) = put $ serialiseToRawBytes address
  get = do
    bytes <- get
    case deserialiseFromRawBytes AsAddressAny bytes of
      Nothing      -> fail "invalid address bytes"
      Just address -> pure $ MarloweAddress address

data MarloweTxOut = MarloweTxOut
  { marloweTxOut_txIn    :: MarloweTxIn
  , marloweTxOut_address :: MarloweAddress
  , marloweTxOut_value   :: Value
  , marloweTxOut_datum   :: Maybe ScriptData
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweTxOut where
  put (MarloweTxOut txIn address value datum) = do
    put txIn
    put address
    put $ serialiseToJSON value
    put $ serialiseToCBOR <$> datum
  get = do
    txIn <- get
    address <- get
    valueBytes <- get
    datumBytes <- get
    value <- case Aeson.eitherDecode valueBytes of
      Left err -> fail err
      Right a  -> pure a
    datum <- forM datumBytes \bytes -> case deserialiseFromCBOR AsScriptData bytes of
      Left err -> fail $ show err
      Right a  -> pure a
    pure $ MarloweTxOut txIn address value datum

data MarloweValidityInterval = MarloweValidityInterval
  { startTime :: UTCTime
  , duration  :: NominalDiffTime
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweValidityInterval where
  put (MarloweValidityInterval (UTCTime (ModifiedJulianDay d) dt) dt') = do
    put d
    put $ diffTimeToPicoseconds dt
    put $ nominalDiffTimeToSeconds dt'
  get = do
    d <- get
    dt <- picosecondsToDiffTime <$> get
    dt' <- secondsToNominalDiffTime <$> get
    pure $ MarloweValidityInterval (UTCTime (ModifiedJulianDay d) dt) dt'
