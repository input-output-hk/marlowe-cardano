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
                    ScriptData, SerialiseAsCBOR (..), SerialiseAsRawBytes (..), TxId, TxIx (..), Value)
import Codec.Serialise (deserialiseOrFail, serialise)
import Control.Monad (forM)
import qualified Data.Aeson as Aeson
import Data.Binary (Binary (..), Get, Put, Word64)
import Data.ByteString.Short (ShortByteString)
import Data.Data (Typeable)
import Data.Time (Day (..), NominalDiffTime, UTCTime (..), diffTimeToPicoseconds, nominalDiffTimeToSeconds,
                  picosecondsToDiffTime, secondsToNominalDiffTime)
import GHC.Generics (Generic)
import Plutus.V1.Ledger.Api (Data)

type MarloweChainSyncClient = ChainSyncClient (BlockInMode CardanoMode) ChainPoint ChainTip

data MarloweChainEvent
  = MarloweRollForward MarloweBlockHeader [MarloweTx] MarloweChainTip
  | MarloweRollBackward MarloweChainPoint MarloweChainTip
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweChainEvent

data MarloweBlockHeader = MarloweBlockHeader MarloweSlotNo MarloweBlockHeaderHash MarloweBlockNo
  deriving (Generic, Typeable, Show, Eq, Ord)

instance Binary MarloweBlockHeader

newtype MarloweSlotNo = MarloweSlotNo Word64
  deriving (Typeable, Show, Eq, Binary, Ord)

newtype MarloweBlockNo = MarloweBlockNo Word64
  deriving (Typeable, Show, Eq, Binary, Ord)

newtype MarloweBlockHeaderHash = MarloweBlockHeaderHash ShortByteString
  deriving (Typeable, Show, Eq, Binary, Ord)

data MarloweChainTip
  = MarloweChainTipAtGenesis
  | MarloweChainTip MarloweSlotNo MarloweBlockHeaderHash MarloweBlockNo
  deriving (Generic, Typeable, Show, Eq, Ord)

instance Binary MarloweChainTip

data MarloweChainPoint
  = MarloweChainPointAtGenesis
  | MarloweChainPoint MarloweSlotNo MarloweBlockHeaderHash
  deriving (Generic, Typeable, Show, Eq, Ord)

instance Binary MarloweChainPoint

headerPoint :: MarloweBlockHeader -> MarloweChainPoint
headerPoint (MarloweBlockHeader slot hash _) = MarloweChainPoint slot hash

data MarloweTxIn = MarloweTxIn MarloweTxId TxIx (Maybe Data)
  deriving (Typeable, Show, Eq, Ord)

instance Binary MarloweTxIn where
  put (MarloweTxIn tid (TxIx ix) redeemer) = do
    put tid
    put ix
    put $ serialise <$> redeemer
  get = do
    tid <- get
    ix <- get
    redeemerBytes <- get
    redeemer <- case deserialiseOrFail <$> redeemerBytes of
      Nothing               -> pure Nothing
      Just (Left err)       -> fail $ show err
      Just (Right redeemer) -> pure $ Just redeemer
    pure $ MarloweTxIn tid (TxIx ix) redeemer

data TxOutRef = TxOutRef MarloweTxId TxIx
  deriving (Typeable, Show, Eq, Ord)

instance Binary TxOutRef where
  put (TxOutRef tid (TxIx ix)) = do
    put tid
    put ix
  get = TxOutRef <$> get <*> (TxIx <$> get)

newtype MarloweTxId = MarloweTxId TxId
  deriving (Typeable, Show, Eq, Ord)

matchOutputRef :: TxOutRef -> MarloweTxIn -> Bool
matchOutputRef (TxOutRef outId outIx) (MarloweTxIn inId inIx _) =
  outId == inId && outIx == inIx

instance Binary MarloweTxId where
  put (MarloweTxId tid) = putToRawBytes tid
  get = MarloweTxId <$> getFromRawBytes AsTxId

newtype MarlowePolicyId = MarlowePolicyId { unMarlowePolicyId :: PolicyId }
  deriving (Typeable, Show, Eq, Ord)

instance Binary MarlowePolicyId where
  put (MarlowePolicyId pid) = putToRawBytes pid
  get = MarlowePolicyId <$> getFromRawBytes AsPolicyId

putToRawBytes :: SerialiseAsRawBytes b => b -> Put
putToRawBytes = put . serialiseToRawBytes

getFromRawBytes :: SerialiseAsRawBytes b => AsType b -> Get b
getFromRawBytes asType = do
  bytes <- get
  case deserialiseFromRawBytes asType bytes of
    Nothing -> fail "Invalid byte sequence"
    Just a  -> pure a

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
  deriving (Generic, Typeable, Show, Eq, Ord)

instance Binary MarloweAddress where
  put (MarloweAddress address) = putToRawBytes address
  get = MarloweAddress <$> getFromRawBytes AsAddressAny

data MarloweTxOut = MarloweTxOut
  { marloweTxOut_txOutRef :: TxOutRef
  , marloweTxOut_address  :: MarloweAddress
  , marloweTxOut_value    :: Value
  , marloweTxOut_datum    :: Maybe ScriptData
  }
  deriving (Generic, Typeable, Show, Eq)

instance Binary MarloweTxOut where
  put (MarloweTxOut txIn address value datum) = do
    put txIn
    put address
    put $ Aeson.encode value
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
