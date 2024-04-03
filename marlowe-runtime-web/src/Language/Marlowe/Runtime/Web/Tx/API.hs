{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Marlowe.Runtime.Web.Tx.API (
  TxHeader (..),
  Tx (..),
  CardanoTx,
  CardanoTxBody,
  ContractTx,
  CreateTxEnvelope (..),
  ApplyInputsTx,
  ApplyInputsTxEnvelope (..),
  WithdrawTx,
  WithdrawTxEnvelope (..),
  PostTxAPI,
  PutSignedTxAPI,
  TxJSON,
) where

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  object,
  withObject,
  (.:),
 )
import GHC.Generics (Generic)
import Network.HTTP.Media ((//))
import Servant (
  Accept,
  Header,
  Header',
  JSON,
  NoContent,
  Proxy (..),
  PutAccepted,
  ReqBody,
  Required,
  Strict,
  type (:>),
 )
import Servant.API (Accept (..))

import Data.Map (Map)
import Data.OpenApi (
  HasType (..),
  NamedSchema (..),
  OpenApiType (..),
  ToSchema,
  declareSchemaRef,
  properties,
  required,
 )
import qualified Data.OpenApi as OpenApi
import Data.OpenApi.Schema (ToSchema (..))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import Language.Marlowe.Analysis.Safety.Types (SafetyError)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Adapter.CommaList (CommaList)
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Asset (Assets)
import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope,
  TxId,
  TxOutRef,
  TxStatus,
 )
import Language.Marlowe.Runtime.Web.Payout.API (Payout)
import Servant.Pagination (
  HasPagination (RangeType, getFieldValue),
 )

data TxHeader = TxHeader
  { contractId :: TxOutRef
  , transactionId :: TxId
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , utxo :: Maybe TxOutRef
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance HasPagination TxHeader "transactionId" where
  type RangeType TxHeader "transactionId" = TxId
  getFieldValue _ TxHeader{..} = transactionId

data Tx = Tx
  { contractId :: TxOutRef
  , transactionId :: TxId
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , status :: TxStatus
  , block :: Maybe BlockHeader
  , inputUtxo :: TxOutRef
  , inputContract :: Semantics.Contract
  , inputState :: Semantics.State
  , inputs :: [Semantics.Input]
  , outputUtxo :: Maybe TxOutRef
  , outputContract :: Maybe Semantics.Contract
  , outputState :: Maybe Semantics.State
  , assets :: Assets
  , payouts :: [Payout]
  , consumingTx :: Maybe TxId
  , invalidBefore :: UTCTime
  , invalidHereafter :: UTCTime
  , reconstructedSemanticInput :: V1.TransactionInput
  , reconstructedSemanticOutput :: V1.TransactionOutput
  , txBody :: Maybe TextEnvelope
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)

data TxJSON a
data CardanoTx
data CardanoTxBody
data ContractTx
data ApplyInputsTx
data WithdrawTx

type PutSignedTxAPI = ReqBody '[JSON] TextEnvelope :> PutAccepted '[JSON] NoContent

type PostTxAPI api =
  Header' '[Required, Strict] "X-Change-Address" Address
    :> Header "X-Address" (CommaList Address)
    :> Header "X-Collateral-UTxO" (CommaList TxOutRef)
    :> api

instance Accept (TxJSON ContractTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.contract-tx-json"

instance Accept (TxJSON ApplyInputsTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.apply-inputs-tx-json"

instance Accept (TxJSON WithdrawTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.withdraw-tx-json"

data CreateTxEnvelope tx = CreateTxEnvelope
  { contractId :: TxOutRef
  , txEnvelope :: TextEnvelope
  , safetyErrors :: [SafetyError]
  }
  deriving (Show, Eq, Generic)

instance ToJSON (CreateTxEnvelope CardanoTx) where
  toJSON CreateTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("tx", toJSON txEnvelope)
      , ("safetyErrors", toJSON safetyErrors)
      ]
instance ToJSON (CreateTxEnvelope CardanoTxBody) where
  toJSON CreateTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("txBody", toJSON txEnvelope)
      , ("safetyErrors", toJSON safetyErrors)
      ]

instance FromJSON (CreateTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "CreateTxEnvelope"
      ( \obj ->
          CreateTxEnvelope
            <$> obj
              .: "contractId"
            <*> obj
              .: "tx"
            <*> obj
              .: "safetyErrors"
      )

instance FromJSON (CreateTxEnvelope CardanoTxBody) where
  parseJSON =
    withObject
      "CreateTxEnvelope"
      ( \obj ->
          CreateTxEnvelope
            <$> obj
              .: "contractId"
            <*> obj
              .: "txBody"
            <*> obj
              .: "safetyErrors"
      )

instance ToSchema (CreateTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    safetyErrorsSchema <- declareSchemaRef (Proxy :: Proxy [SafetyError])
    return $
      NamedSchema (Just "CreateTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("tx", txEnvelopeSchema)
               , ("safetyErrors", safetyErrorsSchema)
               ]
          & required .~ ["contractId", "tx"]

instance ToSchema (CreateTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    safetyErrorsSchema <- declareSchemaRef (Proxy :: Proxy [SafetyError])
    return $
      NamedSchema (Just "CreateTxBodyEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("txBody", txEnvelopeSchema)
               , ("safetyErrors", safetyErrorsSchema)
               ]
          & required .~ ["contractId", "txBody"]

data WithdrawTxEnvelope tx = WithdrawTxEnvelope
  { withdrawalId :: TxId
  , txEnvelope :: TextEnvelope
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (WithdrawTxEnvelope CardanoTx) where
  toJSON WithdrawTxEnvelope{..} =
    object
      [ ("withdrawalId", toJSON withdrawalId)
      , ("tx", toJSON txEnvelope)
      ]
instance ToJSON (WithdrawTxEnvelope CardanoTxBody) where
  toJSON WithdrawTxEnvelope{..} =
    object
      [ ("withdrawalId", toJSON withdrawalId)
      , ("txBody", toJSON txEnvelope)
      ]

instance FromJSON (WithdrawTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "WithdrawTxEnvelope"
      ( \obj ->
          WithdrawTxEnvelope
            <$> obj .: "withdrawalId"
            <*> obj .: "tx"
      )

instance FromJSON (WithdrawTxEnvelope CardanoTxBody) where
  parseJSON =
    withObject
      "WithdrawTxEnvelope"
      ( \obj ->
          WithdrawTxEnvelope
            <$> obj .: "withdrawalId"
            <*> obj .: "txBody"
      )

instance ToSchema (WithdrawTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "WithdrawTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("withdrawalId", withdrawalIdSchema)
               , ("tx", txEnvelopeSchema)
               ]
          & required .~ ["withdrawalId", "tx"]

instance ToSchema (WithdrawTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    withdrawalIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "WithdrawTxBodyEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("withdrawalId", withdrawalIdSchema)
               , ("txBody", txEnvelopeSchema)
               ]
          & required .~ ["withdrawalId", "txBody"]

data ApplyInputsTxEnvelope tx = ApplyInputsTxEnvelope
  { contractId :: TxOutRef
  , transactionId :: TxId
  , txEnvelope :: TextEnvelope
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (ApplyInputsTxEnvelope CardanoTx) where
  toJSON ApplyInputsTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("transactionId", toJSON transactionId)
      , ("tx", toJSON txEnvelope)
      ]
instance ToJSON (ApplyInputsTxEnvelope CardanoTxBody) where
  toJSON ApplyInputsTxEnvelope{..} =
    object
      [ ("contractId", toJSON contractId)
      , ("transactionId", toJSON transactionId)
      , ("txBody", toJSON txEnvelope)
      ]

instance FromJSON (ApplyInputsTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "ApplyInputsTxEnvelope"
      ( \obj -> do
          contractId <- obj .: "contractId"
          transactionId <- obj .: "transactionId"
          txEnvelope <- obj .: "tx"
          pure ApplyInputsTxEnvelope{..}
      )

instance FromJSON (ApplyInputsTxEnvelope CardanoTxBody) where
  parseJSON =
    withObject
      "ApplyInputsTxEnvelope"
      ( \obj -> do
          contractId <- obj .: "contractId"
          transactionId <- obj .: "transactionId"
          txEnvelope <- obj .: "txBody"
          pure ApplyInputsTxEnvelope{..}
      )

instance ToSchema (ApplyInputsTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    transactionIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "ApplyInputsTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("transactionId", transactionIdSchema)
               , ("tx", txEnvelopeSchema)
               ]
          & required .~ ["contractId", "transactionId", "tx"]

instance ToSchema (ApplyInputsTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    contractIdSchema <- declareSchemaRef (Proxy :: Proxy TxOutRef)
    transactionIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "ApplyInputsTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("contractId", contractIdSchema)
               , ("transactionId", transactionIdSchema)
               , ("txBody", txEnvelopeSchema)
               ]
          & required .~ ["contractId", "transactionId", "txBody"]
