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

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.
module Language.Marlowe.Runtime.Web.Role.API (
  BurnRoleTokensTxEnvelope (..),
  BurnTokensTx,
  SubmitBurnTokensTxAPI,
  RoleAPI,
) where

import Language.Marlowe.Runtime.Web.Adapter.Servant (OperationId, RenameResponseSchema)
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Servant (
  Accept (contentType),
  JSON,
  MimeRender (mimeRender),
  MimeUnrender (mimeUnrender),
  Proxy (..),
  ReqBody,
  type (:>),
 )

import Control.Lens ((&), (.~), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  eitherDecode,
  encode,
  object,
  withObject,
  (.:),
 )
import Data.OpenApi (
  HasProperties (properties),
  HasRequired (required),
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiObject),
  ToSchema (..),
  declareSchemaRef,
 )
import qualified Data.OpenApi as OpenApi
import GHC.Generics (Generic)

import Language.Marlowe.Runtime.Web.Core.Tx (TextEnvelope, TxId)
import Language.Marlowe.Runtime.Web.Role.TokenFilter (RoleTokenFilter)
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  CardanoTxBody,
  PostTxAPI,
  PutSignedTxAPI,
  TxJSON,
 )
import Network.HTTP.Media ((//))
import Servant.API (
  Capture,
  Description,
  PostCreated,
  Summary,
  type (:<|>),
 )

type RoleAPI =
  "role-tokens" :> "burnTxs" :> (BuildBurnTokensTxAPI :<|> Capture "TxId" TxId :> SubmitBurnTokensTxAPI)

data BurnTokensTx

instance Accept (TxJSON BurnTokensTx) where
  contentType _ = "application" // "vendor.iog.marlowe-runtime.burn-role-tokens-tx-json"

instance MimeRender (TxJSON BurnTokensTx) (BurnRoleTokensTxEnvelope CardanoTx) where
  mimeRender _ = encode . toJSON

instance MimeUnrender (TxJSON BurnTokensTx) (BurnRoleTokensTxEnvelope CardanoTx) where
  mimeUnrender _ = eitherDecode

type BuildBurnTokensTxAPI =
  Summary "Build a Burn role tokens Transation"
    :> Description
        "Build an unsigned (Cardano) transaction body which burns role tokens matching a filter. \
        \Role tokens used by active contracts will not be burned and the request will fail if active role tokens are included. \
        \To submit the signed transaction, use the PUT /roles/burnTokensTxs/submit endpoint."
    :> OperationId "buildBurnRoleTokensTx"
    :> RenameResponseSchema "BurnRoleTokensResponse"
    :> ( ReqBody '[JSON] RoleTokenFilter :> PostTxAPI (PostCreated '[JSON] (BurnRoleTokensTxEnvelope CardanoTxBody))
          :<|> ReqBody '[JSON] RoleTokenFilter :> PostTxAPI (PostCreated '[TxJSON BurnTokensTx] (BurnRoleTokensTxEnvelope CardanoTx))
       )

type SubmitBurnTokensTxAPI =
  Summary "Submit a Burn Role Token Transaction"
    :> Description
        "Submit a signed (Cardano) transaction that burns role tokens. \
        \The transaction must have originally been created by the POST /roles/burnTokensTxs/build endpoint. \
        \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
        \it will not wait for the transaction to be published in a block. \
        \Use the GET /roles/burn/{burnId} endpoint to poll the on-chain status."
    :> OperationId "submitBurnRoleTokensTx"
    :> PutSignedTxAPI

data BurnRoleTokensTxEnvelope tx = BurnRoleTokensTxEnvelope
  { txId :: TxId
  , txEnvelope :: TextEnvelope
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON (BurnRoleTokensTxEnvelope CardanoTx) where
  toJSON BurnRoleTokensTxEnvelope{..} =
    object
      [ ("txId", toJSON txId)
      , ("tx", toJSON txEnvelope)
      ]
instance ToJSON (BurnRoleTokensTxEnvelope CardanoTxBody) where
  toJSON BurnRoleTokensTxEnvelope{..} =
    object
      [ ("txId", toJSON txId)
      , ("txBody", toJSON txEnvelope)
      ]

instance FromJSON (BurnRoleTokensTxEnvelope CardanoTx) where
  parseJSON =
    withObject
      "BurnRoleTokensTxEnvelope"
      ( \obj ->
          BurnRoleTokensTxEnvelope
            <$> obj .: "txId"
            <*> obj .: "tx"
      )

instance FromJSON (BurnRoleTokensTxEnvelope CardanoTxBody) where
  parseJSON =
    withObject
      "BurnRoleTokensTxEnvelope"
      ( \obj ->
          BurnRoleTokensTxEnvelope
            <$> obj .: "txId"
            <*> obj .: "txBody"
      )

instance ToSchema (BurnRoleTokensTxEnvelope CardanoTx) where
  declareNamedSchema _ = do
    txIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "BurnRoleTokensTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"tx\" must be \"Tx BabbageEra\" or \"Tx ConwayEra\""
          & properties
            .~ [ ("txId", txIdSchema)
               , ("tx", txEnvelopeSchema)
               ]
          & required .~ ["txId", "tx"]

instance ToSchema (BurnRoleTokensTxEnvelope CardanoTxBody) where
  declareNamedSchema _ = do
    txIdSchema <- declareSchemaRef (Proxy :: Proxy TxId)
    txEnvelopeSchema <- declareSchemaRef (Proxy :: Proxy TextEnvelope)
    return $
      NamedSchema (Just "BurnRoleTokensTxEnvelope") $
        mempty
          & type_ ?~ OpenApiObject
          & OpenApi.description ?~ "The \"type\" property of \"txBody\" must be \"TxBody BabbageEra\" or \"TxBody ConwayEra\""
          & properties
            .~ [ ("txId", txIdSchema)
               , ("txBody", txEnvelopeSchema)
               ]
          & required .~ ["txId", "txBody"]
