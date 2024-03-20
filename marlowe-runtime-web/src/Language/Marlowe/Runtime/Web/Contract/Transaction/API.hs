{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Marlowe.Runtime.Web.Contract.Transaction.API (
  TransactionsAPI,
  GetTransactionsAPI,
  GetTransactionsResponse,
  TransactionAPI,
  GetTransactionAPI,
  GetTransactionResponse,
  PostTransactionsResponse,
) where

import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedGet)
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  OperationId,
  RenameResponseSchema,
 )
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Language.Marlowe.Runtime.Web.Core.Tx
import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTx,
  ApplyInputsTxEnvelope,
  CardanoTx,
  CardanoTxBody,
  PostTxAPI,
  PutSignedTxAPI,
  Tx,
  TxHeader,
  TxJSON,
 )
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  PostTransactionsRequest,
 )
import Servant (
  Capture,
  Description,
  Get,
  JSON,
  PostCreated,
  ReqBody,
  Summary,
  type (:<|>),
  type (:>),
 )

-- | /contracts/:contractId/transactions sub-API
type TransactionsAPI =
  GetTransactionsAPI
    :<|> PostTransactionsAPI
    :<|> Capture "transactionId" TxId :> TransactionAPI

-- | GET /contracts/:contractId/transactions sub-API
type GetTransactionsAPI =
  Summary "Get transactions for contract"
    :> Description
        "Get published transactions for a contract. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getTransactionsForContract"
    :> RenameResponseSchema "GetTransactionsResponse"
    :> PaginatedGet '["transactionId"] GetTransactionsResponse

type GetTransactionsResponse = WithLink "transaction" TxHeader

-- | /contracts/:contractId/transactions/:transactionId sub-API
type TransactionAPI =
  GetTransactionAPI
    :<|> Summary "Submit contract input application"
      :> Description
          "Submit a signed (Cardano) transaction that applies inputs to an open Marlowe contract. \
          \The transaction must have originally been created by the POST /contracts/{contractId}/transactions endpoint. \
          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
          \it will not wait for the transaction to be published in a block. \
          \Use the GET /contracts/{contractId}/transactions/{transactionId} endpoint to poll the on-chain status."
      :> OperationId "submitContractTransaction"
      :> PutSignedTxAPI

-- | GET /contracts/:contractId/transactions/:transactionId sub-API
type GetTransactionAPI =
  Summary "Get contract transaction by ID"
    :> OperationId "getContractTransactionById"
    :> RenameResponseSchema "GetTransactionResponse"
    :> Get '[JSON] GetTransactionResponse

type GetTransactionResponse = WithLink "previous" (WithLink "next" Tx)

-- | POST /contracts/:contractId/transactions sub-API
type PostTransactionsAPI =
  Summary "Apply inputs to contract"
    :> Description
        "Build an unsigned (Cardano) transaction body which applies inputs to an open Marlowe contract. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /contracts/{contractId}/transactions/{transactionId} endpoint."
    :> OperationId "applyInputsToContract"
    :> RenameResponseSchema "ApplyInputsResponse"
    :> ( ReqBody '[JSON] PostTransactionsRequest :> PostTxAPI (PostCreated '[JSON] (PostTransactionsResponse CardanoTxBody))
          :<|> ReqBody '[JSON] PostTransactionsRequest
            :> PostTxAPI (PostCreated '[TxJSON ApplyInputsTx] (PostTransactionsResponse CardanoTx))
       )

type PostTransactionsResponse tx = WithLink "transaction" (ApplyInputsTxEnvelope tx)
