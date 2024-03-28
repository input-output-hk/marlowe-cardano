{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Language.Marlowe.Runtime.Web.Withdrawal.API (
  WithdrawalHeader (..),
  Withdrawal (..),
  WithdrawalsAPI,
  GetWithdrawalsResponse,
  PostWithdrawalsRequest (..),
  PostWithdrawalsResponse,
  WithdrawalAPI,
  GetWithdrawalAPI,
  PostTransactionsRequest (..),
) where

import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedGet)
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  OperationId,
  RenameResponseSchema,
 )
import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()
import Language.Marlowe.Runtime.Web.Core.Asset (PolicyId)

import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import Servant (
  Capture,
  Description,
  Get,
  JSON,
  PostCreated,
  QueryParams,
  ReqBody,
  Summary,
  type (:<|>),
  type (:>),
 )

import Data.Aeson (FromJSON, ToJSON)
import Data.Map (Map)
import Data.OpenApi (
  ToSchema,
 )
import Data.Set (Set)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics.Types as Semantics
import Language.Marlowe.Runtime.Web.Core.MarloweVersion (MarloweVersion)
import Language.Marlowe.Runtime.Web.Core.Semantics.Schema ()
import Servant.Pagination (HasPagination (..))

import Language.Marlowe.Runtime.Web.Core.Metadata (Metadata)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TxId,
  TxOutRef,
  TxStatus,
 )
import Language.Marlowe.Runtime.Web.Payout.API (PayoutHeader)
import Language.Marlowe.Runtime.Web.Tx.API (
  CardanoTx,
  CardanoTxBody,
  PostTxAPI,
  PutSignedTxAPI,
  TxJSON,
  WithdrawTx,
  WithdrawTxEnvelope,
 )

-- | /withdrawals sub-API
type WithdrawalsAPI =
  GetWithdrawalsAPI
    :<|> PostWithdrawalsAPI
    :<|> Capture "withdrawalId" TxId :> WithdrawalAPI

-- | /contracts/:contractId/withdrawals/:withdrawalId sub-API
type WithdrawalAPI =
  GetWithdrawalAPI
    :<|> Summary "Submit payout withdrawal"
      :> Description
          "Submit a signed (Cardano) transaction that withdraws available payouts from a role payout validator. \
          \The transaction must have originally been created by the POST /withdrawals endpoint. \
          \This endpoint will respond when the transaction is submitted successfully to the local node, which means \
          \it will not wait for the transaction to be published in a block. \
          \Use the GET /withdrawals/{withdrawalId} endpoint to poll the on-chain status."
      :> OperationId "submitWithdrawal"
      :> PutSignedTxAPI

-- | GET /contracts/:contractId/withdrawals/:withdrawalId sub-API
type GetWithdrawalAPI =
  Summary "Get withdrawal by ID"
    :> OperationId "getWithdrawalById"
    :> Get '[JSON] Withdrawal

-- | POST /contracts sub-API
type PostWithdrawalsAPI =
  Summary "Withdraw payouts"
    :> Description
        "Build an unsigned (Cardano) transaction body which withdraws available payouts from a role payout validator. \
        \This unsigned transaction must be signed by a wallet (such as a CIP-30 or CIP-45 wallet) before being submitted. \
        \To submit the signed transaction, use the PUT /withdrawals/{withdrawalId} endpoint."
    :> OperationId "withdrawPayouts"
    :> RenameResponseSchema "WithdrawPayoutsResponse"
    :> ( ReqBody '[JSON] PostWithdrawalsRequest :> PostTxAPI (PostCreated '[JSON] (PostWithdrawalsResponse CardanoTxBody))
          :<|> ReqBody '[JSON] PostWithdrawalsRequest
            :> PostTxAPI (PostCreated '[TxJSON WithdrawTx] (PostWithdrawalsResponse CardanoTx))
       )

type PostWithdrawalsResponse tx = WithLink "withdrawal" (WithdrawTxEnvelope tx)

-- | GET /contracts/:contractId/withdrawals sub-API
type GetWithdrawalsAPI =
  Summary "Get withdrawals"
    :> Description
        "Get published withdrawal transactions. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getWithdrawals"
    :> QueryParams "roleCurrency" PolicyId
    :> RenameResponseSchema "GetWithdrawalsResponse"
    :> PaginatedGet '["withdrawalId"] GetWithdrawalsResponse

type GetWithdrawalsResponse = WithLink "withdrawal" WithdrawalHeader

data WithdrawalHeader = WithdrawalHeader
  { withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

instance HasPagination WithdrawalHeader "withdrawalId" where
  type RangeType WithdrawalHeader "withdrawalId" = TxId
  getFieldValue _ WithdrawalHeader{..} = withdrawalId

data Withdrawal = Withdrawal
  { payouts :: Set PayoutHeader
  , withdrawalId :: TxId
  , status :: TxStatus
  , block :: Maybe BlockHeader
  }
  deriving (Show, Eq, Ord, Generic, ToJSON, FromJSON, ToSchema)

newtype PostWithdrawalsRequest = PostWithdrawalsRequest
  { payouts :: Set TxOutRef
  }
  deriving (Show, Eq, Ord, Generic)

instance FromJSON PostWithdrawalsRequest
instance ToJSON PostWithdrawalsRequest
instance ToSchema PostWithdrawalsRequest

data PostTransactionsRequest = PostTransactionsRequest
  { version :: MarloweVersion
  , tags :: Map Text Metadata
  , metadata :: Map Word64 Metadata
  , invalidBefore :: Maybe UTCTime
  , invalidHereafter :: Maybe UTCTime
  , inputs :: [Semantics.Input]
  }
  deriving (Show, Eq, Generic, ToJSON, FromJSON, ToSchema)
