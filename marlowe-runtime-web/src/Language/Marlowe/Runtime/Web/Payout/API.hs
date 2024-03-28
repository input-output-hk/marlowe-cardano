{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
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

module Language.Marlowe.Runtime.Web.Payout.API (
  PayoutHeader (..),
  PayoutState (..),
  PayoutStatus (..),
  Payout (..),
  PayoutsAPI,
  GetPayoutAPI,
  GetPayoutResponse,
  GetPayoutsResponse,
) where

import Control.Lens ((&), (?~))
import Data.Aeson (
  FromJSON (parseJSON),
  ToJSON (toJSON),
  Value (String),
  withText,
 )
import Data.OpenApi (
  HasEnum (enum_),
  HasType (type_),
  NamedSchema (NamedSchema),
  OpenApiType (OpenApiString),
  ToParamSchema (..),
  ToSchema (..),
 )
import qualified Data.OpenApi as OpenApi
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Web.Adapter.Links (WithLink)
import Language.Marlowe.Runtime.Web.Adapter.Pagination (PaginatedGet)
import Language.Marlowe.Runtime.Web.Adapter.Servant (
  OperationId,
  RenameResponseSchema,
 )
import Language.Marlowe.Runtime.Web.Core.Address (Address)
import Language.Marlowe.Runtime.Web.Core.Asset (AssetId, Assets)
import Language.Marlowe.Runtime.Web.Core.Tx (TxId, TxOutRef)
import Servant.API (
  Capture,
  Description,
  FromHttpApiData (parseQueryParam),
  Get,
  JSON,
  Optional,
  QueryParam',
  QueryParams,
  Summary,
  ToHttpApiData (toQueryParam),
  type (:<|>),
  type (:>),
 )
import Servant.Pagination (
  HasPagination (RangeType, getFieldValue),
 )

data PayoutHeader = PayoutHeader
  { payoutId :: TxOutRef
  , contractId :: TxOutRef
  , withdrawalId :: Maybe TxId
  , role :: AssetId
  , status :: PayoutStatus
  }
  deriving (Show, Eq, Ord, Generic)

data Payout = Payout
  { payoutId :: TxOutRef
  , role :: Text
  , assets :: Assets
  }
  deriving (FromJSON, ToJSON, ToSchema, Show, Eq, Generic)

data PayoutStatus
  = Available
  | Withdrawn
  deriving (Show, Eq, Ord, Generic)

data PayoutState = PayoutState
  { payoutId :: TxOutRef
  , contractId :: TxOutRef
  , withdrawalId :: Maybe TxId
  , role :: AssetId
  , payoutValidatorAddress :: Address
  , status :: PayoutStatus
  , assets :: Assets
  }
  deriving (FromJSON, ToJSON, ToSchema, Show, Eq, Generic)

-- | /payouts sub-API
type PayoutsAPI =
  GetPayoutsAPI
    :<|> Capture "payoutId" TxOutRef :> GetPayoutAPI

type GetPayoutAPI =
  Summary "Get payout by ID"
    :> OperationId "getPayoutById"
    :> RenameResponseSchema "GetPayoutResponse"
    :> Get '[JSON] GetPayoutResponse

type GetPayoutResponse = WithLink "contract" (WithLink "transaction" (WithLink "withdrawal" PayoutState))

-- | GET /payouts sub-API
type GetPayoutsAPI =
  Summary "Get role payouts"
    :> Description
        "Get payouts to parties from role-based contracts. \
        \Results are returned in pages, with paging being specified by request headers."
    :> OperationId "getPayouts"
    :> QueryParams "contractId" TxOutRef
    :> QueryParams "roleToken" AssetId
    :> QueryParam'
        '[Optional, Description "Whether to include available or withdrawn payouts in the results."]
        "status"
        PayoutStatus
    :> RenameResponseSchema "GetPayoutsResponse"
    :> PaginatedGet '["payoutId"] GetPayoutsResponse

type GetPayoutsResponse = WithLink "payout" PayoutHeader

instance ToJSON PayoutStatus where
  toJSON =
    String . \case
      Available -> "available"
      Withdrawn -> "withdrawn"

instance FromJSON PayoutStatus where
  parseJSON =
    withText
      "PayoutStatus"
      ( \str -> case T.toLower str of
          "available" -> pure Available
          "withdrawn" -> pure Withdrawn
          _ -> fail "expected \"available\" or \"withdrawn\""
      )

instance ToHttpApiData PayoutStatus where
  toQueryParam = \case
    Available -> "available"
    Withdrawn -> "withdrawn"

instance FromHttpApiData PayoutStatus where
  parseQueryParam str = case T.toLower str of
    "available" -> pure Available
    "withdrawn" -> pure Withdrawn
    _ -> Left "expected \"available\" or \"withdrawn\""

instance ToSchema PayoutStatus where
  declareNamedSchema = pure . NamedSchema (Just "PayoutStatus") . toParamSchema

instance ToParamSchema PayoutStatus where
  toParamSchema _ =
    mempty
      & type_ ?~ OpenApiString
      & enum_ ?~ ["available", "withdrawn"]
      & OpenApi.description
        ?~ "The status of a payout. Either it is available to be withdrawn, or it has already been withdrawn."

instance HasPagination PayoutHeader "payoutId" where
  type RangeType PayoutHeader "payoutId" = TxOutRef
  getFieldValue _ PayoutHeader{..} = payoutId

instance ToJSON PayoutHeader
instance FromJSON PayoutHeader
instance ToSchema PayoutHeader
