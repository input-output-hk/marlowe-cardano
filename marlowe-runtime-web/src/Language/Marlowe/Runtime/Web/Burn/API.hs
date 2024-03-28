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

-- | This module specifies the Marlowe Runtime Web API as a Servant API type.
module Language.Marlowe.Runtime.Web.Burn.API (BurnsAPI) where

import Language.Marlowe.Runtime.Web.Contract.Next.Schema ()

import Language.Marlowe.Runtime.Web.Adapter.Servant (OperationId, RenameResponseSchema)
import Servant (
  Description,
  Summary,
  type (:>),
 )

type BurnsAPI = PostBurnsAPI

-- :<|> Capture "burnId" TxId :> BurnAPI

-- | POST /role-token-burns sub-API
type PostBurnsAPI =
  Summary "Burn role tokens"
    :> Description
        "Build an unsigned (Cardano) transaction body which burns role tokens matching a filter. \
        \Role tokens used by active contracts will not be burned and the request will fail if active role tokens are included. \
        \To submit the signed transaction, use the PUT /role-token-burns/{burnId} endpoint."
    :> OperationId "burnRoleTokens"
    :> RenameResponseSchema "BurnRoleTokensResponse"

-- :> ( ReqBody '[JSON] PostBurnRequest :> PostTxAPI (PostCreated '[JSON] (PostBurnResponse CardanoTxBody))
--       :<|> ReqBody '[JSON] PostBurnRequest :> PostTxAPI (PostCreated '[TxJSON BurnTx] (PostBurnResponse CardanoTx))
--    )
