{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Withdrawals
  where

import Data.Foldable (traverse_)
import Data.Functor ((<&>))
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Web hiding (Unsigned)
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.HistoryClient (LoadTxError(..))
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadWithdrawals)
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

type Withdrawals = [Withdrawal]

compile $ SelectorSpec "withdrawals"
  [ "get" ≔ FieldSpec "get"
      [ ["get", "contract", "id"] ≔ ''TxOutRef
      , ["start", "from"] ≔ ''TxId
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , "results"≔ ''Withdrawals
      ]
  ]

server
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> TxOutRef
  -> ServerT WithdrawalsAPI (AppM r)
server = get

get
  :: EventBackend (AppM r) r WithdrawalsSelector
  -> TxOutRef
  -> Maybe (Ranges '["withdrawalId"] GetWithdrawalsResponse)
  -> AppM r (PaginatedResponse '["withdrawalId"] GetWithdrawalsResponse)
get eb contractId ranges = withEvent eb Get \ev -> do
  let
    range :: Range "withdrawalId" TxId
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @Withdrawal)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ GetContractId contractId
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  contractId' <- fromDTOThrow err400 contractId
  startFrom <- fromDTOThrow err416 rangeValue
  let mods = setAncestor $ reference ev
  loadWithdrawals mods contractId' startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Left ContractNotFound -> throwError err404
    Left TxNotFound -> throwError err416
    Right withdrawals -> do
      let
        withdrawals' = toDTO withdrawals <&> \(block, withdrawalId) -> Withdrawal
          { withdrawalId
          , contractId
          , status = Confirmed
          , block = Just block
          }
      addField ev $ Results withdrawals'
      addHeader (length withdrawals') . fmap ListObject <$> returnRange range withdrawals'
