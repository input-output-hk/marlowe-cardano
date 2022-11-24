{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts/:contractId/transactions REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Transactions
  where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.HistoryClient (LoadContractHeadersError(..))
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadTransactions)
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

type TxHeaders = [TxHeader]

compile $ SelectorSpec "transactions"
  [ "get" ≔ FieldSpec "get"
      [ ["get", "contract", "id"] ≔ ''TxOutRef
      , ["start", "from"] ≔ ''TxId
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , ["tx", "headers"] ≔ ''TxHeaders
      ]
  ]

server
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> ServerT TransactionsAPI (AppM r)
server = get

get
  :: EventBackend (AppM r) r TransactionsSelector
  -> TxOutRef
  -> Maybe (Ranges '["transactionId"] TxHeader)
  -> AppM r (PaginatedResponse '["transactionId"] TxHeader)
get eb contractId ranges = withEvent eb Get \ev -> do
  let
    range :: Range "transactionId" TxId
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @TxHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ GetContractId contractId
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  contractId' <- fromDTOThrow err400 contractId
  startFrom <- fromDTOThrow err416 rangeValue
  let mods = setAncestor $ reference ev
  loadTransactions mods contractId' startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Left ContractNotFound -> throwError err404
    Left InitialTransactionNotFound -> throwError err416
    Right headers -> do
      let headers' = toDTO headers
      addField ev $ TxHeaders headers'
      addHeader (length headers) <$> returnRange range headers'
