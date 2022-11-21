{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the /contracts REST API.

module Language.Marlowe.Runtime.Web.Server.REST.Contracts
  where

import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.DTO
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadContract, loadContractHeaders)
import Observe.Event (EventBackend, addField, reference, withEvent)
import Observe.Event.BackendModification (setAncestor)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

type ContractHeaders = [ContractHeader]

compile $ SelectorSpec "contracts"
  [ "get" ≔ FieldSpec "get"
      [ ["start", "from"] ≔ ''TxOutRef
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , ["contract", "headers"] ≔ ''ContractHeaders
      ]
  , ["get", "one"] ≔ FieldSpec ["get", "one"]
      [ ["get", "id"] ≔ ''TxOutRef
      , ["get", "result"] ≔ ''ContractState
      ]
  ]

server
  :: EventBackend (AppM r) r ContractsSelector
  -> ServerT ContractsAPI (AppM r)
server eb = get eb :<|> contractServer eb

contractServer
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> ServerT ContractAPI (AppM r)
contractServer = getOne

get
  :: EventBackend (AppM r) r ContractsSelector
  -> Maybe (Ranges '["contractId"] GetContractsResponse)
  -> AppM r (PaginatedResponse '["contractId"] GetContractsResponse)
get eb ranges = withEvent eb Get \ev -> do
  let
    range :: Range "contractId" TxOutRef
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  startFrom <- fromDTOThrow err416 rangeValue
  loadContractHeaders startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Nothing -> throwError err416
    Just headers -> do
      let headers' = toDTO headers
      addField ev $ ContractHeaders headers'
      let response = IncludeLink api (Proxy @"contract") <$> headers'
      addHeader (length headers) <$> returnRange range response

getOne
  :: EventBackend (AppM r) r ContractsSelector
  -> TxOutRef
  -> AppM r ContractState
getOne eb contractId = withEvent eb GetOne \ev -> do
  addField ev $ GetId contractId
  contractId' <- fromDTOThrow err400 contractId
  loadContract (setAncestor $ reference ev) contractId' >>= \case
    Nothing -> throwError err404
    Just contractRecord -> do
      let contractState = toDTO contractRecord
      addField ev $ GetResult contractState
      pure contractState
