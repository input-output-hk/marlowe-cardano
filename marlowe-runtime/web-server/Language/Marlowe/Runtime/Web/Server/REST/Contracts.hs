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
import Language.Marlowe.Runtime.Web.Server.Monad (AppM, loadContractHeaders)
import Observe.Event (EventBackend, addField, withEvent)
import Observe.Event.BackendModification (EventBackendModifiers, modifyEventBackend)
import Observe.Event.DSL (FieldSpec(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant
import Servant.Pagination

type ContractHeaders = [ContractHeader]

compile $ SelectorSpec "contracts"
  [ "get" ≔ FieldSpec ["get", "contracts"]
      [ ["start", "from"] ≔ ''TxOutRef
      , "limit" ≔ ''Int
      , "offset" ≔ ''Int
      , "order" ≔ ''String
      , ["contract", "headers"] ≔ ''ContractHeaders
      ]
  ]

server
  :: EventBackend AppM r ContractsSelector
  -> EventBackendModifiers r r'
  -> ServerT ContractsAPI AppM
server = get

get
  :: EventBackend AppM r ContractsSelector
  -> EventBackendModifiers r r'
  -> Maybe (Ranges '["contractId"] ContractHeader)
  -> AppM (PaginatedResponse '["contractId"] ContractHeader)
get eventBackend mods ranges = withEvent (modifyEventBackend mods eventBackend) Get \ev -> do
  let
    range :: Range "contractId" TxOutRef
    range@Range{..} = fromMaybe (getDefaultRange (Proxy @ContractHeader)) $ extractRange =<< ranges
  traverse_ (addField ev . StartFrom) rangeValue
  addField ev $ Limit rangeLimit
  addField ev $ Offset rangeOffset
  addField ev $ Order $ show rangeOrder
  startFrom <- case traverse fromDTO rangeValue of
    Nothing -> throwError err404
    Just startFrom -> pure startFrom
  loadContractHeaders startFrom rangeLimit rangeOffset rangeOrder >>= \case
    Nothing -> throwError err416
    Just headers -> do
      let headers' = toDTO headers
      addField ev $ ContractHeaders headers'
      addHeader (length headers) <$> returnRange range headers'
