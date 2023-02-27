{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines a server for the root REST API.

module Language.Marlowe.Runtime.Web.Server.REST
  where

import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.Monad (AppM)
import qualified Language.Marlowe.Runtime.Web.Server.REST.Contracts as Contracts
import Observe.Event (EventBackend, narrowEventBackend)
import Observe.Event.DSL (SelectorField(..), SelectorSpec(..))
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant

compile $ SelectorSpec "api"
  [ "contracts" ≔ Inject ''Contracts.ContractsSelector
  ]

server :: EventBackend (AppM r) r ApiSelector -> ServerT API (AppM r)
server eventBackend = Contracts.server (narrowEventBackend Contracts eventBackend) :<|> pure NoContent
