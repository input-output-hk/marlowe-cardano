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
import qualified Language.Marlowe.Runtime.Web.Server.REST.Withdrawals as Withdrawals
import Observe.Event.DSL (SelectorField(..), SelectorSpec(..))
import Observe.Event.Explicit (EventBackend, injectSelector, narrowEventBackend)
import Observe.Event.Render.JSON.DSL.Compile (compile)
import Observe.Event.Syntax ((≔))
import Servant

compile $ SelectorSpec "api"
  [ "contracts" ≔ Inject ''Contracts.ContractsSelector
  , "withdrawals" ≔ Inject ''Withdrawals.WithdrawalsSelector
  ]

server :: EventBackend IO r ApiSelector -> ServerT API (AppM r)
server eventBackend = Contracts.server (narrowEventBackend (injectSelector Contracts) eventBackend)
       :<|> Withdrawals.server (narrowEventBackend (injectSelector Withdrawals) eventBackend)
       :<|> healthcheckServer

healthcheckServer :: AppM r NoContent
healthcheckServer = pure NoContent
