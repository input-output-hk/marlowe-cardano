{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import Servant

server :: ServerT API AppM
server = Contracts.server :<|> Withdrawals.server :<|> healthcheckServer

healthcheckServer :: AppM NoContent
healthcheckServer = pure NoContent
