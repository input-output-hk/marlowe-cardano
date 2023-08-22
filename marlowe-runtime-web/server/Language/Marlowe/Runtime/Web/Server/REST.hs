{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the root REST API.
module Language.Marlowe.Runtime.Web.Server.REST where

import Language.Marlowe.Runtime.Web
import Language.Marlowe.Runtime.Web.Server.Monad (ServerM)
import qualified Language.Marlowe.Runtime.Web.Server.REST.Contracts as Contracts
import qualified Language.Marlowe.Runtime.Web.Server.REST.Payouts as Payouts
import qualified Language.Marlowe.Runtime.Web.Server.REST.Withdrawals as Withdrawals
import Servant

server :: ServerT API ServerM
server = Contracts.server :<|> Withdrawals.server :<|> Payouts.server :<|> healthcheckServer

healthcheckServer :: ServerM NoContent
healthcheckServer = pure NoContent
