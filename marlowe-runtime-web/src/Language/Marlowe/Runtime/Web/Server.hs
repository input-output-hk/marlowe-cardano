{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

-- | This module defines a server for the root REST API.
module Language.Marlowe.Runtime.Web.Server (server) where

import Language.Marlowe.Runtime.Web.API (RuntimeAPI)
import Language.Marlowe.Runtime.Web.Adapter.Server.Monad (ServerM)
import qualified Language.Marlowe.Runtime.Web.Contract.Server as Contracts
import qualified Language.Marlowe.Runtime.Web.Payout.Server as Payouts
import qualified Language.Marlowe.Runtime.Web.Withdrawal.Server as Withdrawals
import Servant (
  HasServer (ServerT),
  NoContent (..),
  type (:<|>) ((:<|>)),
 )

server :: ServerT RuntimeAPI ServerM
server = Contracts.server :<|> Withdrawals.server :<|> Payouts.server :<|> healthcheckServer

healthcheckServer :: ServerM NoContent
healthcheckServer = pure NoContent
