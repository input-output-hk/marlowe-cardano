{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Marlowe.Run.Wallet.V1.Server where

import Cardano.Prelude hiding (Handler)
import qualified Cardano.Wallet.Api.Client as WBE.Api
import qualified Cardano.Wallet.Api.Types as WBE
import Marlowe.Run.Dto (WalletIdDto, dtoHandler)
import Marlowe.Run.Wallet.V1 (getTotalFunds)
import Marlowe.Run.Wallet.V1.API (API, GetTotalFundsResponse (..))
import qualified Marlowe.Run.Wallet.V1.CentralizedTestnet.Server as CentralizedTestnet
import Marlowe.Run.Wallet.V1.Client (callWBE)
import Servant (ServerError, ServerT, err404, (:<|>) ((:<|>)))
import Servant.Client (ClientEnv)

handlers ::
    MonadIO m =>
    MonadReader ClientEnv m =>
    MonadError ServerError m =>
    ServerT API m
handlers = handleGetTotalFunds :<|> CentralizedTestnet.handlers

handleGetTotalFunds ::
    MonadIO m =>
    MonadError ServerError m =>
    MonadReader ClientEnv m =>
    WalletIdDto ->
    m GetTotalFundsResponse
handleGetTotalFunds =
    let
        getWallet =
            either (const $ throwError err404) pure
            <=< callWBE . WBE.Api.getWallet WBE.Api.walletClient . WBE.ApiT
    in
        dtoHandler $ getTotalFunds getWallet
