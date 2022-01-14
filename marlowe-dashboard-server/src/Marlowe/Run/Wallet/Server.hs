{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Marlowe.Run.Wallet.Server where

import Cardano.Prelude hiding (Handler)
import qualified Cardano.Wallet.Api.Client as WBE.Api
import qualified Cardano.Wallet.Api.Types as WBE
import Data.Text.Class (FromText (fromText))
import Marlowe.Run.Types (Env)
import Marlowe.Run.Wallet (getTotalFunds)
import Marlowe.Run.Wallet.API (API, GetTotalFundsDto (..), getTotalFundsToDto)
import qualified Marlowe.Run.Wallet.CentralizedTestnet.Server as CentralizedTestnet
import Marlowe.Run.Wallet.Client (callWBE)
import Servant (ServerError, ServerT, err400, err404, (:<|>) ((:<|>)))

handlers ::
    MonadIO m =>
    MonadReader Env m =>
    MonadError ServerError m =>
    ServerT API m
handlers = handleGetTotalFunds :<|> CentralizedTestnet.handlers

handleGetTotalFunds ::
    MonadIO m =>
    MonadError ServerError m =>
    MonadReader Env m =>
    Text ->
    m GetTotalFundsDto
handleGetTotalFunds walletIdText = do
    walletId <- either (const $ throwError err400) pure $ fromText walletIdText
    let
        getWallet =
            either (const $ throwError err404) pure
            <=< callWBE . WBE.Api.getWallet WBE.Api.walletClient . WBE.ApiT
    getTotalFundsToDto <$> getTotalFunds getWallet walletId
