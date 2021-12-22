{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Webserver.Wallet.Client
 ( callWBE
 , decodeError
 )
 where

import           Cardano.Prelude             hiding (Handler)


import           Data.Aeson                  as Aeson
import qualified Data.Aeson.Types            as Aeson

import qualified Data.ByteString.Lazy        as BL

import           Marlowe.Run.Webserver.Types (Env)
import           Servant.Client              (ClientError, ClientM, runClientM)

callWBE :: MonadIO m => MonadReader Env m => ClientM a -> m (Either ClientError a)
callWBE client = do
    clientEnv <- ask
    liftIO $ runClientM client clientEnv

-- NOTE: This was copied from Cardano-wallet/Cardano.Cli
decodeError
    :: BL.ByteString
    -> Maybe Text
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj
