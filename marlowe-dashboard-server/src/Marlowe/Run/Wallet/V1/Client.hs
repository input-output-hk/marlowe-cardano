{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Marlowe.Run.Wallet.V1.Client
 ( callWBE
 , decodeError
 )
 where

import Cardano.Prelude hiding (Handler)
import Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Lazy as BL
import Servant.Client (ClientEnv, ClientError, ClientM, runClientM)

callWBE :: MonadIO m => MonadReader ClientEnv  m => ClientM a -> m (Either ClientError a)
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
