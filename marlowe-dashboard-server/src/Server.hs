{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Server where

import           API                                        (API)
import           Cardano.Mnemonic                           (mkSomeMnemonic)

import qualified Cardano.Wallet.Api.Client                  as Wallet.Api
import qualified Cardano.Wallet.Api.Types                   as Wallet.Types
import qualified Cardano.Wallet.Primitive.Types             as Wallet.Types

import           Control.Monad.Except                       (ExceptT)
import           Control.Monad.IO.Class                     (MonadIO, liftIO)
import           Control.Monad.Logger                       (LoggingT, MonadLogger, logInfoN, runStderrLoggingT)
import           Control.Monad.Reader                       (ReaderT, runReaderT)
import           Data.Aeson                                 (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Aeson                                 as Aeson
import qualified Data.Aeson.Types                           as Aeson

import           Cardano.Wallet.Primitive.AddressDerivation (Passphrase (Passphrase))
import qualified Data.ByteString.Lazy                       as BL
import           Data.Maybe                                 (fromMaybe)
import           Data.Proxy                                 (Proxy (Proxy))
import           Data.String                                as S
import           Data.Text                                  (Text)
import qualified Data.Text                                  as Text
import           Data.Text.Class                            (FromText (..))
import qualified Data.Text.Encoding                         as Text
import           GHC.Generics                               (Generic)
import           Network.HTTP.Client                        (defaultManagerSettings, newManager)
import           Network.Wai.Middleware.Cors                (cors, corsRequestHeaders, simpleCorsResourcePolicy)
import           Servant                                    (Application, Handler (Handler), Server, ServerError,
                                                             errBody, hoistServer, serve, serveDirectoryFileServer,
                                                             (:<|>) ((:<|>)), (:>))
import           Servant.Client                             (BaseUrl (BaseUrl, baseUrlHost, baseUrlPath, baseUrlPort, baseUrlScheme),
                                                             ClientEnv (manager), ClientError (FailureResponse),
                                                             ClientM, ResponseF (responseBody), Scheme (Http),
                                                             mkClientEnv, runClientM)
import           Text.Regex                                 (Regex)
import qualified Text.Regex                                 as Regex
import           Types                                      (RestoreError (..), RestorePostData (..))
import qualified WebSocket                                  as WS

handlers :: FilePath -> Server API
handlers staticPath =
        WS.handle
        :<|> (handleVersion
                :<|> restoreWallet
             )
        :<|> serveDirectoryFileServer staticPath


-- TODO: Can we get this from cabal somehow?
handleVersion :: Handler Text
handleVersion = pure "1.0.0.0"

-- FIXME: Reuse connection and setup using configuration
callWBE :: ClientM a -> IO (Either ClientError a)
callWBE client = do
    manager <- newManager defaultManagerSettings
    let baseUrl = BaseUrl{baseUrlScheme=Http,baseUrlHost="localhost",baseUrlPort=8090,baseUrlPath=""}
        clientEnv = mkClientEnv manager baseUrl
    runClientM client clientEnv



restoreWallet :: MonadIO m => RestorePostData -> m (Either RestoreError Text)
restoreWallet d =
    let
        phrase = getMnemonicPhrase d
        passphrase = Text.unpack $ getPassphrase d
        mMnemonic = Wallet.Types.ApiMnemonicT <$> mkSomeMnemonic @'[15, 18, 21, 24] phrase
    in
      case mMnemonic of
        Left _ -> pure $ Left InvalidMnemonic
        Right mnemonic -> do
            liftIO $ putStrLn "Hello?"
            let
                walletPostData = Wallet.Types.WalletOrAccountPostData $ Left $ Wallet.Types.WalletPostData
                    Nothing
                    mnemonic
                    Nothing
                    -- FIXME: get this from parameter
                    (Wallet.Types.ApiT $ Wallet.Types.WalletName "plutus-wallet")
                    (Wallet.Types.ApiT $ Passphrase $ fromString passphrase )
            result <- liftIO $ callWBE $ Wallet.Api.postWallet Wallet.Api.walletClient walletPostData
            liftIO $ putStrLn "Called client: "

            -- liftIO $ putStrLn $ show $ toJSON <$> result
            case result of
                Left (FailureResponse _ r) -> do
                    let
                        matchDuplicateWallet :: Regex
                        matchDuplicateWallet = Regex.mkRegex "wallet with the following id: ([a-z0-9]{40})"

                        mWalletIdFromErr :: Maybe Wallet.Types.WalletId
                        mWalletIdFromErr = do
                            msg <- decodeError $ responseBody r
                            matches <- Regex.matchRegex matchDuplicateWallet msg
                            walletIdStr <- safeHead matches
                            hush $ fromText $ Text.pack walletIdStr

                    case mWalletIdFromErr of
                        Just walletId -> pure $ Right $ Text.pack $ show walletId
                        Nothing       -> pure $ Left UnknownRestoreError
                    -- pure $ Left InvalidMnemonic
                Left err -> do
                    liftIO $ putStrLn "restoreWallet failed"
                    liftIO $ putStrLn $ "Error: " <> show err
                    pure $ Left UnknownRestoreError
                Right (Wallet.Types.ApiWallet (Wallet.Types.ApiT walletId) _ _ _ _ _ _ _ _) -> do
                    liftIO $ putStrLn $ "Restored wallet: " <> show walletId
                    pure $ Right $ Text.pack $ show walletId

-- NOTE: This was copied from Cardano-wallet/Cardano.Cli and changed return type from Text to String
decodeError
    :: BL.ByteString
    -> Maybe String
decodeError bytes = do
    obj <- Aeson.decode bytes
    Aeson.parseMaybe (Aeson.withObject "Error" (.: "message")) obj

-- NOTE: Coppied from plutus-apps/playground-common/Schema.hs with same note
-- We could take this from the `safe` package, but I don't think it's worth the extra dependency.
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

app :: FilePath -> Application
app staticPath =
  cors (const $ Just policy) $ serve (Proxy @API) (handlers staticPath)
  where
    policy =
      simpleCorsResourcePolicy

initializeApplication :: FilePath -> IO Application
initializeApplication staticPath = pure $ app staticPath
