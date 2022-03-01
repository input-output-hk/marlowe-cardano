{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}

{-# OPTIONS_GHC -Wno-orphans       #-}

module Marlowe.Run.Wallet.V1.CentralizedTestnet.Server
 ( handlers
 )
 where

import Cardano.Mnemonic (SomeMnemonic (..))
import Cardano.Prelude hiding (Handler, log)
import Cardano.Wallet.Api (WalletKeys)
import qualified Cardano.Wallet.Api.Client as WBE.Api
import Cardano.Wallet.Api.Types (ApiVerificationKeyShelley (..))
import qualified Cardano.Wallet.Api.Types as WBE
import qualified Cardano.Wallet.Primitive.AddressDerivation as WBE
import qualified Cardano.Wallet.Primitive.Types as WBE
import Colog (log, pattern D, pattern E)
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Internal.Strict as HashMap
import qualified Data.Text as Text
import Data.Text.Class (FromText (..))
import Ledger (PaymentPubKeyHash (..), PubKeyHash (..))
import Marlowe.Run.Env (HasEnv)
import qualified Marlowe.Run.Wallet.V1.CentralizedTestnet as Service
import Marlowe.Run.Wallet.V1.CentralizedTestnet.API (API)
import Marlowe.Run.Wallet.V1.CentralizedTestnet.Types
import Marlowe.Run.Wallet.V1.Client (callWBE, decodeError)
import Marlowe.Run.Wallet.V1.Types (Address (Address), WalletId (WalletId), WalletInfo (..), WalletName (unWalletName))
import PlutusTx.Builtins.Internal (BuiltinByteString (..))
import Servant (ServerError, ServerT, err500, (:<|>) ((:<|>)), (:>))
import Servant.Client (ClientError (FailureResponse), ClientM, ResponseF (responseBody), client)
import Text.Regex (Regex)
import qualified Text.Regex as Regex

handlers ::
    MonadIO m =>
    MonadError ServerError m =>
    HasEnv m =>
    ServerT API m
handlers = restoreWallet :<|> createWallet

createWallet ::
     MonadIO m =>
     HasEnv m =>
     MonadError ServerError  m =>
     CreatePostData ->
     m CreateResponse
createWallet CreatePostData{..} = do
  let walletName = unWalletName getCreateWalletName
  let passphrase = unPassphrase getCreatePassphrase
  (mnemonic, walletId, pubKeyHash, address) <-
    Service.createWallet getPubKeyHashFromWallet getAddress postWallet walletName passphrase
  pure
    $ CreateResponse (CreateMnemonic mnemonic)
    $ WalletInfo{ walletId = WalletId walletId, pubKeyHash = PaymentPubKeyHash pubKeyHash, address = Address address }

-- [UC-WALLET-TESTNET-2][2] Restore a testnet wallet
restoreWallet ::
     MonadIO m =>
     HasEnv m =>
     MonadError ServerError m =>
     RestorePostData ->
     m WalletInfo
restoreWallet RestorePostData{..} = do
    let mnemonic = unRestoreMnemonic getRestoreMnemonicPhrase
    let walletName = unWalletName getRestoreWalletName
    let passphrase = unPassphrase getRestorePassphrase
    (walletId, pubKeyHash, address) <-
      Service.restoreWallet getPubKeyHashFromWallet getAddress postWallet mnemonic walletName passphrase
    pure $ WalletInfo { walletId = WalletId walletId, pubKeyHash = PaymentPubKeyHash pubKeyHash, address = Address address }

getPubKeyHashFromWallet ::
    MonadIO m =>
    MonadError ServerError m =>
    HasEnv m =>
    WBE.WalletId ->
    m PubKeyHash
getPubKeyHashFromWallet walletId = do
  let
    -- This endpoint is not exposed directly by the WBE, I took this helper from the plutus-pab code.
    getWalletKey :: WBE.ApiT WBE.WalletId -> WBE.ApiT WBE.Role -> WBE.ApiT WBE.DerivationIndex -> Maybe Bool -> ClientM WBE.ApiVerificationKeyShelley
    getWalletKey :<|> _ :<|> _ :<|> _ = client (Proxy @("v2" :> WalletKeys))

    -- makeRequest :: String
    makeRequest = callWBE $
       getWalletKey
        (WBE.ApiT walletId)
        -- Role: External, to receive funds
        (WBE.ApiT WBE.UtxoExternal)
        -- DerivationIndex: first address
        (WBE.ApiT (WBE.DerivationIndex 0))
        -- Return hashed version
        (Just True)

    transformResponse = (fmap . fmap) (PubKeyHash . BuiltinByteString . fst . getApiVerificationKey)
  either (const $ throwError err500) pure =<< transformResponse makeRequest

getAddress ::
    MonadIO m =>
    MonadError ServerError m =>
    HasEnv m =>
    WBE.WalletId ->
    m Text
getAddress walletId = do
  response <- callWBE $ WBE.Api.listAddresses WBE.Api.addressClient (WBE.ApiT walletId) Nothing
  case response of
    Right ((Aeson.Object obj) : _) ->
      case HashMap.lookup "id" obj of
        (Just (Aeson.String s)) -> pure s
        _                       -> throwError err500
    _                              -> throwError err500

postWallet ::
    MonadIO m =>
    HasEnv m =>
    MonadError ServerError m =>
    WBE.WalletName ->
    WBE.Passphrase "raw" ->
    SomeMnemonic ->
    m WBE.WalletId
postWallet walletName passphrase mnemonic = do
    let
        walletPostData = WBE.WalletOrAccountPostData $ Left $ WBE.WalletPostData
            Nothing
            (WBE.ApiMnemonicT mnemonic)
            Nothing
            (WBE.ApiT walletName)
            (WBE.ApiT passphrase)

    result <- callWBE $ WBE.Api.postWallet WBE.Api.walletClient walletPostData
    case result of
        Left err@(FailureResponse _ r) -> do
            let
                matchDuplicateWallet :: Regex
                matchDuplicateWallet = Regex.mkRegex "wallet with the following id: ([a-z0-9]{40})"

                mWalletIdFromErr :: Maybe WBE.WalletId
                mWalletIdFromErr = do
                    msg <- decodeError $ responseBody r
                    matches <- Regex.matchRegex matchDuplicateWallet $ Text.unpack msg
                    walletIdStr <- headMay matches
                    rightToMaybe $ fromText $ Text.pack walletIdStr
            case mWalletIdFromErr of
                Just walletId -> pure walletId
                Nothing       ->  do
                    log E $ "restoreWallet failed: " <> show err
                    throwError err500
        Left err -> do
            log E $ "restoreWallet failed: " <> show err
            throwError err500
        Right WBE.ApiWallet{WBE.id = WBE.ApiT walletId} -> do
            log D $ "Restored wallet: " <> show walletId
            pure walletId
