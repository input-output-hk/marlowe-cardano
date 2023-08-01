{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON where

import Cardano.Api (AddressInEra, CardanoMode, LocalNodeConnectInfo)
import Contrib.Data.Aeson.Traversals qualified as A
import Contrib.Data.Time.Clock (nominalDiffTimeToMilliseconds)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.OneLine qualified as A
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.Has (Has (getter))
import Data.Map.Strict qualified as M
import Data.Proxy (Proxy)
import Data.Text qualified as Text
import Data.Time (NominalDiffTime)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe qualified as Marlowe
import Language.Marlowe.CLI.Sync (toPlutusAddress)
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currencies (Currencies),
  Currency (Currency, ccCurrencySymbol),
  CurrencyNickname (CurrencyNickname),
  Wallet (_waAddress),
  WalletNickname (WalletNickname),
  Wallets,
  getAllWallets,
 )
import Language.Marlowe.CLI.Types (CliError (CliError))
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Core.V1.Semantics.Types.Address qualified as Marlowe
import Plutus.V1.Ledger.Value qualified as PV

-- | Either a JSON of the Input or a JSON of the Contract which
-- | is parametrized by the currencies and wallets.
newtype ParametrizedMarloweJSON = ParametrizedMarloweJSON {unParametrizedMarloweJSON :: A.Value}
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON ParametrizedMarloweJSON where
  parseJSON = pure . ParametrizedMarloweJSON

instance ToJSON ParametrizedMarloweJSON where
  toJSON (ParametrizedMarloweJSON json) = json

rewriteCurrencyRefs :: Currencies -> ParametrizedMarloweJSON -> Either CurrencyNickname ParametrizedMarloweJSON
rewriteCurrencyRefs (Currencies currencies) (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    getCurrency nickname = case M.lookup nickname currencies of
      Nothing -> Left nickname
      Just currency -> pure currency
    rewrite = \case
      obj@(A.Object (KeyMap.toAscList -> props)) -> do
        case props of
          [("currency_symbol", A.String ""), ("token_name", A.String "")] -> pure obj
          [("currency_symbol", A.String currencyNickname), ("token_name", tokenName)] -> do
            let nickname = Text.unpack currencyNickname
            Currency{ccCurrencySymbol = PV.CurrencySymbol cs} <- getCurrency (CurrencyNickname nickname)
            pure $
              A.object
                [ ("currency_symbol", A.toJSON cs)
                , ("token_name", tokenName)
                ]
          _ -> pure obj
      v -> do
        pure v

data RewritePartyError era = WalletNotFound WalletNickname | InvalidWalletAddress WalletNickname (AddressInEra era)
  deriving stock (Eq, Generic, Show)

rewritePartyRefs
  :: Marlowe.Network -> Wallets era -> ParametrizedMarloweJSON -> Either (RewritePartyError era) ParametrizedMarloweJSON
rewritePartyRefs network wallets (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    getWallet nickname = case M.lookup nickname $ getAllWallets wallets of
      Nothing -> Left $ WalletNotFound nickname
      Just wallet -> pure wallet
    rewrite = \case
      A.Object (KeyMap.toList -> [("address", A.String walletNickname)]) -> do
        wallet <- getWallet (WalletNickname $ Text.unpack walletNickname)
        let address = toPlutusAddress . _waAddress $ wallet
        pure $ A.toJSON (Marlowe.Address network address)
      v -> do
        pure v

newtype Now = Now POSIXTime

rewriteTimeouts :: (Monad m) => Now -> ParametrizedMarloweJSON -> m ParametrizedMarloweJSON
rewriteTimeouts (Now n) (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    rewrite = \case
      A.Object (KeyMap.toList -> [("relative", A.Number difference)]) -> do
        let -- Diff is expressed in seconds
            diff :: NominalDiffTime
            diff = fromInteger (ceiling difference)

            t = n + diff
        pure $ A.toJSON $ nominalDiffTimeToMilliseconds t
      v -> pure v

rewriteParametrizedMarloweJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) ParametrizedMarloweJSON
rewriteParametrizedMarloweJSON network wallets currencies n json = do
  json' <- first RewritePartyFailure $ rewritePartyRefs network wallets json
  json'' <- first CurrencyNotFound $ rewriteCurrencyRefs currencies json'
  rewriteTimeouts n json''

data ParametrizedMarloweJSONDecodeError era
  = RewritePartyFailure (RewritePartyError era)
  | CurrencyNotFound CurrencyNickname
  | InvalidMarloweJSON String String
  deriving stock (Eq, Generic, Show)

decodeParametrizedContractJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) Marlowe.Contract
decodeParametrizedContractJSON network wallets currencies n json = do
  rewriteParametrizedMarloweJSON network wallets currencies n json >>= \(ParametrizedMarloweJSON contractJSON) ->
    case A.fromJSON contractJSON of
      A.Error err -> Left $ InvalidMarloweJSON err (Text.unpack $ A.renderValue contractJSON)
      A.Success contract -> pure contract

decodeParametrizedInputJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) Marlowe.Input
decodeParametrizedInputJSON network wallets currencies n json = do
  rewriteParametrizedMarloweJSON network wallets currencies n json >>= \(ParametrizedMarloweJSON contractJSON) ->
    case A.fromJSON contractJSON of
      A.Error err -> Left $ InvalidMarloweJSON err (Text.unpack $ A.renderValue contractJSON)
      A.Success input -> pure input

doRewriteParametrizedMarloweJSON
  :: forall env era st m
   . (MonadIO m)
  => (MonadError CliError m)
  => (MonadReader env m)
  => (Has (LocalNodeConnectInfo CardanoMode) env)
  => (MonadState st m)
  => (Has (Wallets era) st)
  => (Has Currencies st)
  => Proxy era
  -> ParametrizedMarloweJSON
  -> m ParametrizedMarloweJSON
doRewriteParametrizedMarloweJSON _ json = do
  network <- (asks getter :: m (LocalNodeConnectInfo CardanoMode)) <&> marloweNetworkFromLocalNodeConnectInfo
  (wallets :: Wallets era) <- gets getter
  (currencies :: Currencies) <- gets getter
  n <- now
  case rewriteParametrizedMarloweJSON network wallets currencies n json of
    Left err -> throwError $ CliError $ "Error rewriting parametrized Marlowe JSON: " <> show err
    Right json' -> pure json'

now :: (MonadIO m) => m Now
now = liftIO $ Now <$> getPOSIXTime
