{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON where

import Cardano.Api (AddressInEra, LocalNodeConnectInfo)
import Contrib.Data.Aeson.Traversals qualified as A
import Contrib.Data.Time.Clock (nominalDiffTimeToMilliseconds)
import Contrib.Data.Time.Units qualified as Time.Units
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader.Class (MonadReader, asks)
import Control.Monad.State.Class (MonadState, gets)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Text qualified as A
import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Has (Has (getter))
import Data.Map.Strict qualified as M
import Data.Map.Strict qualified as Map
import Data.Proxy (Proxy)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as TL
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import Data.Time.Units (Microsecond, TimeUnit (..))
import Data.Vector qualified as Vector
import Debug.Trace (traceM)
import GHC.Generics (Generic)
import Language.Marlowe qualified as Marlowe
import Language.Marlowe.CLI.Run (toPlutusAddress)
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currencies (Currencies),
  Currency (Currency, ccCurrencySymbol),
  CurrencyNickname (CurrencyNickname),
  Wallet (_waAddress),
  WalletNickname (WalletNickname),
  Wallets,
  allWalletsMap,
 )
import Language.Marlowe.CLI.Types (CliError (CliError), SomeTimeout (..))
import Language.Marlowe.Cardano (marloweNetworkFromLocalNodeConnectInfo)
import Language.Marlowe.Core.V1.Semantics.Types.Address qualified as Marlowe
import PlutusLedgerApi.V1.Value qualified as PV

-- | Either a JSON of the Input or a JSON of the Contract which
-- | is parametrized by the currencies and wallets.
newtype ParametrizedMarloweJSON = ParametrizedMarloweJSON {unParametrizedMarloweJSON :: A.Value}
  deriving stock (Eq, Ord, Generic, Show)

instance FromJSON ParametrizedMarloweJSON where
  parseJSON = pure . ParametrizedMarloweJSON

instance ToJSON ParametrizedMarloweJSON where
  toJSON (ParametrizedMarloweJSON json) = json

-- It can be:

-- * a pair (A.Array [A.String "", A.String ""])

-- * a pair (A.Array [A.String currencyNickname, A.String tokenName])

-- * (A.String "ADA")

-- * (A.Object [("currency_symbol", A.String ""), ("token_name", A.String "")])

-- * (A.Object [("currency_symbol", A.String currencyNickname), ("token_name", A.String tokenName)])
rewriteCurrencyProps :: Currencies -> A.Value -> Either A.Value A.Value
rewriteCurrencyProps (Currencies currencies) json = rewrite json
  where
    getCurrency nickname = case M.lookup (CurrencyNickname nickname) currencies of
      Nothing -> Left json
      Just currency -> pure currency

    rewrite = \case
      A.Object (KeyMap.toList -> [("currency_symbol", A.String ""), ("token_name", A.String "")]) -> do
        pure $ A.object [("currency_symbol", A.String ""), ("token_name", A.String "")]
      A.Object (KeyMap.toList -> [("currency_symbol", A.String currencyNickname), ("token_name", tokenName)]) -> do
        Currency{ccCurrencySymbol = PV.CurrencySymbol cs} <- getCurrency (Text.unpack currencyNickname)
        pure $
          A.object
            [ ("currency_symbol", A.toJSON cs)
            , ("token_name", tokenName)
            ]
      A.Array (Vector.toList -> [A.String "", A.String ""]) -> do
        pure $ A.object [("currency_symbol", A.String ""), ("token_name", A.String "")]
      A.Array (Vector.toList -> [A.String currencyNickname, A.String tokenName]) -> do
        Currency{ccCurrencySymbol = PV.CurrencySymbol cs} <- getCurrency (Text.unpack currencyNickname)
        pure $
          A.object
            [ ("currency_symbol", A.toJSON cs)
            , ("token_name", A.String tokenName)
            ]
      A.String (Text.toLower -> "ada") -> do
        pure $ A.object [("currency_symbol", A.String ""), ("token_name", A.String "")]
      _ -> Left json

rewriteCurrencyRefs :: Currencies -> ParametrizedMarloweJSON -> Either A.Value ParametrizedMarloweJSON
rewriteCurrencyRefs currencies (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    rewrite = \case
      A.Object (Map.fromList . KeyMap.toAscList -> props) -> do
        props' <- case Map.lookup "token" props of
          Just c -> rewriteCurrencyProps currencies c <&> \t -> Map.insert "token" t props
          Nothing -> case Map.lookup "of_token" props of
            Just c -> rewriteCurrencyProps currencies c <&> \t -> Map.insert "of_token" t props
            Nothing -> pure props
        pure $ A.object $ Map.toList props'
      v -> do
        pure v

data RewritePartyError era = WalletNotFound WalletNickname | InvalidWalletAddress WalletNickname (AddressInEra era)
  deriving stock (Eq, Generic, Show)

rewritePartyRefs
  :: Marlowe.Network -> Wallets era -> ParametrizedMarloweJSON -> Either (RewritePartyError era) ParametrizedMarloweJSON
rewritePartyRefs network wallets (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON <$> A.rewriteBottomUp rewrite json
  where
    getWallet nickname = case M.lookup nickname $ allWalletsMap wallets of
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
      A.Object (KeyMap.toList -> [("relative", A.Number duration)]) -> do
        let diff = Time.Units.toNominalDiffTime $ (fromMicroseconds :: Integer -> Microsecond) $ ceiling duration * 1_000_000
        pure $ A.toJSON $ nominalDiffTimeToMilliseconds (n + diff)
      obj@(A.Object (Map.fromList . KeyMap.toList -> props)) -> do
        case Map.lookup "timeout" props of
          Just str@(A.String _) -> do
            let v =
                  A.fromJSON str & \case
                    A.Success (RelativeTimeout diff) -> A.toJSON $ nominalDiffTimeToMilliseconds (n + diff)
                    _ -> obj
            traceM $ show (A.fromJSON str :: A.Result SomeTimeout)
            pure $ A.object $ Map.toList $ Map.insert "timeout" v props
          _ -> pure obj
      v -> pure v

-- We turn:
-- ```
--  timeout: timeout_value
--  timeout_continuation: timeout_continuation
--  unless: input
--  then: continauation
-- ```
-- into:
-- ```
--  when:
--    - case: input
--      then: continuation
--  timeout: timeout_value
--  timeout_continuation: timeout_continuation
--  ```
rewriteUnlessSyntax :: ParametrizedMarloweJSON -> ParametrizedMarloweJSON
rewriteUnlessSyntax (ParametrizedMarloweJSON json) = ParametrizedMarloweJSON $ runIdentity $ A.rewriteBottomUp rewrite json
  where
    rewrite = \case
      A.Object
        ( KeyMap.toAscList ->
            [("then", continuation), ("timeout", timeout), ("timeout_continuation", timeoutContinuation), ("unless", input)]
          ) -> do
          pure $
            A.object
              [
                ( "when"
                , A.Array $
                    Vector.fromList
                      [ A.object
                          [ ("case", input)
                          , ("then", continuation)
                          ]
                      ]
                )
              , ("timeout", timeout)
              , ("timeout_continuation", timeoutContinuation)
              ]
      v -> pure v

rewriteParametrizedMarloweJSON
  :: Marlowe.Network
  -> Wallets era
  -> Currencies
  -> Now
  -> ParametrizedMarloweJSON
  -> Either (ParametrizedMarloweJSONDecodeError era) ParametrizedMarloweJSON
rewriteParametrizedMarloweJSON network wallets currencies n json = do
  json' <-
    first RewritePartyFailure $
      rewritePartyRefs network wallets $
        rewriteUnlessSyntax json
  json'' <- first InvalidAsset $ rewriteCurrencyRefs currencies json'
  rewriteTimeouts n json''

data ParametrizedMarloweJSONDecodeError era
  = RewritePartyFailure (RewritePartyError era)
  | InvalidAsset A.Value
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
      A.Error err -> Left $ InvalidMarloweJSON err (TL.unpack $ A.encodeToLazyText contractJSON)
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
      A.Error err -> Left $ InvalidMarloweJSON err (TL.unpack $ A.encodeToLazyText contractJSON)
      A.Success input -> pure input

doRewriteParametrizedMarloweJSON
  :: forall env era st m
   . (MonadIO m)
  => (MonadError CliError m)
  => (MonadReader env m)
  => (Has LocalNodeConnectInfo env)
  => (MonadState st m)
  => (Has (Wallets era) st)
  => (Has Currencies st)
  => Proxy era
  -> ParametrizedMarloweJSON
  -> m ParametrizedMarloweJSON
doRewriteParametrizedMarloweJSON _ json = do
  network <- (asks getter :: m LocalNodeConnectInfo) <&> marloweNetworkFromLocalNodeConnectInfo
  (wallets :: Wallets era) <- gets getter
  (currencies :: Currencies) <- gets getter
  n <- now
  case rewriteParametrizedMarloweJSON network wallets currencies n json of
    Left err -> throwError $ CliError $ "Error rewriting parametrized Marlowe JSON: " <> show err
    Right json' -> pure json'

now :: (MonadIO m) => m Now
now = liftIO $ Now <$> getPOSIXTime
