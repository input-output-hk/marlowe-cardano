

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Network.Oracle.CoinGecko
  ( CoinGeckoApi
  , Currency(..)
  , CurrencyPair(..)
  , CurrencyPairs(..)
  , coinGeckoApi
  , coinGeckoEnv
  , fetchCurrencyPair
  , fetchCurrencyPairRaw
  , fetchCurrencyPairs
  , getCurrencyPair
  , getCurrencyPairRaw
  , getCurrencyPairs
  ) where


import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (FromJSON(parseJSON), parseFail, toJSON, withObject, withText)
import Data.Bifunctor (first)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Servant.API (Get, JSON, QueryParam', Required, Strict, (:>))
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)


coinGeckoEnv :: Manager -> IO ClientEnv
coinGeckoEnv manager = mkClientEnv manager <$> parseBaseUrl "https://api.coingecko.com"


type CoinGeckoApi = "api" :> "v3" :> "simple" :> "price" :> QueryParam' '[Required,Strict] "ids" Text :> QueryParam' '[Required,Strict] "vs_currencies" Text :> Get '[JSON] CurrencyPairs


coinGeckoApi :: Proxy CoinGeckoApi
coinGeckoApi = Proxy


newtype CurrencyPairs = CurrencyPairs {currencyPairs :: [CurrencyPair]}
  deriving (Eq, Ord, Read, Show)

instance FromJSON CurrencyPairs where
  parseJSON =
    withObject "CurrencyPairs"
      $ \o ->
        fmap (CurrencyPairs . concat)
          $ sequence
          [
            flip (withObject "CurrencyPair") o'
              $ \o'' ->
                do
                  baseCurrency <- parseJSON $ toJSON base
                  sequence
                    [
                      do
                        quoteCurrency <- parseJSON $ toJSON quote
                        let
                          scale = 100_000_000
                        rate <- (round :: Double -> Integer) . (fromInteger scale *) <$> parseJSON rate'
                        pure CurrencyPair{..}
                    |
                      (quote, rate') <- toList o''
                    ]
          |
            (base, o') <- toList o
          ]


data Currency = ADA | BTC | ETH | EUR | GBP | JPY | USD
  deriving (Eq, Ord, Read, Show)

instance FromJSON Currency where
  parseJSON =
    withText "Currency"
      $ \case
        "btc"      -> pure BTC
        "eth"      -> pure ETH
        "eur"      -> pure EUR
        "gbp"      -> pure GBP
        "jpy"      -> pure JPY
        "usd"      -> pure USD
        "bitcoin"  -> pure BTC
        "cardano"  -> pure ADA
        "ethereum" -> pure ETH
        _          -> parseFail "Unrecognized currency."


toBaseCurrency :: Currency -> Either String Text
toBaseCurrency ADA = pure "cardano"
toBaseCurrency BTC = pure "bitcoin"
toBaseCurrency ETH = pure "ethereum"
toBaseCurrency _   = Left "Unsupported base currency."


toQuoteCurrency :: Currency -> Either String Text
toQuoteCurrency BTC = pure "btc"
toQuoteCurrency ETH = pure "eth"
toQuoteCurrency EUR = pure "eur"
toQuoteCurrency GBP = pure "gbp"
toQuoteCurrency JPY = pure "jpy"
toQuoteCurrency USD = pure "usd"
toQuoteCurrency _   = Left "Unsupported quote currency."


data CurrencyPair =
  CurrencyPair
  {
    baseCurrency :: Currency
  , quoteCurrency :: Currency
  , rate :: Integer
  , scale :: Integer
  }
    deriving (Eq, Ord, Read, Show)


getCurrencyPairs :: ClientM [CurrencyPair]
getCurrencyPairs = currencyPairs <$> client coinGeckoApi "cardano,bitcoin,ethereum" "btc,eth,usd,eur,gbp,jpy"


fetchCurrencyPairs :: ClientEnv -> IO (Either String [CurrencyPair])
fetchCurrencyPairs = fmap (first show) . runClientM getCurrencyPairs


getCurrencyPair :: Currency -> Currency -> ClientM (Either String CurrencyPair)
getCurrencyPair base quote =
  either (pure . Left) id
    $ getCurrencyPairRaw
    <$> toBaseCurrency base
    <*> toQuoteCurrency quote


fetchCurrencyPair :: ClientEnv -> Currency -> Currency -> IO (Either String CurrencyPair)
fetchCurrencyPair env base quote = fmap (either (Left . show) id) $ getCurrencyPair base quote `runClientM` env


getCurrencyPairRaw :: Text -> Text -> ClientM (Either String CurrencyPair)
getCurrencyPairRaw base quote =
  client coinGeckoApi base quote >>=
    (
      \case
        [pair] -> pure $ Right pair
        []     -> pure $ Left "Currency pair not found."
        _      -> pure $ Left "Duplicate currency rates."
    ) . currencyPairs


fetchCurrencyPairRaw:: ClientEnv -> Text -> Text -> IO (Either String CurrencyPair)
fetchCurrencyPairRaw env base quote = fmap (either (Left . show) id) $ getCurrencyPairRaw base quote `runClientM` env
