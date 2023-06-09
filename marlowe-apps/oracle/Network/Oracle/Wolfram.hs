

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
coinGeckoEnv manager = mkClientEnv manager <$> parseBaseUrl "http://api.wolframalpha.com/v1/result"


type WolframApi = QueryParam' '[Required,Strict] "appid" "6WU6JX-46EP5U9AGX" :> QueryParam' '[Required,Strict] "i" Text :> Get '[JSON] CurrencyPairs


wolframApi :: Proxy WolframApi
wolframApi = Proxy


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


data Currency = BTC | USD
  deriving (Eq, Ord, Read, Show)

instance FromJSON Currency where
  parseJSON =
    withText "Currency"
      $ \case
        "btc"      -> pure BTC
        _          -> parseFail "Unrecognized currency."


toBaseCurrency :: Currency -> Either String Text
toBaseCurrency BTC = pure "bitcoin"
toBaseCurrency USD = pure "usd"
toBaseCurrency _   = Left "Unsupported base currency."


toQuoteCurrency :: Currency -> Either String Text
toQuoteCurrency BTC = pure "btc"
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
getCurrencyPairs = currencyPairs <$> client wolframApi "cardano,bitcoin,ethereum" "btc,eth,usd,eur,gbp,jpy"


fetchCurrencyPairs :: ClientEnv -> IO (Either String [CurrencyPair])
fetchCurrencyPairs = fmap (first show) . runClientM getCurrencyPairs


getCurrencyPair :: Currency -> Currency -> ClientM (Either String CurrencyPair)
getCurrencyPair base quote =
  either (pure . Left) id
    $ getCurrencyPairRaw
    <$> toBaseCurrency base
    <*> toQuoteCurrency quote


fetchWolfCurrencyPair :: ClientEnv -> Currency -> Currency -> IO (Either String CurrencyPair)
fetchWolfCurrencyPair env base quote = fmap (either (Left . show) id) $ getCurrencyPair base quote `runClientM` env

generateQuestion :: Text -> Text -> Text
generateQuestion base quote = "1 " ++ base ++ " to " ++ quote

getCurrencyPairRaw :: Text -> Text -> ClientM (Either String CurrencyPair)
getCurrencyPairRaw base quote =
  client coinGeckoApi (generateQuestion base quote) >>=
    (
      \case
        [pair] -> pure $ Right pair
        []     -> pure $ Left "Currency pair not found."
        _      -> pure $ Left "Duplicate currency rates."
    ) . currencyPairs


fetchCurrencyPairRaw:: ClientEnv -> Text -> Text -> IO (Either String CurrencyPair)
fetchCurrencyPairRaw env base quote = fmap (either (Left . show) id) $ getCurrencyPairRaw base quote `runClientM` env
