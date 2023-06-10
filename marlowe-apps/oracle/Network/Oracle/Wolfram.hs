

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Network.Oracle.Wolfram
  ( Currency(..)
  , WolfCurrencyPair(..)
  , WolframApi
  , fetchCurrencyPairRaw
  , fetchWolfCurrencyPair
  , getCurrencyPair
  , getCurrencyPairRaw
  , wolframApi
  , wolframEnv
  ) where


import Data.Aeson.KeyMap (toList)
import Data.Aeson.Types (FromJSON(parseJSON), parseFail, toJSON, withObject, withText)
import Data.Bifunctor (first)
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Servant.API (Get, JSON, PlainText, QueryParam', Required, Strict, (:>))
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)
import Text.Regex.Base
import Text.Regex.Posix ((=~))

wolframEnv :: Manager -> IO ClientEnv
wolframEnv manager = mkClientEnv manager <$> parseBaseUrl "http://api.wolframalpha.com/v1/result"

appId :: Text
appId = "6WU6JX-46EP5U9AGX"

type WolframApi = QueryParam' '[Required,Strict] "appid" Text :> QueryParam' '[Required,Strict] "i" Text :> Get '[PlainText] Text


wolframApi :: Proxy WolframApi
wolframApi = Proxy

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
toQuoteCurrency USD = pure "usd"
toQuoteCurrency _   = Left "Unsupported quote currency."


data WolfCurrencyPair =
  WolfCurrencyPair
  {
    baseCurrency :: Currency
  , quoteCurrency :: Currency
  , rate :: Integer
  , scale :: Integer
  }
    deriving (Eq, Ord, Read, Show)

getCurrencyPair :: Currency -> Currency -> ClientM (Either String WolfCurrencyPair)
getCurrencyPair base quote =
  either (pure . Left) id
    $ getCurrencyPairRaw
    <$> toBaseCurrency base
    <*> toQuoteCurrency quote


fetchWolfCurrencyPair :: ClientEnv -> Currency -> Currency -> IO (Either String WolfCurrencyPair)
fetchWolfCurrencyPair env base quote = fmap (either (Left . show) id) $ getCurrencyPair base quote `runClientM` env

generateQuestion :: Text -> Text -> Text
generateQuestion base quote = "1 " <> base <> " to " <> quote

extractValue :: String -> Integer
extractValue answer = read (answer =~ ("[0-9]+" :: String)) :: Integer

getCurrencyPairRaw :: Text -> Text -> ClientM (Either String WolfCurrencyPair)
getCurrencyPairRaw base quote =
  client wolframApi appId (generateQuestion base quote) >>=
    (
      \case
        "" -> pure $ Left "No number returned"
        rateAnswer ->
            let
                baseCurrency = toBaseCurrency base
                quoteCurrency = toQuoteCurrency quote
            in pure $ Right $ WolfCurrencyPair{scale = 1, rate = extractValue rateAnswer, ..}
    )


fetchCurrencyPairRaw:: ClientEnv -> Text -> Text -> IO (Either String WolfCurrencyPair)
fetchCurrencyPairRaw env base quote = fmap (either (Left . show) id) $ getCurrencyPairRaw base quote `runClientM` env
