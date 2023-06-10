

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Network.Oracle.Wolfram
  ( WolfCurrency(..)
  , WolfCurrencyPair(..)
  , WolframApi
  , fetchWolfCurrencyPair
  , getWolfCurrencyPair
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

data WolfCurrency = WOLF_BTC | WOLF_USD
  deriving (Eq, Ord, Read, Show)

toWolfBaseCurrency :: WolfCurrency -> Either String Text
toWolfBaseCurrency WOLF_BTC = pure "bitcoin"
toWolfBaseCurrency WOLF_USD = pure "usd"
toWolfBaseCurrency _   = Left "Unsupported base currency."

toWolfQuoteCurrency :: WolfCurrency -> Either String Text
toWolfQuoteCurrency WOLF_BTC = pure "btc"
toWolfQuoteCurrency WOLF_USD = pure "usd"
toWolfQuoteCurrency _   = Left "Unsupported quote currency."

data WolfCurrencyPair =
  WolfCurrencyPair
  {
    wolfBaseCurrency :: WolfCurrency
  , wolfQuoteCurrency :: WolfCurrency
  , wolfRate :: Integer
  , wolfScale :: Integer
  }
    deriving (Eq, Ord, Read, Show)

fetchWolfCurrencyPair :: ClientEnv -> WolfCurrency -> WolfCurrency -> IO (Either String WolfCurrencyPair)
fetchWolfCurrencyPair env base quote = fmap (either (Left . show) id) $ getWolfCurrencyPair base quote `runClientM` env

genewolfRateQuestion :: Text -> Text -> Text
genewolfRateQuestion base quote = "1 " <> base <> " to " <> quote

extractValue :: String -> Integer
extractValue answer = read (answer =~ ("[0-9]+" :: String)) :: Integer

getWolfCurrencyPair :: WolfCurrency -> WolfCurrency -> ClientM (Either String WolfCurrencyPair)
getWolfCurrencyPair wolfBaseCurrency wolfQuoteCurrency =
  let
    base = toWolfBaseCurrency wolfBaseCurrency
    quote = toWolfQuoteCurrency wolfQuoteCurrency
    question = genewolfRateQuestion <$> base <*> quote
  in sequence (client wolframApi appId <$> question) >>=
    (
      \case
        "" -> pure $ Left "No number returned"
        wolfRateAnswer -> pure $ Right $ WolfCurrencyPair{wolfScale = 1, wolfRate = extractValue wolfRateAnswer, ..}
    )

