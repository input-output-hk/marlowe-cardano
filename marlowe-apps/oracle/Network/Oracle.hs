

{-# LANGUAGE RecordWildCards #-}


module Network.Oracle
  ( Oracle(..)
  , OracleEnv
  , makeOracle
  , oracles
  , readOracle
  ) where


import Network.HTTP.Client (Manager)
import Network.Oracle.CoinGecko (Currency(..), CurrencyPair(..), coinGeckoEnv, fetchCurrencyPair)
import Network.Oracle.Sofr (fetchSofrBasisPoints, nyfrbEnv)
import Servant.Client (ClientEnv)


data Oracle = SOFR | BTCETH | BTCEUR | BTCGBP | BTCJPY | BTCUSD | ADABTC | ADAETH | ADAEUR | ADAGBP | ADAJPY | ADAUSD | ETHBTC | ETHEUR | ETHGBP | ETHJPY | ETHUSD
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


oracles :: [Oracle]
oracles = [minBound..maxBound]


data OracleEnv =
  OracleEnv
  {
    nyfrb :: ClientEnv
  , coinGecko :: ClientEnv
  }


makeOracle :: Manager -> IO OracleEnv
makeOracle manager = OracleEnv <$> nyfrbEnv manager <*> coinGeckoEnv manager


readOracle :: OracleEnv -> Oracle -> IO (Either String Integer)
readOracle OracleEnv{..} SOFR   = fetchSofrBasisPoints nyfrb
readOracle OracleEnv{..} BTCETH = fmap rate <$> fetchCurrencyPair coinGecko BTC ETH
readOracle OracleEnv{..} BTCEUR = fmap rate <$> fetchCurrencyPair coinGecko BTC EUR
readOracle OracleEnv{..} BTCGBP = fmap rate <$> fetchCurrencyPair coinGecko BTC GBP
readOracle OracleEnv{..} BTCJPY = fmap rate <$> fetchCurrencyPair coinGecko BTC JPY
readOracle OracleEnv{..} BTCUSD = fmap rate <$> fetchCurrencyPair coinGecko BTC USD
readOracle OracleEnv{..} ADABTC = fmap rate <$> fetchCurrencyPair coinGecko ADA BTC
readOracle OracleEnv{..} ADAETH = fmap rate <$> fetchCurrencyPair coinGecko ADA ETH
readOracle OracleEnv{..} ADAEUR = fmap rate <$> fetchCurrencyPair coinGecko ADA EUR
readOracle OracleEnv{..} ADAGBP = fmap rate <$> fetchCurrencyPair coinGecko ADA GBP
readOracle OracleEnv{..} ADAJPY = fmap rate <$> fetchCurrencyPair coinGecko ADA JPY
readOracle OracleEnv{..} ADAUSD = fmap rate <$> fetchCurrencyPair coinGecko ADA USD
readOracle OracleEnv{..} ETHBTC = fmap rate <$> fetchCurrencyPair coinGecko ETH BTC
readOracle OracleEnv{..} ETHEUR = fmap rate <$> fetchCurrencyPair coinGecko ETH EUR
readOracle OracleEnv{..} ETHGBP = fmap rate <$> fetchCurrencyPair coinGecko ETH GBP
readOracle OracleEnv{..} ETHJPY = fmap rate <$> fetchCurrencyPair coinGecko ETH JPY
readOracle OracleEnv{..} ETHUSD = fmap rate <$> fetchCurrencyPair coinGecko ETH USD
