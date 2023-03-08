

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Network.Oracle
  ( Oracle(..)
  , OracleEnv
  , makeOracle
  , oracles
  , readOracle
  , toOracleSymbol
  ) where


import Data.Maybe (fromJust)
import Data.Text (Text)
import Network.HTTP.Client (Manager)
import Network.Oracle.CoinGecko (Currency(..), CurrencyPair(..), coinGeckoEnv, fetchCurrencyPair)
import Network.Oracle.Sofr (fetchSofrBasisPoints, nyfrbEnv)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import Observe.Event.Syntax ((≔))
import Servant.Client (ClientEnv)
import Text.Read (readMaybe)


data Oracle = SOFR | BTCETH | BTCEUR | BTCGBP | BTCJPY | BTCUSD | ADABTC | ADAETH | ADAEUR | ADAGBP | ADAJPY | ADAUSD | ETHBTC | ETHEUR | ETHGBP | ETHJPY | ETHUSD
  deriving (Bounded, Enum, Eq, Ord, Read, Show)


toOracleSymbol :: String -> Maybe Oracle
toOracleSymbol = readMaybe


oracles :: [Oracle]
oracles = [minBound..maxBound]


pairs :: [(Oracle, (Currency, Currency))]
pairs =
  [
    (BTCETH, (BTC, ETH))
  , (BTCEUR, (BTC, EUR))
  , (BTCGBP, (BTC, GBP))
  , (BTCJPY, (BTC, JPY))
  , (BTCUSD, (BTC, USD))
  , (ADABTC, (ADA, BTC))
  , (ADAETH, (ADA, ETH))
  , (ADAEUR, (ADA, EUR))
  , (ADAGBP, (ADA, GBP))
  , (ADAJPY, (ADA, JPY))
  , (ADAUSD, (ADA, USD))
  , (ETHBTC, (ETH, BTC))
  , (ETHEUR, (ETH, EUR))
  , (ETHGBP, (ETH, GBP))
  , (ETHJPY, (ETH, JPY))
  , (ETHUSD, (ETH, USD))
  ]


data OracleEnv =
  OracleEnv
  {
    nyfrb :: ClientEnv
  , coinGecko :: ClientEnv
  }


makeOracle
  :: Manager
  -> IO OracleEnv
makeOracle manager =
  OracleEnv
    <$> nyfrbEnv manager
    <*> coinGeckoEnv manager


readOracle
  :: EventBackend IO r DynamicEventSelector
  -> OracleEnv
  -> Oracle
  -> IO (Either String Integer)
readOracle eventBackend OracleEnv{..} symbol =
 do
  withEvent eventBackend (DynamicEventSelector "Oracle")
    $ \event ->
      do
        addField event $ ("symbol" :: Text) ≔ show symbol
        value <-
          case symbol of
            SOFR -> do
                      addField event $ ("source" :: Text) ≔ ("NYFRB" :: String)
                      addField event $ ("unit" :: Text) ≔ ("basis points" :: String)
                      fetchSofrBasisPoints nyfrb
            _    -> do
                      result <- uncurry (fetchCurrencyPair coinGecko) . fromJust $ symbol `lookup` pairs
                      addField event $ ("source" :: Text) ≔ ("CoinGecko" :: String)
                      addField event $ ("result":: Text)  ≔ show result
                      addField event $ ("unit" :: Text) ≔ ("/ 100,000,000" :: String)
                      pure $ rate <$> result
        addField event
          $ either (("failure":: Text)  ≔) (("value":: Text)  ≔) value
        pure value
