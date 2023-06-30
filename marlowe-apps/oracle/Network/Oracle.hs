{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Oracle (
  Oracle (..),
  OracleEnv,
  makeOracle,
  oracles,
  readOracle,
  toOracleSymbol,
) where

import Control.Exception (IOException, catch)
import Data.Aeson (encode)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Language.Marlowe.Core.V1.Semantics.Types (Bound (..))
import Language.Marlowe.Oracle.Types (OracleRequest (..), choiceName')
import Network.HTTP.Client (Manager)
import Network.Oracle.CoinGecko (Currency (..), CurrencyPair (..), coinGeckoEnv, fetchCurrencyPair)
import Network.Oracle.Random (fetchRandom, randomEnv)
import Network.Oracle.Sofr (fetchSofrBasisPoints, nyfrbEnv)
import Observe.Event.Dynamic (DynamicEventSelector (..))
import Observe.Event.Explicit (EventBackend, addField, withEvent)
import Observe.Event.Syntax ((≔))
import Servant.Client (ClientEnv)
import System.Process (readProcess)
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy.Char8 as LBS8 (unpack)

data Oracle
  = SOFR
  | RANDOM
  | BTCETH
  | BTCEUR
  | BTCGBP
  | BTCJPY
  | BTCUSD
  | ADABTC
  | ADAETH
  | ADAEUR
  | ADAGBP
  | ADAJPY
  | ADAUSD
  | ETHBTC
  | ETHEUR
  | ETHGBP
  | ETHJPY
  | ETHUSD
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

toOracleSymbol :: String -> Maybe Oracle
toOracleSymbol = readMaybe

oracles :: [Oracle]
oracles = [minBound .. maxBound]

pairs :: [(Oracle, (Currency, Currency))]
pairs =
  [ (BTCETH, (BTC, ETH))
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

data OracleEnv = OracleEnv
  { nyfrb :: ClientEnv
  , coinGecko :: ClientEnv
  , random :: ClientEnv
  }

makeOracle
  :: Manager
  -> IO OracleEnv
makeOracle manager =
  OracleEnv
    <$> nyfrbEnv manager
    <*> coinGeckoEnv manager
    <*> randomEnv manager

readOracle
  :: EventBackend IO r DynamicEventSelector
  -> Either String OracleEnv
  -> OracleRequest
  -> IO (Either String Integer)
readOracle eventBackend oracleEnv oracleRequest@OracleRequest{..} =
  do
    withEvent eventBackend (DynamicEventSelector "Oracle") $
      \event ->
        do
          let symbol = choiceName' oracleRequest
          addField event $ ("symbol" :: Text) ≔ symbol
          value <-
            case (oracleEnv, toOracleSymbol symbol) of
              (Right OracleEnv{nyfrb}, Just SOFR) ->
                do
                  addField event $ ("source" :: Text) ≔ ("NYFRB" :: String)
                  addField event $ ("unit" :: Text) ≔ ("basis points" :: String)
                  fetchSofrBasisPoints nyfrb
              (Right OracleEnv{random}, Just RANDOM) ->
                case bounds of
                  [Bound min' max'] ->
                    do
                      addField event $ ("source" :: Text) ≔ ("Random.Org" :: String)
                      addField event $ ("min" :: Text) ≔ min'
                      addField event $ ("max" :: Text) ≔ max'
                      fetchRandom min' max' random
                  _ -> pure . Left $ "Illegal multiple bounds: " <> show bounds <> "."
              (Right OracleEnv{coinGecko}, Just symbol') ->
                do
                  result <- uncurry (fetchCurrencyPair coinGecko) . fromJust $ symbol' `lookup` pairs
                  addField event $ ("source" :: Text) ≔ ("CoinGecko" :: String)
                  addField event $ ("result" :: Text) ≔ show result
                  addField event $ ("unit" :: Text) ≔ ("/ 100,000,000" :: String)
                  pure $ rate <$> result
              (Right _, Nothing) -> pure . Left $ "Unknown oracle symbol: " <> symbol <> "."
              (Left command, _) ->
                ( do
                    result <- readProcess command [] . LBS8.unpack $ encode oracleRequest
                    case readMaybe result of
                      Just result' -> pure $ pure result'
                      Nothing -> pure . Left $ "Illegal result: " <> result <> "."
                )
                  `catch` (\e -> pure . Left $ show (e :: IOException))
          addField event $
            either (("failure" :: Text) ≔) (("value" :: Text) ≔) value
          pure value
