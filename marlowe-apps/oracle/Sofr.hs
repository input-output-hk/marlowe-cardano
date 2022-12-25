

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}


module Sofr
  ( ReferenceRate(..)
  , ReferenceRates(..)
  , SofrApi
  , fetchSofrBasisPoints
  , getReferenceRates
  , getSofrBasisPoints
  , nyfrbEnv
  , sofrApi
  ) where


import Data.Aeson (FromJSON(parseJSON), withObject, (.:))
import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (Get, JSON, (:>))
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)



newtype ReferenceRates = ReferenceRates {referenceRates :: [ReferenceRate]}
  deriving (Eq, Ord, Read, Show)

instance FromJSON ReferenceRates where
  parseJSON = withObject "ReferenceRates" $ fmap ReferenceRates . (.: "refRates")


nyfrbEnv :: IO ClientEnv
nyfrbEnv =
  mkClientEnv
    <$> newTlsManager
    <*> parseBaseUrl "https://markets.newyorkfed.org"


type SofrApi = "api/rates/secured/sofr/last/1.json" :> Get '[JSON] ReferenceRates


sofrApi :: Proxy SofrApi
sofrApi = Proxy


data ReferenceRate =
  ReferenceRate
  {
    effectiveDate :: Text
  , rateType :: Text
  , percentRate :: Double
  , percentPercentile1 :: Double
  , percentPercentile25 :: Double
  , percentPercentile75 :: Double
  , percentPercentile99 :: Double
  , volumeInBillions :: Double
  , revisionIndicator :: Text
  }
    deriving (Eq, Ord, Read, Show)

instance FromJSON ReferenceRate where
  parseJSON =
    withObject "ReferenceRate"
      $ \o ->
        do
          effectiveDate <- o .: "effectiveDate"
          rateType <- o .: "type"
          percentRate <- o .: "percentRate"
          percentPercentile1 <- o .: "percentPercentile1"
          percentPercentile25 <- o .: "percentPercentile25"
          percentPercentile75 <- o .: "percentPercentile75"
          percentPercentile99 <- o .: "percentPercentile99"
          volumeInBillions <- o .: "volumeInBillions"
          revisionIndicator <- o .: "revisionIndicator"
          pure ReferenceRate{..}


getReferenceRates :: ClientM [ReferenceRate]
getReferenceRates = referenceRates <$> client sofrApi


getSofrBasisPoints :: ClientM (Either String Integer)
getSofrBasisPoints =
  do
    rates <- getReferenceRates
    case filter ((== "SOFR") . rateType) rates of
      [ReferenceRate{..}] -> pure . Right . round $ 100 * percentRate
      []                  -> pure $ Left "No SOFR rate entry in NYFRB data feed."
      _                   -> pure $ Left "Multiple SOFR rate entries in NYFRB data feed."


fetchSofrBasisPoints :: ClientEnv -> IO (Either String Integer)
fetchSofrBasisPoints env =
  runClientM getSofrBasisPoints env
    >>= \case
      Right (Right rate)   -> pure $ Right rate
      Right (Left message) -> pure $ Left message
      Left message         -> pure . Left $ show message
