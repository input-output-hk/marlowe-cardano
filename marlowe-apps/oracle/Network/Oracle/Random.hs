

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}


module Network.Oracle.Random
  ( RandomApi
  , fetchRandom
  , getRandom
  , randomEnv
  , randomApi
  ) where


import Data.Proxy (Proxy(Proxy))
import Data.Text (Text, unpack)
import Text.Read (readMaybe)
import Network.HTTP.Client (Manager)
import Servant.API (Get, PlainText, (:>), QueryParam', Required)
import Servant.Client (ClientEnv, ClientM, client, mkClientEnv, parseBaseUrl, runClientM)


randomEnv :: Manager -> IO ClientEnv
randomEnv manager = mkClientEnv manager <$> parseBaseUrl "https://www.random.org"


type RandomApi =
  "integers"
    :> QueryParam' '[Required] "format" Text
    :> QueryParam' '[Required] "col" Integer
    :> QueryParam' '[Required] "rnd" Text
    :> QueryParam' '[Required] "base" Integer
    :> QueryParam' '[Required] "num" Integer
    :> QueryParam' '[Required] "min" Integer
    :> QueryParam' '[Required] "max" Integer
    :> Get '[PlainText] Text


randomApi :: Proxy RandomApi
randomApi = Proxy


getRandom
  :: Integer
  -> Integer
  -> ClientM (Either String Integer)
getRandom min' max' =
  maybe (Left "Failed parsing integer.") Right
    . readMaybe
    . unpack
    <$> client randomApi "plain" 1 "new" 10 1 min' max'


fetchRandom
  :: Integer
  -> Integer
  -> ClientEnv
  -> IO (Either String Integer)
fetchRandom min' max' = fmap (either (Left . show) id) . runClientM (getRandom min' max')
