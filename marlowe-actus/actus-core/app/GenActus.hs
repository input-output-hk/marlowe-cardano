{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module GenActus
  where

import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString.Lazy as B (readFile)
import Data.Map (Map, elems, toList)
import Data.String (IsString(..))
import GHC (runGhc)
import GHC.Generics (Generic)
import GHC.Paths (libdir)
import GHC.SourceGen
import System.Environment

main :: IO ()
main = do
  p <- getEnv "ACTUS_DICTIONARY_DIR"
  Right d <- load (p <> "/actus-dictionary.json")
  runGhc (Just libdir) $ putPpr (actusModule d)

data ContractType = ContractType
  { acronym :: String
  , name :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Terms = Terms
  { group :: String
  , identifier :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Version = ActusVersion
  { version' :: String,
    date :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ActusDictionary = ActusDictionary
  { taxonomy :: Map String ContractType,
    terms :: Map String Terms
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

load :: FilePath -> IO (Either String ActusDictionary)
load f = eitherDecode <$> B.readFile f

actusModule :: ActusDictionary -> HsModule'
actusModule ActusDictionary {..} =
  module'
    (Just "Domain")
    Nothing
    []
    [ data'
        "CT"
        []
        (map f (elems taxonomy))
        []
    ]
  where
    f ContractType {..} = prefixCon (fromString acronym) []
