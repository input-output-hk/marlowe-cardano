{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
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
  path <- getEnv "ACTUS_DICTIONARY_DIR"
  actusDictionary <- load (path <> "/actus-dictionary.json")
  runGhc (Just libdir) $ putPpr (actusModule $ either error id actusDictionary)

data ContractType = ContractType
  { identifier :: String
  , name :: String
  , acronym :: String
  , family :: String
  , class' :: String
  , description :: String
  , coverage :: Maybe String
  , status :: Maybe String
  }
  deriving Show

instance FromJSON ContractType where
  parseJSON (Object v) =
    ContractType
      <$> v .: "identifier"
      <*> v .: "name"
      <*> v .: "acronym"
      <*> v .: "family"
      <*> v .: "class"
      <*> v .: "description"
      <*> v .:? "coverage"
      <*> v .:? "status"
  parseJSON _ = mzero

data Term = Term
  { identifier :: String
  , group :: Maybe String
  , name :: String
  , acronym :: String
  , type' :: String
  -- TODO: allowed values depends on type
  -- , allowedValues :: [AllowedValue]
  , default' :: String
  , description :: Maybe String
  }
  deriving Show

instance FromJSON Term where
  parseJSON (Object v) =
    Term
      <$> v .: "identifier"
      <*> v .: "group"
      <*> v .: "name"
      <*> v .: "acronym"
      <*> v .: "type"
   --   <*> v .: "allowedValues"
      <*> v .: "default"
      <*> v .:? "description"
  parseJSON _ = mzero

data AllowedValue = AllowedValue
  { option :: String
  , identifier :: String
  , name :: String
  , acronym :: String
  , description :: String
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

data ActusDictionary = ActusDictionary
  { taxonomy :: Map String ContractType,
    terms :: Map String Term
  }
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON)

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
        [deriving' [var "Show", var "Read", var "Eq", var "Generic", var "FromJSON"]]
    ]
  where
    f ContractType {..} = prefixCon (fromString acronym) []
