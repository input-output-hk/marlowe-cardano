{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}

module Verbosity where

import Prelude hiding (Enum, fromEnum, pred, succ, toEnum)

import Colog.Core.Severity as Severity
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (unexpected)
import Data.Maybe (fromMaybe)
import GHC.Generics
import Prelude.SafeEnum (DownwardEnum, Enum, UpwardEnum, fromEnum, precedes, pred, succ, succeeds, toEnum)

data Verbosity
  = Silent
  | Normal
  | Verbose
  | Annoying
  deriving stock
    ( Bounded
    , Eq
    , Generic
    , Ord
    , Read
    , Show
    )

instance ToJSON Verbosity where
  toJSON = toJSON . fromMaybe 0 . fromEnum

instance FromJSON Verbosity where
  parseJSON a = do
    i <- parseJSON a
    case toEnum i of
      Just v  -> pure v
      Nothing -> unexpected a

instance UpwardEnum Verbosity where
  succ x = do
    i <- fromEnum x
    toEnum (i + 1)
  succeeds = (>)

instance DownwardEnum Verbosity where
  pred x = do
    i <- fromEnum x
    toEnum (i - 1)
  precedes = (<)

instance Enum Verbosity where
  toEnum 0 = Just Silent
  toEnum 1 = Just Normal
  toEnum 2 = Just Verbose
  toEnum 3 = Just Annoying
  toEnum _ = Nothing

  fromEnum Silent   = Just 0
  fromEnum Normal   = Just 1
  fromEnum Verbose  = Just 2
  fromEnum Annoying = Just 3

mkLogFilter :: Verbosity -> (Severity -> Bool)
mkLogFilter Silent   = const False
mkLogFilter Normal   = (> Severity.Info)
mkLogFilter Verbose  = (> Severity.Debug)
mkLogFilter Annoying = const True
