{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ViewPatterns #-}

module Contrib.Data.Time.Units.Aeson where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON))
import Data.Aeson qualified as A
import Data.Char qualified as Char
import Data.Text qualified as Text
import Data.Time.Units (TimeUnit (..))
import Data.Time.Units qualified as Time.Units
import GHC.Generics (Generic)
import Text.Read (readMaybe)

newtype Day = Day {toDay :: Time.Units.Day}
  deriving stock (Eq, Generic, Show)

instance FromJSON Day where
  parseJSON json = do
    s <- parseJSON json
    pure $ Day . Time.Units.fromMicroseconds $ s * 24 * 60 * 60 * 1_000_000

instance ToJSON Day where
  toJSON (Day s) = do
    let micro = Time.Units.toMicroseconds s
    toJSON (micro `div` (24 * 60 * 60 * 1_000_000))

instance TimeUnit Day where
  toMicroseconds = Time.Units.toMicroseconds . toDay
  fromMicroseconds = Day . Time.Units.fromMicroseconds

newtype Hour = Hour {toHour :: Time.Units.Hour}
  deriving stock (Eq, Generic, Show)

instance FromJSON Hour where
  parseJSON json = do
    s <- parseJSON json
    pure $ Hour . Time.Units.fromMicroseconds $ s * 60 * 60 * 1_000_000

instance ToJSON Hour where
  toJSON (Hour s) = do
    let micro = Time.Units.toMicroseconds s
    toJSON (micro `div` (60 * 60 * 1_000_000))

instance TimeUnit Hour where
  toMicroseconds = Time.Units.toMicroseconds . toHour
  fromMicroseconds = Hour . Time.Units.fromMicroseconds

newtype Minute = Minute {toMinute :: Time.Units.Minute}
  deriving stock (Eq, Generic, Show)

instance FromJSON Minute where
  parseJSON json = do
    s <- parseJSON json
    pure $ Minute . Time.Units.fromMicroseconds $ s * 60 * 1_000_000

instance ToJSON Minute where
  toJSON (Minute s) = do
    let micro = Time.Units.toMicroseconds s
    toJSON (micro `div` (60 * 1_000_000))

instance TimeUnit Minute where
  toMicroseconds = Time.Units.toMicroseconds . toMinute
  fromMicroseconds = Minute . Time.Units.fromMicroseconds

newtype Second = Second {toSecond :: Time.Units.Second}
  deriving stock (Eq, Generic, Show)

instance FromJSON Second where
  parseJSON json = do
    s <- parseJSON json
    pure $ Second . Time.Units.fromMicroseconds $ s * 1_000_000

instance ToJSON Second where
  toJSON (Second s) = do
    let micro = Time.Units.toMicroseconds s
    toJSON (micro `div` 1_000_000)

instance TimeUnit Second where
  toMicroseconds = Time.Units.toMicroseconds . toSecond
  fromMicroseconds = Second . Time.Units.fromMicroseconds

newtype Millisecond = Millisecond {toMillisecond :: Time.Units.Millisecond}
  deriving stock (Eq, Generic, Show)

instance FromJSON Millisecond where
  parseJSON json = do
    s <- parseJSON json
    pure $ Millisecond . Time.Units.fromMicroseconds $ s * 1_000

instance ToJSON Millisecond where
  toJSON (Millisecond s) = do
    let micro = Time.Units.toMicroseconds s
    toJSON (micro `div` 1_000)

instance TimeUnit Millisecond where
  toMicroseconds = Time.Units.toMicroseconds . toMillisecond
  fromMicroseconds = Millisecond . Time.Units.fromMicroseconds

newtype Microsecond = Microsecond {toMicrosecond :: Time.Units.Microsecond}
  deriving stock (Eq, Generic, Show)

instance FromJSON Microsecond where
  parseJSON json = do
    s <- parseJSON json
    pure $ Microsecond . Time.Units.fromMicroseconds $ s

instance ToJSON Microsecond where
  toJSON (Microsecond s) = do
    toJSON $ Time.Units.toMicroseconds s

instance TimeUnit Microsecond where
  toMicroseconds = Time.Units.toMicroseconds . toMicrosecond
  fromMicroseconds = Microsecond . Time.Units.fromMicroseconds

newtype Duration = Duration {durationMicroseconds :: Time.Units.Microsecond}
  deriving stock (Eq, Generic, Show)

instance Semigroup Duration where
  Duration a <> Duration b = Duration $ a + b

instance Monoid Duration where
  mempty = Duration 0

instance TimeUnit Duration where
  toMicroseconds = toMicroseconds . durationMicroseconds
  fromMicroseconds = Duration . fromMicroseconds

-- Based on the prefix (""|+|-) and suffix "us", "ms" "s", "m", "h", "d"
-- we parse out the duration - for example "+1d" is 1 day, "-1h" is negative duration 1 hour,
instance FromJSON Duration where
  parseJSON json = do
    A.String (Text.unpack -> str) <- parseJSON json
    let (prefix, suffix) = do
          case str of
            ('+' : rest) -> span Char.isDigit rest
            ('-' : rest) -> ('-' : takeWhile Char.isDigit rest, dropWhile Char.isDigit rest)
            _ -> span Char.isDigit str

    n <- case readMaybe prefix of
      Nothing -> fail $ "Invalid duration: " <> str
      Just n -> pure n

    case suffix of
      "us" -> do
        pure $ Duration $ fromMicroseconds n
      "ms" -> do
        pure $ Duration $ fromMicroseconds $ 1_000 * n
      "s" -> do
        pure $ Duration $ fromMicroseconds $ 1_000_000 * n
      "m" -> do
        pure $ Duration $ fromMicroseconds $ 60 * 1_000_000 * n
      "h" -> do
        pure $ Duration $ fromMicroseconds $ 60 * 60 * 1_000_000 * n
      "d" -> do
        pure $ Duration $ fromMicroseconds $ 24 * 60 * 60 * 1_000_000 * n
      _ -> fail $ "Invalid duration: " <> str
