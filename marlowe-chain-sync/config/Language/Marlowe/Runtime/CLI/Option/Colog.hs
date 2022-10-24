{-# LANGUAGE DataKinds #-}
module Language.Marlowe.Runtime.CLI.Option.Colog
  where

import qualified Colog
import Control.Arrow ((>>>))
import Control.Monad.IO.Class (MonadIO)
import Data.Char (toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import qualified Language.Marlowe.Runtime.Logging.Colog as Logging
import Options.Applicative

data Verbosity = LogLevel Colog.Severity | Silent
  deriving (Eq, Show)

verbosityParser :: Verbosity -> Parser Verbosity
verbosityParser defaultVerbosity = fromMaybe defaultVerbosity <$> (fmap LogLevel <$> optional logLevel <|> optional silent)
  where
    silent :: Parser Verbosity
    silent = flag' Silent $ mconcat
      [ long "silent"
      , help "Suppress all logs."
      ]

    -- Parses log severity expressed using: Debug, Info, Normal, Warning, General, Error
    logLevel :: Parser Colog.Severity
    logLevel = do
      let
        capitalize :: String -> String
        capitalize = map toUpper
        severities =
          [ ("DEBUG", Colog.Debug)
          , ("INFO", Colog.Info)
          , ("WARNING", Colog.Warning)
          , ("ERROR", Colog.Error)
          ]
        spec = "[" <> (intercalate "|" . map (capitalize. fst) $ severities) <> "]"

        severityParser :: ReadM Colog.Severity
        severityParser = eitherReader $ capitalize >>> flip lookup severities >>> \case
          Just sev -> Right sev
          Nothing -> Left $ "Invalid log level. Expecting value:  " <> spec <> "."

      option severityParser $ mconcat
        [ long "log-level"
        , metavar "LOG_LEVEL"
        , help $ "Log everything up including the given level: " <> spec
        ]

logActionParser :: MonadIO m => Verbosity -> Parser (Colog.LogAction m Colog.Message)
logActionParser verbosity = mkLogger <$> verbosityParser verbosity

mkLogger :: MonadIO m => Verbosity -> Colog.LogAction m Colog.Message
mkLogger Silent = Logging.mkLogger Nothing
mkLogger (LogLevel level) = Logging.mkLogger (Just level)


