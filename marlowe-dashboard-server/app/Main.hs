{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main,
  )
where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger (MonadLogger, logInfoN, runStderrLoggingT)
import qualified Data.Text as Text
import Network.Wai.Handler.Warp (HostPreference, defaultSettings, setHost, setPort)
import Options.Applicative (CommandFields, Mod, Parser, auto, command, customExecParser, disambiguate, fullDesc, help,
                            helper, idm, info, long, option, prefs, short, showDefault, showHelpOnEmpty,
                            showHelpOnError, strOption, subparser, value)
import qualified Webserver

-- | You might wonder why we don't stick everything in `Config`. The
-- answer is that pushing certain flags to the command line makes
-- automated deployment easier.
--
-- You might also wonder why we don't stick everything on the command
-- line. The answer is for flags that rarely change, putting them in a
-- config file makes development easier.
data Command
  = Run
      { _host   :: !HostPreference,
        _port   :: !Int,
        _config :: !FilePath
      }
  deriving (Show, Eq)

commandParser :: Parser Command
commandParser = subparser webserverCommandParser

webserverCommandParser :: Mod CommandFields Command
webserverCommandParser =
  command "webserver" $
    flip info fullDesc $ do
      _host <-
        strOption
          ( short 'b' <> long "bind" <> help "Webserver bind address"
              <> showDefault
              <> value "127.0.0.1"
          )
      _port <-
        option
          auto
          ( short 'p' <> long "port" <> help "Webserver port number"
              <> showDefault
              <> value 8080
          )
      _config <-
        strOption
          ( short 'c' <> long "config" <> help "Location of the configuration file"
          )
      pure Run {..}

runCommand :: (MonadIO m, MonadLogger m) => Command -> m ()
runCommand Run {..} = liftIO $ Webserver.run _config settings
  where
    settings = setHost _host . setPort _port $ defaultSettings

main :: IO ()
main = do
  options <-
    customExecParser
      (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
      (info (helper <*> commandParser) idm)
  runStderrLoggingT $ do
    logInfoN $ "Running: " <> Text.pack (show options)
    runCommand options
