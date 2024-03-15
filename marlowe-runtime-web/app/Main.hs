{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Colog (LogAction (LogAction), cmap, fmtMessage, logTextStdout)
import Colog.Message (Message)
import Control.Concurrent.Component (runComponent_)
import Control.Concurrent.Component.Run (runAppMTraced)
import Control.Monad.Reader (ask)
import Control.Monad.Trans.Marlowe (MarloweT (..))
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import Language.Marlowe.Runtime.Client (connectToMarloweRuntimeTraced)
import Language.Marlowe.Runtime.Web.Server
import Language.Marlowe.Runtime.Web.Server.Logging (renderServerSelectorOTel)
import Network.Wai.Handler.Warp (
  run,
 )
import Observe.Event (injectSelector)
import OpenTelemetry.Trace
import Options
import Paths_marlowe_runtime_web (version)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import UnliftIO (MonadUnliftIO, newMVar, withMVar)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  Options{..} <- getOptions
  runAppMTraced instrumentationLibrary (renderServerSelectorOTel port) do
    dependencies <- connectToMarloweRuntimeTraced (injectSelector RuntimeClient) runtimeHost runtimePort $ MarloweT do
      connector <- ask
      pure
        ServerDependencies
          { openAPIEnabled
          , accessControlAllowOriginAll
          , runApplication = run $ fromIntegral port
          , connector
          }
    runComponent_ server dependencies
  where
    instrumentationLibrary =
      InstrumentationLibrary
        { libraryName = "marlowe-web-server"
        , libraryVersion = fromString $ showVersion version
        }

concurrentLogger :: (MonadUnliftIO m) => IO (LogAction m Message)
concurrentLogger = do
  lock <- newMVar ()
  let LogAction baseAction = cmap fmtMessage logTextStdout
  pure $ LogAction $ withMVar lock . const . baseAction
