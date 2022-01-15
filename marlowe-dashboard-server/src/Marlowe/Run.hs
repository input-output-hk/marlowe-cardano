{-# LANGUAGE DeriveGeneric #-}

module Marlowe.Run where

import Cardano.Prelude hiding (Handler)
import qualified Data.Text as Text
import Data.Version (showVersion)
import qualified Paths_marlowe_dashboard_server as Package.Paths

getVersion :: Applicative m => m Text
getVersion = pure $ Text.pack $ showVersion Package.Paths.version
