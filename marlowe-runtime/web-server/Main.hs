{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Main
  where

import Control.Monad.Reader (runReaderT)
import Flags
import Monad (AppEnv(..), AppM(..))
import Network.Wai.Handler.Warp (run)
import Servant
import qualified Server

main :: IO ()
main = do
  let _loadContractHeaders _ = pure $ Just []
  run 8080 $ app AppEnv{..} Enabled

app :: AppEnv -> Flag openAPIFlag -> Application
app env Enabled = serve api $ hoistServer api (flip runReaderT env . runAppM) $ Server.server Enabled
  where
    api = Server.api Enabled
app env Disabled = serve api $ hoistServer api (flip runReaderT env . runAppM) $ Server.server Disabled
  where
    api = Server.api Disabled
