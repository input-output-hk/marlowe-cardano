{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Main
  where

import Flags
import Network.Wai.Handler.Warp (run)
import Servant
import qualified Server

main :: IO ()
main = run 8080 $ app Enabled

app :: Flag openAPIFlag -> Application
app Enabled = serve (Server.api Enabled) $ Server.server Enabled
app Disabled = serve (Server.api Disabled) $ Server.server Disabled
