{-# LANGUAGE ApplicativeDo         #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE ImportQualifiedPost   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Main
    ( main
    ) where

import SetupParser (AppOpts (..), NoConfigCommand (..), parseOptions)

import qualified Cardano.BM.Configuration.Model as CM
import Control.Monad.Logger (logErrorN, runStdoutLoggingT)
import Data.Text.Extras (tshow)
import qualified Plutus.PAB.Monitoring.Monitoring as LM
import Plutus.PAB.Types (PABError)

import System.Exit (ExitCode (ExitFailure), exitSuccess, exitWith)

-- Get default logging configuration
runNoConfigCommand ::
    NoConfigCommand
    -> IO ()
runNoConfigCommand WriteDefaultConfig{outputFile} =
    LM.defaultConfig >>= flip CM.exportConfiguration outputFile

main :: IO ()
main = do
    AppOpts { cmd } <- parseOptions

    -- execute parsed pab command and handle errors on failure
    result <- Right <$> runNoConfigCommand cmd
    either handleError (const exitSuccess) result

    where
        handleError (err :: PABError) = do
            runStdoutLoggingT $ (logErrorN . tshow) err
            exitWith (ExitFailure 1)
