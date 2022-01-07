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

import           SetupParser                      (AppOpts (..), NoConfigCommand (..), parseOptions)

import qualified Cardano.BM.Configuration.Model   as CM
import           Control.Monad.Logger             (logErrorN, runStdoutLoggingT)
import           Data.Proxy                       (Proxy (..))
import           Data.Text.Extras                 (tshow)
import           MarloweContract                  (MarloweContract)
import qualified Plutus.PAB.Monitoring.Monitoring as LM
import qualified Plutus.PAB.Run.PSGenerator       as PSGenerator
import           Plutus.PAB.Types                 (PABError)

import           System.Exit                      (ExitCode (ExitFailure), exitSuccess, exitWith)

runNoConfigCommand ::
    NoConfigCommand
    -> IO ()
runNoConfigCommand = \case

    -- Generate PureScript bridge code
    PSGenerator {psGenOutputDir} -> do
        PSGenerator.generateDefault psGenOutputDir

    -- Generate PureScript API code
    PSApiGenerator {psGenOutputDir} -> do
        PSGenerator.generateAPIModule (Proxy :: Proxy MarloweContract) psGenOutputDir
        PSGenerator.generateWith @MarloweContract psGenOutputDir

    -- Get default logging configuration
    WriteDefaultConfig{outputFile} -> LM.defaultConfig >>= flip CM.exportConfiguration outputFile

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
