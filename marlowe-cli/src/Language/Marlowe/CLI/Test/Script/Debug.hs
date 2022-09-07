{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Language.Marlowe.CLI.Test.Script.Debug where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Language.Marlowe.CLI.Test.Types (ScriptOperation (..))
import Language.Marlowe.CLI.Types (CliError (CliError))
import Ledger.Orphans ()

data SoFormat = SoName | SoShow

printTraceMsg :: String -> String -> String
printTraceMsg loc msg = "[" <> loc <> "]" <> " " <> msg

logTraceMsg :: MonadIO m => String -> String -> m ()
logTraceMsg loc msg = liftIO . putStrLn $ printTraceMsg loc msg

printSo :: SoFormat -> ScriptOperation -> String
printSo SoShow po = show po
printSo SoName po = printName po
  where
    printName AutoRun {}       = "AutoRun"
    printName CreateWallet {}  = "CreateWallet"
    printName FundWallet {}    = "FundWallet"
    printName Mint {}          = "Mint"
    printName Initialize {}    = "Initialize"
    printName Prepare {}       = "Prepare"
    printName Fail {}          = "Fail"
    printName Publish {}       = "Publish"
    printName FindPublished {} = "FindPublished"

printSoMsg :: SoFormat -> ScriptOperation -> String -> String
printSoMsg format po = printTraceMsg (printSo format po)

logSoMsg :: MonadIO m => SoFormat -> ScriptOperation -> String -> m ()
logSoMsg frmt po = logTraceMsg (printSo frmt po)

throwCliPoError :: MonadError CliError m => ScriptOperation -> String -> m a
throwCliPoError po msg = throwError
    $ CliError
    $ printSoMsg SoName po msg
