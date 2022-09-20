{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# LANGUAGE BlockArguments #-}

module Language.Marlowe.CLI.Test.Script.Debug
  where

import Control.Lens (view)
import Control.Monad.Error.Class (catchError)
import Control.Monad.Except (MonadError(throwError))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (ReaderT(runReaderT))
import Control.Monad.Reader.Class (MonadReader)
import Language.Marlowe.CLI.Test.Types (ScriptEnv, ScriptOperation(..), seEra)
import Language.Marlowe.CLI.Types (CliEnv(CliEnv), CliError(CliError))
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
    printName AutoRun {}      = "AutoRun"
    printName BurnAll {}      = "BurnAll"
    printName CreateWallet {} = "CreateWallet"
    printName FundWallet {}   = "FundWallet"
    printName Mint {}         = "Mint"
    printName Initialize {}   = "Initialize"
    printName Prepare {}      = "Prepare"
    printName Fail {}         = "Fail"
    printName Publish {}      = "Publish"
    printName SplitWallet {}  = "SplitWallet"

printSoMsg :: SoFormat -> ScriptOperation -> String -> String
printSoMsg format po = printTraceMsg (printSo format po)

logSoMsg :: MonadIO m => SoFormat -> ScriptOperation -> String -> m ()
logSoMsg frmt po = logTraceMsg (printSo frmt po)

logSoMsg' :: MonadIO m => ScriptOperation -> String -> m ()
logSoMsg' = logSoMsg SoName

throwSoError :: MonadError CliError m => ScriptOperation -> String -> m a
throwSoError po msg = throwError
    $ CliError
    $ printSoMsg SoName po msg

runSoCli :: MonadError CliError m
         => MonadReader (ScriptEnv era) m
         => ScriptOperation
         -> ReaderT (CliEnv era) m a
         -> m a
runSoCli so action = do
  era <- view seEra
  withCliErrorMsg (printSoMsg SoName so) $ runReaderT action (CliEnv era)


-- This helper is present in the newer version of mtl
withError :: MonadError e m => (e -> e) -> m a -> m a
withError modifyError action = catchError action \e -> do
                                throwError $ modifyError e

withCliErrorMsg :: MonadError CliError m => (String -> String) -> m a -> m a
withCliErrorMsg f = withError (\(CliError msg) -> CliError (f msg))


liftSoMaybe :: MonadError CliError m => ScriptOperation -> String -> Maybe a -> m a
liftSoMaybe _ _ (Just a) = pure a
liftSoMaybe so msg _ = throwSoError so msg
