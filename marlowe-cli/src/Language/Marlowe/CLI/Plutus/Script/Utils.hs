{-# LANGUAGE GADTs #-}

module Language.Marlowe.CLI.Plutus.Script.Utils where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.Marlowe.Scripts (marloweValidatorCompiled)
import PlutusTx qualified
import Prettyprinter (pretty)

printPir :: (MonadIO m) => Maybe FilePath -> m ()
printPir outFile =
  liftIO
    . maybe print ((. show) . writeFile) outFile
    . pretty
    $ PlutusTx.getPir marloweValidatorCompiled

printUplc :: (MonadIO m) => Maybe FilePath -> m ()
printUplc outFile =
  liftIO
    . maybe print ((. show) . writeFile) outFile
    . pretty
    $ PlutusTx.getPlc marloweValidatorCompiled
