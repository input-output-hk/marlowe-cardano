module Language.Marlowe.Runtime.Transaction.Submit
  where

import Cardano.Api (ScriptDataSupportedInEra, Tx)
import Control.Concurrent.STM (STM, atomically, newTVar, readTVar, writeTVar)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader)
import Language.Marlowe.Runtime.Transaction.Api (SubmitError, SubmitStatus(..))

data SubmitJobStatus
  = Running SubmitStatus
  | Succeeded BlockHeader
  | Failed SubmitError

data SubmitJob = SubmitJob
  { submitJobStatus :: STM SubmitJobStatus
  , runSubmitJob :: IO ()
  }

mkSubmitJob :: ScriptDataSupportedInEra era -> Tx era -> STM SubmitJob
mkSubmitJob era tx = do
  statusVar <- newTVar $ Running Submitting
  pure $ SubmitJob (readTVar statusVar) $ doSubmit era tx $ atomically . writeTVar statusVar

doSubmit :: ScriptDataSupportedInEra era -> Tx era -> (SubmitJobStatus -> IO ()) -> IO ()
doSubmit _tellStatus = error "not implemented"
