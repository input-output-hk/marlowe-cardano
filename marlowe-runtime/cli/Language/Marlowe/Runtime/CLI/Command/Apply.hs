{-# LANGUAGE ExistentialQuantification #-}

module Language.Marlowe.Runtime.CLI.Command.Apply
  where

import Language.Marlowe (POSIXTime)
import Language.Marlowe.Runtime.CLI.Command.Tx (TxCommand)
import Language.Marlowe.Runtime.CLI.Monad (CLI)
import Language.Marlowe.Runtime.Core.Api (ContractId, IsMarloweVersion(Redeemer), MarloweVersion)
import Options.Applicative

data ApplyCommand = forall v. ApplyCommand
  { contractId :: ContractId
  , marloweVersion :: MarloweVersion v
  , inputs :: ContractInputs v
  , validityLowerBound :: Maybe POSIXTime
  , validityUpperBound :: Maybe POSIXTime
  }

data ContractInputs v
  = ContractInputsByFile FilePath
  | ContractInputsByValue (Redeemer v)

applyCommandParser :: ParserInfo (TxCommand ApplyCommand)
applyCommandParser = info empty $ progDesc "Apply inputs to a contract"

depositCommandParser :: ParserInfo (TxCommand ApplyCommand)
depositCommandParser = info empty $ progDesc "Deposit funds into a contract"

chooseCommandParser :: ParserInfo (TxCommand ApplyCommand)
chooseCommandParser = info empty $ progDesc "Make a choice in a contract"

notifyCommandParser :: ParserInfo (TxCommand ApplyCommand)
notifyCommandParser = info empty $ progDesc "Notify a contract to proceed"

runApplyCommand :: TxCommand ApplyCommand -> CLI ()
runApplyCommand = error "not implemented"
