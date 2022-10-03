module Language.Marlowe.Runtime.CLI.Command.Rm
  where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import qualified Data.Text.IO as T
import Language.Marlowe.Runtime.CLI.Monad (CLI, runCLIExceptT, runHistoryCommand)
import Language.Marlowe.Runtime.CLI.Option (contractIdArgument)
import Language.Marlowe.Runtime.Core.Api (ContractId, renderContractId)
import Language.Marlowe.Runtime.History.Api (HistoryCommand(..))
import Options.Applicative

newtype RmCommand = RmCommand { contractId :: ContractId }
  deriving (Show, Eq)

rmCommandParser :: ParserInfo RmCommand
rmCommandParser = info parser $ progDesc "Stop managing a contract"
  where
    parser = RmCommand <$> contractIdArgument "The ID of the contract to remove"

runRmCommand :: RmCommand -> CLI ()
runRmCommand RmCommand{..} = runCLIExceptT do
  removed <- ExceptT $ runHistoryCommand $ StopFollowingContract contractId
  when removed do
    liftIO $ T.putStrLn $ renderContractId contractId
