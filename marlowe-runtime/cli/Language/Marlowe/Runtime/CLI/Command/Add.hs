module Language.Marlowe.Runtime.CLI.Command.Add
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

newtype AddCommand = AddCommand { contractId :: ContractId }
  deriving (Show, Eq)

addCommandParser :: ParserInfo AddCommand
addCommandParser = info parser $ progDesc "Start managing a new contract"
  where
    parser = AddCommand <$> contractIdArgument "The ID of the contract to add"

runAddCommand :: AddCommand -> CLI ()
runAddCommand AddCommand{..} = runCLIExceptT do
  added <- ExceptT $ runHistoryCommand $ FollowContract contractId
  when added do
    liftIO $ T.putStrLn $ renderContractId contractId
