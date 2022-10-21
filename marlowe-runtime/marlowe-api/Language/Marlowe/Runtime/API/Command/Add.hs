module Language.Marlowe.Runtime.API.Command.Add
  where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(..))
import Data.Foldable (for_)
import qualified Data.Text.IO as T
import Language.Marlowe.Runtime.API.Monad (CLI, runCLIExceptT, runHistoryCommand)
import Language.Marlowe.Runtime.CLI.Option (contractIdArgument)
import Language.Marlowe.Runtime.Core.Api (ContractId, renderContractId)
import Language.Marlowe.Runtime.History.Api (HistoryCommand(..))
import Options.Applicative

newtype AddCommand = AddCommand { contractIds :: [ContractId] }
  deriving (Show, Eq)

addCommandParser :: ParserInfo AddCommand
addCommandParser = info parser $ progDesc "Start managing a new contract"
  where
    parser = AddCommand <$> many (contractIdArgument "A contract ID to add (i.e. start managing)")

runAddCommand :: AddCommand -> CLI ()
runAddCommand AddCommand{..} = runCLIExceptT $ for_ contractIds \contractId -> do
  added <- ExceptT $ runHistoryCommand $ FollowContract contractId
  when added do
    liftIO $ T.putStrLn $ renderContractId contractId
