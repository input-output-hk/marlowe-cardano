module Language.Marlowe.Runtime.API.Command.Rm
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

newtype RmCommand = RmCommand { contractIds :: [ContractId] }
  deriving (Show, Eq)

rmCommandParser :: ParserInfo RmCommand
rmCommandParser = info parser $ progDesc "Stop managing a contract"
  where
    parser = RmCommand <$> many (contractIdArgument "A contract ID to remove (i.e. stop managing)")

runRmCommand :: RmCommand -> CLI ()
runRmCommand RmCommand{..} = runCLIExceptT $ for_ contractIds \contractId -> do
  removed <- ExceptT $ runHistoryCommand $ StopFollowingContract contractId
  when removed do
    liftIO $ T.putStrLn $ renderContractId contractId
