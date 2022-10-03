module Language.Marlowe.Runtime.CLI.Command.Ls
  where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import Data.Void (absurd)
import Language.Marlowe.Runtime.CLI.Monad (CLI, runHistoryQueryClient)
import Language.Marlowe.Runtime.Core.Api (renderContractId)
import Language.Marlowe.Runtime.History.Api (FollowerStatus(..), HistoryQuery(..))
import Network.Protocol.Query.Client
import Options.Applicative

data LsStatusFlag
  = LsShowStatus
  | LsHideStatus
  deriving (Show, Eq)

data LsFailedFlag
  = LsShowFailed
  | LsHideFailed
  deriving (Show, Eq)

data LsCommand = LsCommand
  { statusFlag:: LsStatusFlag
  , failedFlag :: LsFailedFlag
  } deriving (Show, Eq)

lsCommandParser :: ParserInfo LsCommand
lsCommandParser = info parser $ progDesc "List managed contracts"
  where
    parser = LsCommand <$> statusFlagParser <*> failedFlagParser
    statusFlagParser = flag LsHideStatus LsShowStatus $ mconcat
      [ long "show-status"
      , short 's'
      , help "Show the status of the contract as well as its ID"
      ]
    failedFlagParser = flag LsHideFailed LsShowFailed $ mconcat
      [ long "show-failed"
      , short 'f'
      , help "Include contracts whose follower encountered an error"
      ]

runLsCommand :: LsCommand -> CLI ()
runLsCommand LsCommand{..} = runHistoryQueryClient
  $ QueryClient
  $ pure
  $ SendMsgRequest GetFollowedContracts ClientStNextCanReject
      { recvMsgReject = absurd
      , recvMsgNextPage = handleNextPage
      }
  where
    handleNextPage results nextPage = do
      void $ Map.traverseWithKey printResult $ Map.filter filterResult results
      pure $ maybe
        (SendMsgDone ())
        (flip SendMsgRequestNext $ ClientStNext handleNextPage)
        nextPage
    filterResult = case failedFlag of
      LsShowFailed -> const True
      LsHideFailed -> \case
        Failed _ -> False
        _        -> True
    printResult = case statusFlag of
      LsHideStatus -> const . printContractId
      LsShowStatus -> printStatus
    printContractId = liftIO . T.putStrLn . renderContractId
    printStatus contractId status = liftIO do
      T.putStr $ renderContractId contractId
      putStr " Status: "
      print status
