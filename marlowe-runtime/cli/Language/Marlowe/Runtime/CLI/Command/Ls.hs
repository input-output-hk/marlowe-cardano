module Language.Marlowe.Runtime.CLI.Command.Ls
  where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.IO as T
import Data.Void (absurd)
import Language.Marlowe.Runtime.CLI.Monad (CLI, logAndDie, runDiscoveryQueryClient, runHistoryQueryClient)
import Language.Marlowe.Runtime.Core.Api (renderContractId)
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader(..), DiscoveryQuery(..))
import qualified Language.Marlowe.Runtime.Discovery.Api as Discovery
import Language.Marlowe.Runtime.History.Api (FollowerStatus(..), HistoryQuery(..))
import qualified Language.Marlowe.Runtime.History.Api as History
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
  , allFlag :: Bool
  } deriving (Show, Eq)

lsCommandParser :: ParserInfo LsCommand
lsCommandParser = info parser $ progDesc "List managed contracts"
  where
    parser = LsCommand <$> statusFlagParser <*> failedFlagParser <*> allFlagParser
    allFlagParser = flag False True $ mconcat
      [ long "all"
      , short 'a'
      , help "Show all Marlowe contracts on chain."
      ]
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
runLsCommand LsCommand{..}
  | allFlag = runDiscoveryQueryClient
    $ queryClient' Discovery.querySchema discoveryVersionMismatch
      $ pure $ SendMsgRequest GetContractHeaders ClientStNextCanReject
          { recvMsgReject = absurd
          , recvMsgNextPage = handleNextPageAll
          }
  | otherwise = runHistoryQueryClient
    $ queryClient' History.querySchema
      historyVersionMismatch
      (pure $ SendMsgRequest GetFollowedContracts ClientStNextCanReject
        { recvMsgReject = absurd
        , recvMsgNextPage = handleNextPage
        }
      )
  where
    discoveryVersionMismatch = const $ logAndDie "Discovery server version mismatch"
    historyVersionMismatch = const $ logAndDie "History server version mismatch"

    handleNextPageAll headers nextPage = do
      let ids = Set.fromList $ contractId <$> headers
      statuses <- fmap (either absurd id) $ runHistoryQueryClient $ liftQuery' History.querySchema historyVersionMismatch $ pure $ GetStatuses ids
      void $ Map.traverseWithKey printResult $ Map.union (Just <$> statuses) $ Map.fromSet (const Nothing) ids
      pure $ maybe
        (SendMsgRequestDone ())
        (flip SendMsgRequestNext $ ClientStNext handleNextPageAll)
        nextPage
    handleNextPage results nextPage = do
      void $ Map.traverseWithKey printResult $ Just <$> Map.filter filterResult results
      pure $ maybe
        (SendMsgRequestDone ())
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
      maybe (putStrLn "not added") print status
