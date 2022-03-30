-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | PAB-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards  #-}


module Language.Marlowe.CLI.Command.PAB (
-- * Marlowe CLI Commands
  PabCommand(..)
, parsePabCommand
, runPabCommand
) where

import Cardano.Api (AddressAny)
import Control.Monad.Except (MonadError, MonadIO, liftIO, runExceptT, throwError)
import Language.Marlowe.CLI.Command.Parse (parseInputContent, parsePOSIXTime, parseRole, parseUrl, parseWalletId)
import Language.Marlowe.CLI.PAB (callApplyInputs, callCreate, callFollow, callRedeem, runApp, runCompanion, runFollower,
                                 stop)
import Language.Marlowe.CLI.Types (CliError (..))
import Language.Marlowe.SemanticsTypes (InputContent)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.Socket (withSocketsDo)
import Network.WebSockets (runClient)
import Plutus.PAB.Events.Contract (ContractInstanceId (..))
import Plutus.PAB.Webserver.Client (pabClient)
import Plutus.V1.Ledger.Api (POSIXTime, TokenName)
import Servant.Client (BaseUrl (..), mkClientEnv, runClientM)
import Wallet.Emulator.Wallet (WalletId)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for running contracts.
data PabCommand =
    -- | Run the Marlowe contract.
    App
    {
      pabUrl        :: BaseUrl         -- ^ The URL for the Marlowe PAB.
    , walletId      :: WalletId        -- ^ The wallet ID for the contract.
    , loop          :: Bool            -- ^ Whether to report websocket messages until the contract stops.
    , paramsFile'   :: Maybe FilePath  -- ^ The output JSON file for the Marlowe parameters.
    , instanceFile' :: Maybe FilePath  -- ^ The output JSON file for the instance ID.
    }
    -- | Call the "create" endpoint.
  | Create
    {
      pabUrl       :: BaseUrl                    -- ^ The URL for the Marlowe PAB.
    , instanceFile :: FilePath                   -- ^ The file containing the instance ID.
    , contractFile :: FilePath                   -- ^ The JSON file containing the contract.
    , owners       :: [(TokenName, AddressAny)]  -- ^ The contract role names and their public key hashes.
    }
    -- | Call the "apply-inputs" endpoint.
  | ApplyInputs
    {
      pabUrl       :: BaseUrl                          -- ^ The URL for the Marlowe PAB.
    , instanceFile :: FilePath                         -- ^ The file containing the instance ID.
    , paramsFile   :: FilePath                         -- ^ The JSON file containing the contract parameters.
    , inputs       :: [(InputContent, Maybe FilePath)] -- ^ The contract's inputs, contract stub for merkleized action.
    , minimumTime  :: POSIXTime                        -- ^ The first valid time for the transaction.
    , maximumTime  :: POSIXTime                        -- ^ The last valid time for the transaction.
    }
    -- | Call the "redeem" endpoint.
  | Redeem
    {
      pabUrl       :: BaseUrl                  -- ^ The URL for the Marlowe PAB.
    , instanceFile :: FilePath                 -- ^ The file containing the instance ID.
    , paramsFile   :: FilePath                 -- ^ The JSON file containing the contract parameters.
    , owner        :: (TokenName, AddressAny)  -- ^ The contract role name and their public key hash.
    }
    -- | Run the follower contract.
  | Follower
    {
      pabUrl        :: BaseUrl         -- ^ The URL for the Marlowe PAB.
    , walletId      :: WalletId        -- ^ The wallet ID for the contract.
    , loop          :: Bool            -- ^ Whether to report websocket messages until the contract stops.
    , instanceFile' :: Maybe FilePath  -- ^ The output JSON file for the instance ID.
    }
    -- | Call the "follow" endpoint.
  | Follow
    {
      pabUrl       :: BaseUrl                  -- ^ The URL for the Marlowe PAB.
    , instanceFile :: FilePath                 -- ^ The file containing the instance ID.
    , paramsFile   :: FilePath                 -- ^ The JSON file containing the contract parameters.
    }
    -- | Run the companion contract.
  | Companion
    {
      pabUrl        :: BaseUrl         -- ^ The URL for the Marlowe PAB.
    , walletId      :: WalletId        -- ^ The wallet ID for the contract.
    , loop          :: Bool            -- ^ Whether to report websocket messages until the contract stops.
    , instanceFile' :: Maybe FilePath  -- ^ The output JSON file for the instance ID.
    }
    -- | Stop a contract.
  | Stop
    {
      pabUrl       :: BaseUrl   -- ^ The URL for the Marlowe PAB.
    , instanceFile :: FilePath  -- ^ The file containing the instance ID.
    }


-- | Run a contract-related command.
runPabCommand :: MonadError CliError m
              => MonadIO m
              => PabCommand  -- ^ The command.
              -> m ()        -- ^ Action for running the command.
runPabCommand command =
  do
    manager <- liftIO $ newManager defaultManagerSettings
    let
      BaseUrl{..} = pabUrl command
      client = pabClient
      apiClientEnv = mkClientEnv manager $ pabUrl command
      runApi f =
        do
          result <- liftIO $ runClientM f apiClientEnv
          case result of
            Right result' -> pure result'
            Left  e       -> throwError . CliError $ show e
      runWs ContractInstanceId{..} f =
        do
          result <-
            liftIO
              . withSocketsDo
              . runClient baseUrlHost baseUrlPort ("/ws/" <> show unContractInstanceId)
              $ runExceptT . f
          case result of
            Right result' -> pure result'
            Left  e       -> throwError e
    case command of
      App{..}         -> runApp paramsFile' loop client runApi runWs walletId instanceFile'
      Create{..}      -> callCreate client runApi instanceFile contractFile owners
      ApplyInputs{..} -> callApplyInputs client runApi instanceFile paramsFile inputs minimumTime maximumTime
      Redeem{..}      -> callRedeem client runApi instanceFile paramsFile owner
      Follower{..}    -> runFollower loop client runApi runWs walletId instanceFile'
      Follow{..}      -> callFollow client runApi instanceFile paramsFile
      Companion{..}   -> runCompanion loop client runApi runWs walletId instanceFile'
      Stop{..}        -> stop client runApi instanceFile


-- | Parser for PAB commands.
parsePabCommand :: O.Parser PabCommand
parsePabCommand =
  O.hsubparser
    $ O.commandGroup "Commands for running contracts on the PAB:"
    <> appCommand
    <> createCommand
    <> inputsCommand
    <> redeemCommand
    <> activateCommand "follower"  Follower
    <> followCommand
    <> activateCommand "companion" Companion
    <> stopCommand


-- | Parser for the "app" command.
appCommand :: O.Mod O.CommandFields PabCommand
appCommand =
  O.command "app"
    . O.info appOptions
    $ O.progDesc "Start the Marlowe application contract."


-- | Parser for the "app" options.
appOptions :: O.Parser PabCommand
appOptions =
  App
    <$> O.option parseUrl          (O.long "pab-url"           <> O.metavar "URL"           <> O.help "URL for the Marlowe PAB."                                   )
    <*> O.option parseWalletId     (O.long "wallet"            <> O.metavar "WALLET_ID"     <> O.help "Wallet ID for the contract."                                )
    <*> O.switch                   (O.long "loop"                                           <> O.help "Whether to listen to PAB messages until the contract stops.")
    <*> (O.optional . O.strOption) (O.long "out-params-file"   <> O.metavar "PARAMS_FILE"   <> O.help "Output file for the Marlowe parameters."                    )
    <*> (O.optional . O.strOption) (O.long "out-instance-file" <> O.metavar "INSTANCE_FILE" <> O.help "Output file for the instance ID."                           )


-- | Parser for the activate commands.
activateCommand :: String -> (BaseUrl -> WalletId -> Bool -> Maybe FilePath -> PabCommand) -> O.Mod O.CommandFields PabCommand
activateCommand name constructor =
  O.command name
    . O.info (activateOptions constructor)
    $ O.progDesc ("Start the Marlowe " <> name <> " contract.")


-- | Parser for the activate options.
activateOptions :: (BaseUrl -> WalletId -> Bool -> Maybe FilePath -> PabCommand) -> O.Parser PabCommand
activateOptions constructor =
  constructor
    <$> O.option parseUrl          (O.long "pab-url"  <> O.metavar "URL"           <> O.help "URL for the Marlowe PAB."                                   )
    <*> O.option parseWalletId     (O.long "wallet"   <> O.metavar "WALLET_ID"     <> O.help "Wallet ID for the contract."                                )
    <*> O.switch                   (O.long "loop"                                  <> O.help "Whether to listen to PAB messages until the contract stops.")
    <*> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "INSTANCE_FILE" <> O.help "Output file for the instance ID."                           )


-- | Parser for the "create" command.
createCommand :: O.Mod O.CommandFields PabCommand
createCommand =
  O.command "create"
    . O.info createOptions
    $ O.progDesc "Create a Marlowe contract."


-- | Parser for the "create" options.
createOptions :: O.Parser PabCommand
createOptions =
  Create
    <$> O.option parseUrl             (O.long "pab-url"       <> O.metavar "URL"           <> O.help "URL for the Marlowe PAB."         )
    <*> O.strOption                   (O.long "instance-file" <> O.metavar "INSTANCE_FILE" <> O.help "Input file for the instance ID."  )
    <*> O.strOption                   (O.long "contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract.")
    <*> (O.many . O.option parseRole) (O.long "owner"         <> O.metavar "ROLE=ADDRESS"  <> O.help "The role name and its address."   )


-- | Parser for the "apply-inputs" command.
inputsCommand :: O.Mod O.CommandFields PabCommand
inputsCommand =
  O.command "apply-inputs"
    . O.info inputsOptions
    $ O.progDesc "Apply inputs to a Marlowe contract."


-- | Parser for the "apply-inputs" options.
inputsOptions :: O.Parser PabCommand
inputsOptions =
  ApplyInputs
    <$> O.option parseUrl              (O.long "pab-url"           <> O.metavar "URL"           <> O.help "URL for the Marlowe PAB."                           )
    <*> O.strOption                    (O.long "instance-file"     <> O.metavar "INSTANCE_FILE" <> O.help "Input file for the instance ID."                    )
    <*> O.strOption                    (O.long "params-file"       <> O.metavar "PARAMS_FILE"   <> O.help "JSON input file for the Marlowe parameters."        )
    <*> O.many parseInputContent
    <*> O.option parsePOSIXTime        (O.long "invalid-before"    <> O.metavar "POSIX_TIME"    <> O.help "Minimum time for the input, in POSIX milliseconds." )
    <*> O.option parsePOSIXTime        (O.long "invalid-hereafter" <> O.metavar "POSIX_TIME"    <> O.help "Maximum time for the input, in POSIX milliseconds." )


-- | Parser for the "redeem" command.
redeemCommand :: O.Mod O.CommandFields PabCommand
redeemCommand =
  O.command "redeem"
    . O.info redeemOptions
    $ O.progDesc "Redeem funds from a Marlowe contract."


-- | Parser for the "redeem" options.
redeemOptions :: O.Parser PabCommand
redeemOptions =
  Redeem
    <$> O.option parseUrl  (O.long "pab-url"       <> O.metavar "URL"             <> O.help "URL for the Marlowe PAB."                   )
    <*> O.strOption        (O.long "instance-file" <> O.metavar "INSTANCE_FILE"   <> O.help "Input file for the instance ID."            )
    <*> O.strOption        (O.long "params-file"   <> O.metavar "PARAMS_FILE"     <> O.help "JSON input file for the Marlowe parameters.")
    <*> O.option parseRole (O.long "owner"         <> O.metavar "ROLE=ADDRESS"    <> O.help "The role name and its address."             )


-- | Parser for the "follow" command.
followCommand :: O.Mod O.CommandFields PabCommand
followCommand =
  O.command "follow"
    . O.info followOptions
    $ O.progDesc "Follow a Marlowe contract."


-- | Parser for the "follow" options.
followOptions :: O.Parser PabCommand
followOptions =
  Follow
    <$> O.option parseUrl  (O.long "pab-url"       <> O.metavar "URL"             <> O.help "URL for the Marlowe PAB."                   )
    <*> O.strOption        (O.long "instance-file" <> O.metavar "INSTANCE_FILE"   <> O.help "Input file for the instance ID."            )
    <*> O.strOption        (O.long "params-file"   <> O.metavar "PARAMS_FILE"     <> O.help "JSON input file for the Marlowe parameters.")


-- | Parser for the "stop" command.
stopCommand :: O.Mod O.CommandFields PabCommand
stopCommand =
  O.command "stop"
    . O.info stopOptions
    $ O.progDesc "Stop a Marlowe contract."


-- | Parser for the "stop" options.
stopOptions :: O.Parser PabCommand
stopOptions =
  Stop
    <$> O.option parseUrl (O.long "pab-url"       <> O.metavar "URL"           <> O.help "URL for the Marlowe PAB."       )
    <*> O.strOption       (O.long "instance-file" <> O.metavar "INSTANCE_FILE" <> O.help "Input file for the instance ID.")
