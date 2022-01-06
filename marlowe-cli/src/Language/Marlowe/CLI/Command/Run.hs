-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Contract-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Command.Run (
-- * Marlowe CLI Commands
  RunCommand(..)
, parseRunCommand
, runRunCommand
) where


import Cardano.Api (AddressAny, ConsensusModeParams (CardanoModeParams), EpochSlots (..), LocalNodeConnectInfo (..),
                    NetworkId (..), SlotNo, StakeAddressReference (..), TxIn)
import Control.Monad (when)
import Control.Monad.Except (MonadError, MonadIO, liftIO, throwError)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseCurrencySymbol, parseInput, parseNetworkId,
                                           parseSlotNo, parseStakeAddressReference, parseTokenName, parseTxIn,
                                           parseTxOut)
import Language.Marlowe.CLI.Run (initializeTransaction, prepareTransaction, runTransaction, withdrawFunds)
import Language.Marlowe.CLI.Types (CliError)
import Language.Marlowe.Client (defaultMarloweParams, marloweParams)
import Language.Marlowe.Semantics (MarloweParams (slotConfig))
import Language.Marlowe.SemanticsTypes (Input)
import Plutus.V1.Ledger.Api (CurrencySymbol, POSIXTime (..), TokenName, defaultCostModelParams)

import qualified Cardano.Api as Api (Value)
import qualified Options.Applicative as O


-- | Marlowe CLI commands and options for running contracts.
data RunCommand =
    -- | Initialize a Marlowe transaction.
    Initialize
    {
      network        :: Maybe NetworkId              -- ^ The network ID, if any.
    , slotLength     :: Integer                      -- ^ The slot length, in milliseconds.
    , slotZeroOffset :: Integer                      -- ^ The effective POSIX time of slot zero, in milliseconds.
    , stake          :: Maybe StakeAddressReference  -- ^ The stake address, if any.
    , rolesCurrency  :: Maybe CurrencySymbol         -- ^ The role currency symbols, if any.
    , contractFile   :: FilePath                     -- ^ The JSON file containing the contract.
    , stateFile      :: FilePath                     -- ^ The JSON file containing the contract's state.
    , outputFile     :: Maybe FilePath               -- ^ The output JSON file for the validator information.
    , printStats     :: Bool                         -- ^ Whether to print statistics about the contract.
    }
    -- | Prepare a Marlowe transaction for execution.
  | Prepare
    {
      marloweInFile :: FilePath        -- ^ The JSON file with Marlowe initial state and initial contract.
    , inputs'       :: [Input]         -- ^ The contract's inputs.
    , minimumSlot   :: SlotNo          -- ^ The first valid slot for the transaction.
    , maximumSlot   :: SlotNo          -- ^ The last valid slot for the transaction.
    , outputFile    :: Maybe FilePath  -- ^ The output JSON file with the results of the computation.
    , printStats    :: Bool            -- ^ Whether to print statistics about the redeemer.
    }
    -- | Run a Marlowe transaction.
  | Run
    {
      network         :: Maybe NetworkId               -- ^ The network ID, if any.
    , socketPath      :: FilePath                      -- ^ The path to the node socket.
    , marloweIn       :: Maybe (FilePath, TxIn, TxIn)  -- ^ The JSON file with the Marlowe initial state and initial contract, along with the script eUTxO being spend and the collateral, unless the transaction opens the contract.
    , marloweOut      :: FilePath                      -- ^ The JSON file with the Marlowe inputs, final state, and final contract.
    , inputs          :: [TxIn]                        -- ^ The ordinary transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]     -- ^ The ordinary transaction outputs.
    , change          :: AddressAny                    -- ^ The change address.
    , signingKeyFiles :: [FilePath]                    -- ^ The files containing the required signing keys.
    , bodyFile        :: FilePath                      -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int                     -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , printStats      :: Bool                          -- ^ Whether to print statistics about the contract and transaction.
    , invalid         :: Bool                          -- ^ Assertion that the transaction is invalid.
    }
    -- | Withdraw funds from a role address.
  | Withdraw
    {
      network         :: Maybe NetworkId           -- ^ The network ID, if any.
    , socketPath      :: FilePath                  -- ^ The path to the node socket.
    , marloweOut      :: FilePath                  -- ^ The JSON file with Marlowe state and contract.
    , roleName        :: TokenName                 -- ^ The role name for the redemption.
    , collateral      :: TxIn                      -- ^ The collateral.
    , inputs          :: [TxIn]                    -- ^ The ordinary transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)] -- ^ The ordinary transaction outputs.
    , change          :: AddressAny                -- ^ The change address.
    , signingKeyFiles :: [FilePath]                -- ^ The files containing the required signing keys.
    , bodyFile        :: FilePath                  -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int                 -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , printStats      :: Bool                      -- ^ Whether to print statistics about the contract and transaction.
    , invalid         :: Bool                      -- ^ Assertion that the transaction is invalid.
    }


-- | Run a contract-related command.
runRunCommand :: MonadError CliError m
                   => MonadIO m
                   => RunCommand  -- ^ The command.
                   -> m ()             -- ^ Action for running the command.
runRunCommand command =
  do
    costModel <-
      maybe
        (throwError "Missing default cost model.")
        pure
        defaultCostModelParams
    let
      network' = fromMaybe Mainnet $ network command
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network'
        , localNodeSocketPath      = socketPath command
        }
      marloweParams' = (maybe defaultMarloweParams marloweParams $ rolesCurrency command)
                       {
                         slotConfig = (slotLength command, POSIXTime $ slotZeroOffset command)
                       }
      stake'         = fromMaybe NoStakeAddress $ stake command
      printTxId = liftIO . putStrLn . ("TxId " <>) . show
      guardMainnet = when (network' == Mainnet) $ throwError "Mainnet usage is not supported."
      padTxOut (address, value) = (address, Nothing, value)
      outputs' = padTxOut <$> outputs command
    case command of
      Initialize{..} -> initializeTransaction
                          marloweParams' costModel network' stake'
                          contractFile stateFile
                          outputFile
                          printStats
      Prepare{..}    -> prepareTransaction
                          marloweInFile
                          inputs' minimumSlot maximumSlot
                          outputFile
                          printStats
      Run{..}        -> guardMainnet
                          >> runTransaction
                            connection
                            marloweIn marloweOut
                            inputs outputs' change
                            signingKeyFiles
                            bodyFile
                            submitTimeout
                            printStats
                            invalid
                          >>= printTxId
      Withdraw{..}   -> guardMainnet
                          >> withdrawFunds
                            connection
                            marloweOut roleName collateral
                            inputs outputs' change
                            signingKeyFiles
                            bodyFile
                            submitTimeout
                            printStats
                            invalid
                          >>= printTxId


-- | Parser for contract commands.
parseRunCommand :: O.Parser RunCommand
parseRunCommand =
  O.hsubparser
    $ O.commandGroup "Commands for running contracts:"
    <> runCommand
    <> initializeCommand
    <> prepareCommand
    <> withdrawCommand


-- | Parser for the "initialize" command.
initializeCommand :: O.Mod O.CommandFields RunCommand
initializeCommand =
  O.command "initialize"
    . O.info initializeOptions
    $ O.progDesc "Initialize the first transaction of a Marlowe contract and write output to a JSON file."


-- | Parser for the "initialize" options.
initializeOptions :: O.Parser RunCommand
initializeOptions =
  Initialize
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"                                  <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.option O.auto                                    (O.long "slot-length"    <> O.metavar "INTEGER"         <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto                                    (O.long "slot-offset"    <> O.metavar "INTEGER"         <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                                  <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL"                          <> O.help "The currency symbol for roles, if any."                 )
    <*> O.strOption                                        (O.long "contract-file"  <> O.metavar "CONTRACT_FILE"                            <> O.help "JSON input file for the contract."                      )
    <*> O.strOption                                        (O.long "state-file"     <> O.metavar "STATE_FILE"                               <> O.help "JSON input file for the contract state."                )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"                              <> O.help "JSON output file for initialize."                       )
    <*> O.switch                                           (O.long "print-stats"                                                            <> O.help "Print statistics."                                      )


-- | Parser for the "prepare" command.
prepareCommand :: O.Mod O.CommandFields RunCommand
prepareCommand =
  O.command "prepare"
    $ O.info prepareOptions
    $ O.progDesc "Prepare the next step of a Marlowe contract and write the output to a JSON file."


-- | Parser for the "prepare" options.
prepareOptions :: O.Parser RunCommand
prepareOptions =
  Prepare
    <$> O.strOption                (O.long "marlowe-file"      <> O.metavar "MARLOWE_FILE"  <> O.help "JSON input file for the Marlowe state and contract.")
    <*> O.many parseInput
    <*> O.option parseSlotNo       (O.long "invalid-before"    <> O.metavar "SLOT"          <> O.help "Minimum slot for the redemption."                   )
    <*> O.option parseSlotNo       (O.long "invalid-hereafter" <> O.metavar "SLOT"          <> O.help "Maximum slot for the redemption."                   )
    <*> (O.optional . O.strOption) (O.long "out-file"          <> O.metavar "OUTPUT_FILE"   <> O.help "JSON output file for contract."                     )
    <*> O.switch                   (O.long "print-stats"                                    <> O.help "Print statistics."                                  )


-- | Parser for the "execute" command.
runCommand :: O.Mod O.CommandFields RunCommand
runCommand =
  O.command "execute"
    $ O.info runOptions
    $ O.progDesc "Run a Marlowe transaction."


-- | Parser for the "execute" options.
runOptions :: O.Parser RunCommand
runOptions =
  Run
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"   <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                                )
    <*> O.strOption                            (O.long "socket-path"     <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."                          )
    <*> O.optional parseMarloweIn
    <*> O.strOption                            (O.long "marlowe-out-file"<> O.metavar "MARLOWE_FILE"  <> O.help "JSON file with the Marlowe inputs, final state, and final contract.")
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"           <> O.metavar "TXID#TXIX"     <> O.help "Transaction input in TxId#TxIx format."                             )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"          <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."                        )
    <*> O.option parseAddressAny               (O.long "change-address"  <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."                           )
    <*> (O.many . O.strOption)                 (O.long "required-signer" <> O.metavar "SIGNING_FILE"  <> O.help "File containing a required signing key."                            )
    <*> O.strOption                            (O.long "out-file"        <> O.metavar "FILE"          <> O.help "Output file for transaction body."                                  )
    <*> (O.optional . O.option O.auto)         (O.long "submit"          <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation."            )
    <*> O.switch                               (O.long "print-stats"                                  <> O.help "Print statistics."                                                  )
    <*> O.switch                               (O.long "script-invalid"                               <> O.help "Assert that the transaction is invalid."                            )
    where
      parseMarloweIn :: O.Parser (FilePath, TxIn, TxIn)
      parseMarloweIn =
        (,,)
          <$> O.strOption        (O.long "marlowe-in-file"  <> O.metavar "MARLOWE_FILE" <> O.help "JSON file with the Marlowe initial state and initial contract, if any.")
          <*> O.option parseTxIn (O.long "tx-in-marlowe"    <> O.metavar "TXID#TXIX"    <> O.help "UTxO spent from Marlowe contract, if any."                             )
          <*> O.option parseTxIn (O.long "tx-in-collateral" <> O.metavar "TXID#TXIX"    <> O.help "Collateral for transaction, if any."                                   )


-- | Parser for the "withdraw" command.
withdrawCommand :: O.Mod O.CommandFields RunCommand
withdrawCommand =
  O.command "withdraw"
    $ O.info withdrawOptions
    $ O.progDesc "Withdraw funds from the Marlowe role address."


-- | Parser for the "withdraw" options.
withdrawOptions :: O.Parser RunCommand
withdrawOptions =
  Withdraw
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"    <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                                )
    <*> O.strOption                            (O.long "socket-path"      <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."                          )
    <*> O.strOption                            (O.long "marlowe-file"     <> O.metavar "MARLOWE_FILE"  <> O.help "JSON file with the Marlowe inputs, final state, and final contract.")
    <*> O.option parseTokenName                (O.long "role-name"        <> O.metavar "TOKEN_NAME"    <> O.help "The role name for the withdrawal."                                  )
    <*> O.option parseTxIn                     (O.long "tx-in-collateral" <> O.metavar "TXID#TXIX"     <> O.help "Collateral for transaction."                                        )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"            <> O.metavar "TXID#TXIX"     <> O.help "Transaction input in TxId#TxIx format."                             )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"           <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."                        )
    <*> O.option parseAddressAny               (O.long "change-address"   <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."                           )
    <*> (O.many . O.strOption)                 (O.long "required-signer"  <> O.metavar "SIGNING_FILE"  <> O.help "File containing a required signing key."                            )
    <*> O.strOption                            (O.long "out-file"         <> O.metavar "FILE"          <> O.help "Output file for transaction body."                                  )
    <*> (O.optional . O.option O.auto)         (O.long "submit"           <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation."            )
    <*> O.switch                               (O.long "print-stats"                                   <> O.help "Print statistics."                                                  )
    <*> O.switch                               (O.long "script-invalid"                                <> O.help "Assert that the transaction is invalid."                            )
