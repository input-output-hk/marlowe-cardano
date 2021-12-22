-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Main entry point for Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI (
-- * Entry Point
  mainCLI
) where


import           Cardano.Api                           (ConsensusModeParams (CardanoModeParams), EpochSlots (..),
                                                        LocalNodeConnectInfo (..), NetworkId (..),
                                                        StakeAddressReference (..))
import           Cardano.Config.Git.Rev                (gitRev)
import           Control.Monad                         (when)
import           Control.Monad.Except                  (liftIO, runExceptT, throwError)
import           Data.Maybe                            (fromMaybe)
import           Data.Version                          (Version, showVersion)
import           Language.Marlowe.CLI.Examples         (makeExample)
import           Language.Marlowe.CLI.Export           (exportAddress, exportDatum, exportMarlowe, exportRedeemer,
                                                        exportValidator)
import           Language.Marlowe.CLI.Parse            (parseAddressAny, parseCurrencySymbol, parseNetworkId,
                                                        parseParty, parseSlot, parseSlotNo, parseStakeAddressReference,
                                                        parseToken, parseTxIn, parseTxOut, parseValue)
import           Language.Marlowe.CLI.Run              (computeMarlowe, makeChoice, makeDeposit, makeNotification)
import           Language.Marlowe.CLI.Transaction      (buildContinuing, buildIncoming, buildOutgoing, buildSimple,
                                                        submit)
import           Language.Marlowe.CLI.Types            (CliError (..), Command (..))
import           Language.Marlowe.Client               (defaultMarloweParams, marloweParams)
import           Language.Marlowe.Semantics            (MarloweParams (slotConfig))
import           Plutus.V1.Ledger.Api                  (POSIXTime (..), defaultCostModelParams)
import           System.Exit                           (exitFailure)
import           System.IO                             (hPutStrLn, stderr)

import qualified Data.Text                             as T (unpack)
import qualified Language.Marlowe.CLI.Examples.Escrow  as Example (makeEscrowContract)
import qualified Language.Marlowe.CLI.Examples.Swap    as Example (makeSwapContract)
import qualified Language.Marlowe.CLI.Examples.Trivial as Example (makeTrivialContract)
import qualified Options.Applicative                   as O


-- | Main entry point for Marlowe CLI tool.
mainCLI :: Version  -- ^ The version of the tool.
        -> IO ()    -- ^ Action to run the tool.
mainCLI version =
  do
    command <- O.execParser $ parser version
    result <-
      runExceptT
        $ do
          costModel <-
            maybe
              (throwError "Missing default cost model.")
              pure
              defaultCostModelParams
          let
            network' = fromMaybe Mainnet $ network command
            marloweParams' = (maybe defaultMarloweParams marloweParams $ rolesCurrency command)
                             {
                               slotConfig = (slotLength command, POSIXTime $ slotZeroOffset command)
                             }
            stake'         = fromMaybe NoStakeAddress                 $ stake         command
            connection =
              LocalNodeConnectInfo
              {
                localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
              , localNodeNetworkId       = network'
              , localNodeSocketPath      = socketPath command
              }
            printTxId = liftIO . putStrLn . ("TxId " <>) . show
            guardMainnet = when (network' == Mainnet) $ throwError "Mainnet usage is not supported."
          case command of
            Export{..}          -> exportMarlowe
                                     marloweParams' costModel network' stake'
                                     contractFile stateFile
                                     inputFiles
                                     outputFile
                                     printStats
            ExportAddress{}     -> exportAddress
                                     marloweParams' network' stake'
            ExportValidator{..} -> exportValidator
                                     marloweParams' costModel network' stake'
                                     outputFile
                                     printHash printStats
            ExportDatum{..}     -> exportDatum
                                     contractFile stateFile
                                     outputFile
                                     printStats
            ExportRedeemer{..}  -> exportRedeemer
                                     inputFiles
                                     outputFile
                                     printStats
            BuildTransact{..}   -> guardMainnet
                                     >> buildSimple
                                       connection
                                       signingKeyFiles
                                       inputs outputs change
                                       bodyFile
                                       submitTimeout
                                       printStats
                                       invalid
                                     >>= printTxId
            BuildCreate{..}     -> guardMainnet
                                     >> buildIncoming
                                       connection
                                       scriptAddress
                                       signingKeyFiles
                                       outputDatumFile
                                       outputValue
                                       inputs outputs change
                                       bodyFile
                                       submitTimeout
                                       printStats
                                       invalid
                                     >>= printTxId
            BuildAdvance{..}    -> guardMainnet
                                     >> buildContinuing
                                       connection
                                       scriptAddress
                                       validatorFile
                                       redeemerFile
                                       inputDatumFile
                                       signingKeyFiles
                                       inputTxIn
                                       outputDatumFile
                                       outputValue
                                       inputs outputs collateral change
                                       minimumSlot maximumSlot
                                       bodyFile
                                       submitTimeout
                                       printStats
                                       invalid
                                     >>= printTxId
            BuildClose{..}      -> guardMainnet
                                     >> buildOutgoing
                                       connection
                                       validatorFile
                                       redeemerFile
                                       inputDatumFile
                                       signingKeyFiles
                                       inputTxIn
                                       inputs outputs collateral change
                                       minimumSlot maximumSlot
                                       bodyFile
                                       submitTimeout
                                       printStats
                                       invalid
                                     >>= printTxId
            Submit{..}          -> guardMainnet
                                     >> submit
                                       connection
                                       bodyFile
                                       signingKeyFiles
                                       (fromMaybe 0 submitTimeout)
                                     >>= printTxId
            Compute{..}         -> computeMarlowe
                                     contractFile stateFile
                                     inputFiles minimumSlot maximumSlot
                                     outputFile
                                     printStats
            InputDeposit{..}    -> makeDeposit
                                     account party token amount
                                     outputFile
            InputChoice{..}     -> makeChoice
                                     choiceName choiceParty chosen
                                     outputFile
            InputNotify{..}     -> makeNotification
                                     outputFile
            TemplateTrivial{..} -> makeExample outputFile
                                     $ Example.makeTrivialContract
                                         bystander
                                         minAda
                                         minSlot
                                         party
                                         depositLovelace
                                         withdrawalLovelace
                                         timeout
            TemplateEscrow{..}  -> makeExample outputFile
                                     $ Example.makeEscrowContract
                                         minAda
                                         price
                                         seller
                                         buyer
                                         mediator
                                         paymentDeadline
                                         complaintDeadline
                                         disputeDeadline
                                         mediationDeadline
            TemplateSwap{..}    -> makeExample outputFile
                                     $ Example.makeSwapContract
                                         minAda
                                         aParty
                                         aToken
                                         aAmount
                                         aTimeout
                                         bParty
                                         bToken
                                         bAmount
                                         bTimeout
    case result of
      Right ()      -> return ()
      Left  message -> do
                         hPutStrLn stderr $ unCliError message
                         exitFailure


-- | Command parser for the tool version.
parser :: Version               -- ^ The tool version.
       -> O.ParserInfo Command  -- ^ The command parser.
parser version =
  O.info
    (
          O.helper
      <*> versionOption version
      <*> O.subparser
            (
                 exportMarloweCommand
              <> exportAddressCommand
              <> exportValidatorCommand
              <> exportDatumCommand
              <> exportRedeemerCommand
              <> buildSimpleCommand
              <> buildIncomingCommand
              <> buildContinuingCommand
              <> buildOutgoingCommand
              <> submitCommand
              <> inputDepositCommand
              <> inputChoiceCommand
              <> inputNotifyCommand
              <> computeCommand
              <> templateTrivialCommand
              <> templateEscrowCommand
              <> templateSwapCommand
            )
    )
    (
         O.fullDesc
      <> O.progDesc "Utilities for Marlowe."
      <> O.header "marlowe-cli"
    )


-- | Option parser for the tool versoin.
versionOption :: Version           -- ^ The tool version.
              -> O.Parser (a -> a) -- ^ The option parser.
versionOption version =
  O.infoOption
    ("marlowe-cli " <> showVersion version <> " @ " <> T.unpack gitRev)
    (O.long "version" <> O.help "Show version.")


-- | Parser for the "export" command.
exportMarloweCommand :: O.Mod O.CommandFields Command -- ^ The parser.
exportMarloweCommand =
  O.command "export-marlowe"
    $ O.info (exportMarloweOptions O.<**> O.helper)
    $ O.progDesc "Export a Marlowe contract to a JSON file."


-- | Parser for the "export" options.
exportMarloweOptions :: O.Parser Command -- ^ The parser.
exportMarloweOptions =
  Export
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"                                  <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.option O.auto                                    (O.long "slot-length"    <> O.metavar "INTEGER"         <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto                                    (O.long "slot-offset"    <> O.metavar "INTEGER"         <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                                  <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL"                          <> O.help "The currency symbol for roles, if any."                 )
    <*> O.strOption                                        (O.long "contract-file"  <> O.metavar "CONTRACT_FILE"                            <> O.help "JSON input file for the contract."                      )
    <*> O.strOption                                        (O.long "state-file"     <> O.metavar "STATE_FILE"                               <> O.help "JSON input file for the contract state."                )
    <*> (O.many . O.strOption)                             (O.long "input-file"     <> O.metavar "INPUT_FILE"                               <> O.help "JSON input file for redeemer inputs."                   )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"                              <> O.help "JSON output file for contract."                         )
    <*> O.switch                                           (O.long "print-stats"                                                            <> O.help "Print statistics."                                      )


-- | Parser for the "address" command.
exportAddressCommand :: O.Mod O.CommandFields Command
exportAddressCommand =
  O.command "export-address"
    . O.info (exportAddressOptions O.<**> O.helper)
    $ O.progDesc "Print a validator address."


-- | Parser for the "address" options.
exportAddressOptions :: O.Parser Command
exportAddressOptions =
  ExportAddress
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"                                  <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.option O.auto                                    (O.long "slot-length"    <> O.metavar "INTEGER"         <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto                                    (O.long "slot-offset"    <> O.metavar "INTEGER"         <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                                  <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL"                          <> O.help "The currency symbol for roles, if any."                 )


-- | Parser for the "validator" command.
exportValidatorCommand :: O.Mod O.CommandFields Command
exportValidatorCommand =
  O.command "export-validator"
    . O.info (exportValidatorOptions O.<**> O.helper)
    $ O.progDesc "Export a validator to a JSON file."


-- | Parser for the "validator" options.
exportValidatorOptions :: O.Parser Command
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"                                  <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.option O.auto                                    (O.long "slot-length"    <> O.metavar "INTEGER"         <> O.value 1000          <> O.help "The slot length, in milliseconds."                      )
    <*> O.option O.auto                                    (O.long "slot-offset"    <> O.metavar "INTEGER"         <> O.value 1591566291000 <> O.help "The effective POSIX time of slot zero, in milliseconds.")
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"                                  <> O.help "Stake address, if any."                                 )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL"                          <> O.help "The currency symbol for roles, if any."                 )
    <*> (O.optional . O.strOption)                         (O.long "out-file"       <> O.metavar "OUTPUT_FILE"                              <> O.help "JSON output file for validator."                        )
    <*> O.switch                                           (O.long "print-hash"                                                             <> O.help "Print validator hash."                                  )
    <*> O.switch                                           (O.long "print-stats"                                                            <> O.help "Print statistics."                                      )


-- | Parser for the "datum" command.
exportDatumCommand :: O.Mod O.CommandFields Command
exportDatumCommand =
  O.command "export-datum"
    . O.info (exportDatumOptions O.<**> O.helper)
    $ O.progDesc "Export a datum to a JSON file."


-- | Parser for the "datum" options.
exportDatumOptions :: O.Parser Command
exportDatumOptions =
  ExportDatum
    <$> O.strOption                (O.long "contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract."      )
    <*> O.strOption                (O.long "state-file"    <> O.metavar "STATE_FILE"    <> O.help "JSON input file for the contract state.")
    <*> (O.optional . O.strOption) (O.long "out-file"      <> O.metavar "DATUM_FILE"    <> O.help "JSON output file for datum."            )
    <*> O.switch                   (O.long "print-stats"                                <> O.help "Print statistics."                      )


-- | Parser for the "redeemer" command.
exportRedeemerCommand :: O.Mod O.CommandFields Command
exportRedeemerCommand =
  O.command "export-redeemer"
    . O.info (exportRedeemerOptions O.<**> O.helper)
    $ O.progDesc "Export a redeemer to a JSON file."


-- | Parser for the "redeemer" options.
exportRedeemerOptions :: O.Parser Command
exportRedeemerOptions =
  ExportRedeemer
    <$> (O.many . O.strOption)     (O.long "input-file"  <> O.metavar "INPUT_FILE"  <> O.help "JSON input file for redeemer inputs.")
    <*> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer."      )
    <*> O.switch                   (O.long "print-stats"                            <> O.help "Print statistics."                   )


-- | Parser for the "transact" command.
buildSimpleCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildSimpleCommand =
  O.command "transaction-simple"
    $ O.info (buildSimpleOptions O.<**> O.helper)
    $ O.progDesc "Build a non-Marlowe transaction."


-- | Parser for the "transact" options.
buildSimpleOptions :: O.Parser Command -- ^ The parser.
buildSimpleOptions =
  BuildTransact
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"   <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"     <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."              )
    <*> (O.many . O.strOption)                 (O.long "required-signer" <> O.metavar "SIGNING_FILE"  <> O.help "Files containing required signing keys."                )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"           <> O.metavar "TXID#TXIX"     <> O.help "Transaction input in TxId#TxIx format."                 )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"          <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."            )
    <*> O.option parseAddressAny               (O.long "change-address"  <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."               )
    <*> O.strOption                            (O.long "out-file"        <> O.metavar "FILE"          <> O.help "Output file for transaction body."                      )
    <*> (O.optional . O.option O.auto)         (O.long "submit"          <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation.")
    <*> O.switch                               (O.long "print-stats"                                  <> O.help "Print statistics."                                      )
    <*> O.switch                               (O.long "script-invalid"                               <> O.help "Assert that the transaction is invalid."                )


-- | Parser for the "create" command.
buildIncomingCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildIncomingCommand =
  O.command "transaction-create"
    $ O.info (buildIncomingOptions O.<**> O.helper)
    $ O.progDesc "Build a transaction that pays to a Marlowe script."


-- | Parser for the "create" options.
buildIncomingOptions :: O.Parser Command -- ^ The parser.
buildIncomingOptions =
  BuildCreate
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"     <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"       <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."              )
    <*> O.option parseAddressAny               (O.long "script-address"    <> O.metavar "ADDRESS"       <> O.help "Address of the Marlowe contract."                       )
    <*> (O.many . O.strOption)                 (O.long "required-signer"   <> O.metavar "SIGNING_FILE"  <> O.help "Files containing required signing keys."                )
    <*> O.strOption                            (O.long "tx-out-datum-file" <> O.metavar "DATUM_FILE"    <> O.help "Datum JSON file datum paid to Marlowe contract."        )
    <*> O.option parseValue                    (O.long "tx-out-marlowe"    <> O.metavar "VALUE"         <> O.help "Value paid to Marlowe contract."                        )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"             <> O.metavar "TXID#TXIX"     <> O.help "Transaction input in TxId#TxIx format."                 )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"            <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."            )
    <*> O.option parseAddressAny               (O.long "change-address"    <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."               )
    <*> O.strOption                            (O.long "out-file"          <> O.metavar "FILE"          <> O.help "Output file for transaction body."                      )
    <*> (O.optional . O.option O.auto)         (O.long "submit"            <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation.")
    <*> O.switch                               (O.long "print-stats"                                    <> O.help "Print statistics."                                      )
    <*> O.switch                               (O.long "script-invalid"                                 <> O.help "Assert that the transaction is invalid."                )


-- | Parser for the "advance" command.
buildContinuingCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildContinuingCommand =
  O.command "transaction-advance"
    $ O.info (buildContinuingOptions O.<**> O.helper)
    $ O.progDesc "Build a transaction that both spends from and pays to a Marlowe script."


-- | Parser for the "advance" options.
buildContinuingOptions :: O.Parser Command -- ^ The parser.
buildContinuingOptions =
  BuildAdvance
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"       <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"         <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."              )
    <*> O.option parseAddressAny               (O.long "script-address"      <> O.metavar "ADDRESS"       <> O.help "Address of the Marlowe contract."                       )
    <*> O.strOption                            (O.long "tx-in-script-file"   <> O.metavar "PLUTUS_FILE"   <> O.help "Plutus file for Marlowe contract."                      )
    <*> O.strOption                            (O.long "tx-in-redeemer-file" <> O.metavar "REDEEMER_FILE" <> O.help "Redeemer JSON file spent from Marlowe contract."        )
    <*> O.strOption                            (O.long "tx-in-datum-file"    <> O.metavar "DATUM_FILE"    <> O.help "Datum JSON file spent from Marlowe contract."           )
    <*> (O.many . O.strOption)                 (O.long "required-signer"     <> O.metavar "SIGNING_FILE"  <> O.help "Files containing required signing keys."                )
    <*> O.option parseTxIn                     (O.long "tx-in-marlowe"       <> O.metavar "TXID#TXIX"     <> O.help "UTxO spent from Marlowe contract."                      )
    <*> O.strOption                            (O.long "tx-out-datum-file"   <> O.metavar "DATUM_FILE"    <> O.help "Datum JSON file datum paid to Marlowe contract."        )
    <*> O.option parseValue                    (O.long "tx-out-marlowe"      <> O.metavar "VALUE"         <> O.help "Value paid to Marlowe contract."                        )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"               <> O.metavar "TXID#TXIX"     <> O.help "Transaction input in TxId#TxIx format."                 )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"              <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."            )
    <*> O.option parseTxIn                     (O.long "tx-in-collateral"    <> O.metavar "TXID#TXIX"     <> O.help "Collateral for transaction."                            )
    <*> O.option parseAddressAny               (O.long "change-address"      <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."               )
    <*> O.option parseSlotNo                   (O.long "invalid-before"      <> O.metavar "SLOT"          <> O.help "Minimum slot for the redemption."                       )
    <*> O.option parseSlotNo                   (O.long "invalid-hereafter"   <> O.metavar "SLOT"          <> O.help "Maximum slot for the redemption."                       )
    <*> O.strOption                            (O.long "out-file"            <> O.metavar "FILE"          <> O.help "Output file for transaction body."                      )
    <*> (O.optional . O.option O.auto)         (O.long "submit"              <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation.")
    <*> O.switch                               (O.long "print-stats"                                      <> O.help "Print statistics."                                      )
    <*> O.switch                               (O.long "script-invalid"                                   <> O.help "Assert that the transaction is invalid."                )


-- | Parser for the "close" command.
buildOutgoingCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildOutgoingCommand =
  O.command "transaction-close"
    $ O.info (buildOutgoingOptions O.<**> O.helper)
    $ O.progDesc "Build a transaction that spends from a Marlowe script."


-- | Parser for the "close" options.
buildOutgoingOptions :: O.Parser Command -- ^ The parser.
buildOutgoingOptions =
  BuildClose
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"       <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"         <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."              )
    <*> O.strOption                            (O.long "tx-in-script-file"   <> O.metavar "PLUTUS_FILE"   <> O.help "Plutus file for Marlowe contract."                      )
    <*> O.strOption                            (O.long "tx-in-redeemer-file" <> O.metavar "REDEEMER_FILE" <> O.help "Redeemer JSON file spent from Marlowe contract."        )
    <*> O.strOption                            (O.long "tx-in-datum-file"    <> O.metavar "DATUM_FILE"    <> O.help "Datum JSON file spent from Marlowe contract."           )
    <*> (O.many . O.strOption)                 (O.long "required-signer"     <> O.metavar "SIGNING_FILE"  <> O.help "Files containing required signing keys."                )
    <*> O.option parseTxIn                     (O.long "tx-in-marlowe"       <> O.metavar "TXID#TXIX"     <> O.help "UTxO spent from Marlowe contract."                      )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"               <> O.metavar "TXID#TXIX"     <> O.help "Transaction input in TxId#TxIx format."                 )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"              <> O.metavar "ADDRESS+VALUE" <> O.help "Transaction output in ADDRESS+VALUE format."            )
    <*> O.option parseTxIn                     (O.long "tx-in-collateral"    <> O.metavar "TXID#TXIX"     <> O.help "Collateral for transaction."                            )
    <*> O.option parseAddressAny               (O.long "change-address"      <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."               )
    <*> O.option parseSlotNo                   (O.long "invalid-before"      <> O.metavar "SLOT"          <> O.help "Minimum slot for the redemption."                       )
    <*> O.option parseSlotNo                   (O.long "invalid-hereafter"   <> O.metavar "SLOT"          <> O.help "Maximum slot for the redemption."                       )
    <*> O.strOption                            (O.long "out-file"            <> O.metavar "FILE"          <> O.help "Output file for transaction body."                      )
    <*> (O.optional . O.option O.auto)         (O.long "submit"              <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation.")
    <*> O.switch                               (O.long "print-stats"                                      <> O.help "Print statistics."                                      )
    <*> O.switch                               (O.long "script-invalid"                                   <> O.help "Assert that the transaction is invalid."                )


-- | Parser for the "submit" command.
submitCommand :: O.Mod O.CommandFields Command -- ^ The parser.
submitCommand =
  O.command "transaction-submit"
    $ O.info (submitOptions O.<**> O.helper)
    $ O.progDesc "Submit a transaction body."


-- | Parser for the "submit" options.
submitOptions :: O.Parser Command
submitOptions =
  Submit
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"   <> O.metavar "INTEGER"      <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"     <> O.metavar "SOCKET_FILE"  <> O.help "Location of the cardano-node socket file."              )
    <*> O.strOption                            (O.long "tx-body-file"    <> O.metavar "BODY_FILE"    <> O.help "File containing the transaction body."                  )
    <*> (O.many . O.strOption)                 (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "Files containing required signing keys."                )
    <*> (O.optional . O.option O.auto)         (O.long "timeout"         <> O.metavar "SECONDS"      <> O.help "Also submit the transaction, and wait for confirmation.")


-- | Parser for the "compute" command.
computeCommand :: O.Mod O.CommandFields Command -- ^ The parser.
computeCommand =
  O.command "compute"
    $ O.info (computeOptions O.<**> O.helper)
    $ O.progDesc "Compute the next step of a Marlowe contract and write the output to a JSON file."


-- | Parser for the "compute" options.
computeOptions :: O.Parser Command -- ^ The parser.
computeOptions =
  Compute
    <$> O.strOption                (O.long "contract-file"     <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract."      )
    <*> O.strOption                (O.long "state-file"        <> O.metavar "STATE_FILE"    <> O.help "JSON input file for the contract state.")
    <*> (O.many . O.strOption)     (O.long "input-file"        <> O.metavar "INPUT_FILE"    <> O.help "JSON input file for redeemer inputs."   )
    <*> O.option parseSlotNo       (O.long "invalid-before"    <> O.metavar "SLOT"          <> O.help "Minimum slot for the redemption."       )
    <*> O.option parseSlotNo       (O.long "invalid-hereafter" <> O.metavar "SLOT"          <> O.help "Maximum slot for the redemption."       )
    <*> (O.optional . O.strOption) (O.long "out-file"          <> O.metavar "OUTPUT_FILE"   <> O.help "JSON output file for contract."         )
    <*> O.switch                   (O.long "print-stats"                                    <> O.help "Print statistics."                      )


-- | Parser for the "deposit" command.
inputDepositCommand :: O.Mod O.CommandFields Command -- ^ The parser.
inputDepositCommand =
  O.command "input-deposit"
    $ O.info (inputDepositOptions O.<**> O.helper)
    $ O.progDesc "Create Marlowe input for a deposit."


-- | Parser for the "deposit" options.
inputDepositOptions :: O.Parser Command -- ^ The parser.
inputDepositOptions =
  InputDeposit
    <$> O.option parseParty                (O.long "deposit-account" <> O.metavar "PARTY"       <> O.help "The account for the deposit."          )
    <*> O.option parseParty                (O.long "deposit-party"   <> O.metavar "PARTY"       <> O.help "The party making the deposit."         )
    <*> (O.optional . O.option parseToken) (O.long "deposit-token"   <> O.metavar "TOKEN"       <> O.help "The token being deposited, if not Ada.")
    <*> O.option O.auto                    (O.long "deposit-amount"  <> O.metavar "INTEGER"     <> O.help "The amount of token being deposited."  )
    <*> (O.optional . O.strOption)         (O.long "out-file"        <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input."  )


-- | Parser for the "choose" command.
inputChoiceCommand :: O.Mod O.CommandFields Command -- ^ The parser.
inputChoiceCommand =
  O.command "input-choose"
    $ O.info (inputChoiceOptions O.<**> O.helper)
    $ O.progDesc "Create Marlowe input for a choice."


-- | Parser for the "choose" options.
inputChoiceOptions :: O.Parser Command -- ^ The parser.
inputChoiceOptions =
  InputChoice
    <$> O.strOption                (O.long "choice-name"   <> O.metavar "NAME"        <> O.help "The name of the choice made."        )
    <*> O.option parseParty        (O.long "choice-party"  <> O.metavar "PARTY"       <> O.help "The party making the choice."        )
    <*> O.option O.auto            (O.long "choice-number" <> O.metavar "INTEGER"     <> O.help "The number chosen."                  )
    <*> (O.optional . O.strOption) (O.long "out-file"      <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input.")


-- | Parser for the "notify" command.
inputNotifyCommand :: O.Mod O.CommandFields Command -- ^ The parser.
inputNotifyCommand =
  O.command "input-notify"
    $ O.info (inputNotifyOptions O.<**> O.helper)
    $ O.progDesc "Create Marlowe input for a notification."


-- | Parser for the "notify" options.
inputNotifyOptions :: O.Parser Command -- ^ The parser.
inputNotifyOptions =
  InputNotify
    <$> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input.")


-- | Parser for the "contract-trivial" command.
templateTrivialCommand :: O.Mod O.CommandFields Command -- ^ The parser.
templateTrivialCommand =
  O.command "contract-trivial"
    $ O.info (templateTrivialOptions O.<**> O.helper)
    $ O.progDesc "Template for a trivial contract."


-- | Parser for the "contract-trivial" options.
templateTrivialOptions :: O.Parser Command
templateTrivialOptions =
  TemplateTrivial
    <$> O.option parseParty        (O.long "bystander"           <> O.metavar "PARTY"       <> O.help "The party providing the min-ADA."    )
    <*> O.option O.auto            (O.long "minimum-ada"         <> O.metavar "INTEGER"     <> O.help "Lovelace in the initial state."      )
    <*> O.option parseSlot         (O.long "minimum-slot"        <> O.metavar "SLOT"        <> O.help "The minimum slot."                   )
    <*> O.option parseParty        (O.long "party"               <> O.metavar "PARTY"       <> O.help "The party."                          )
    <*> O.option O.auto            (O.long "deposit-lovelace"    <> O.metavar "INTEGER"     <> O.help "Lovelace in the deposit."            )
    <*> O.option O.auto            (O.long "withdrawal-lovelace" <> O.metavar "INTEGER"     <> O.help "Lovelace in the withdrawal."         )
    <*> O.option parseSlot         (O.long "timeout"             <> O.metavar "SLOT"        <> O.help "The timeout."                        )
    <*> (O.optional . O.strOption) (O.long "out-file"            <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input.")


-- | Parser for the "contract-escrow" command.
templateEscrowCommand :: O.Mod O.CommandFields Command -- ^ The parser.
templateEscrowCommand =
  O.command "contract-escrow"
    $ O.info (templateEscrowOptions O.<**> O.helper)
    $ O.progDesc "Template for a escrow contract."


-- | Parser for the "contract-escrow" options.
templateEscrowOptions :: O.Parser Command
templateEscrowOptions =
  TemplateEscrow
    <$> O.option O.auto            (O.long "minimum-ada"        <> O.metavar "INTEGER"     <> O.help "Lovelace in the initial state."                     )
    <*> O.option O.auto            (O.long "price"              <> O.metavar "INTEGER"     <> O.help "The price of the sale, in lovelace."                )
    <*> O.option parseParty        (O.long "seller"             <> O.metavar "PARTY"       <> O.help "The seller."                                        )
    <*> O.option parseParty        (O.long "buyer"              <> O.metavar "PARTY"       <> O.help "The buyer."                                         )
    <*> O.option parseParty        (O.long "mediator"           <> O.metavar "PARTY"       <> O.help "The mediator."                                      )
    <*> O.option parseSlot         (O.long "payment-deadline"   <> O.metavar "SLOT"        <> O.help "The deadline for the buyer to pay."                 )
    <*> O.option parseSlot         (O.long "complaint-deadline" <> O.metavar "SLOT"        <> O.help "The deadline for the buyer to complain."            )
    <*> O.option parseSlot         (O.long "dispute-deadline"   <> O.metavar "SLOT"        <> O.help "The deadline for the seller to dispute a complaint.")
    <*> O.option parseSlot         (O.long "mediation-deadline" <> O.metavar "SLOT"        <> O.help "The deadline for the mediator to decide."           )
    <*> (O.optional . O.strOption) (O.long "out-file"           <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input."               )


-- | Parser for the "contract-swap" command.
templateSwapCommand :: O.Mod O.CommandFields Command -- ^ The parser.
templateSwapCommand =
  O.command "contract-swap"
    $ O.info (templateSwapOptions O.<**> O.helper)
    $ O.progDesc "Template for a swap contract."


-- | Parser for the "contract-swap" options.
templateSwapOptions :: O.Parser Command
templateSwapOptions =
  TemplateSwap
    <$> O.option O.auto            (O.long "minimum-ada" <> O.metavar "INTEGER"     <> O.help "Lovelace that the first party contributes to the initial state.")
    <*> O.option parseParty        (O.long "a-party"     <> O.metavar "PARTY"       <> O.help "The first party."                                               )
    <*> O.option parseToken        (O.long "a-token"     <> O.metavar "TOKEN"       <> O.help "The first party's token."                                       )
    <*> O.option O.auto            (O.long "a-amount"    <> O.metavar "INTEGER"     <> O.help "The amount of the first party's token."                         )
    <*> O.option parseSlot         (O.long "a-timeout"   <> O.metavar "SLOT"        <> O.help "The timeout for the first party's deposit."                     )
    <*> O.option parseParty        (O.long "b-party"     <> O.metavar "PARTY"       <> O.help "The second party."                                              )
    <*> O.option parseToken        (O.long "b-token"     <> O.metavar "TOKEN"       <> O.help "The second party's token."                                      )
    <*> O.option O.auto            (O.long "b-amount"    <> O.metavar "INTEGER"     <> O.help "The amount of the second party's token."                        )
    <*> O.option parseSlot         (O.long "b-timeout"   <> O.metavar "SLOT"        <> O.help "The timeout for the second party's deposit."                    )
    <*> (O.optional . O.strOption) (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for contract input."                           )
