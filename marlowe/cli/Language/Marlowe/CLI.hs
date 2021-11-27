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


import           Cardano.Api                      (ConsensusModeParams (CardanoModeParams), EpochSlots (..),
                                                   LocalNodeConnectInfo (..), NetworkId (..),
                                                   StakeAddressReference (..))
import           Control.Monad.Except             (ExceptT, liftIO, runExceptT, throwError)
import           Data.Maybe                       (fromMaybe)
import           Data.Version                     (Version, showVersion)
import           Language.Marlowe.CLI.Export      (exportAddress, exportDatum, exportMarlowe, exportRedeemer,
                                                   exportValidator)
import           Language.Marlowe.CLI.Parse       (parseAddressAny, parseCurrencySymbol, parseNetworkId, parseSlotNo,
                                                   parseStakeAddressReference, parseTxIn, parseTxOut, parseValue)
import           Language.Marlowe.CLI.Transaction (buildContinuing, buildIncoming, buildOutgoing, buildSimple, submit)
import           Language.Marlowe.CLI.Types       (CliError (..), Command (..))
import           Language.Marlowe.Client          (defaultMarloweParams, marloweParams)
import           Plutus.V1.Ledger.Api             (defaultCostModelParams)
import           System.Exit                      (exitFailure)
import           System.IO                        (hPutStrLn, stderr)

import qualified Options.Applicative              as O


-- | Hardwired example.
type Example = Bool -> ExceptT CliError IO ()


-- | Main entry point for Marlowe CLI tool.
mainCLI :: Version  -- ^ The version of the tool.
        -> Example  -- ^ Hardwired example.
        -> IO ()    -- ^ Action to run the tool.
mainCLI version example =
  do
    command <- O.execParser $ parser version
    let
      marloweParams' = maybe defaultMarloweParams marloweParams $ rolesCurrency command
      network'       = fromMaybe Mainnet                        $ network       command
      stake'         = fromMaybe NoStakeAddress                 $ stake         command
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network'
        , localNodeSocketPath      = socketPath command
        }
      printTxId = liftIO . putStrLn . ("TxId " ++) . show
    result <-
      runExceptT
        $ do
          costModel <-
            maybe
              (throwError "Missing default cost model.")
              pure
              defaultCostModelParams
          case command of
            Export{..}          -> exportMarlowe
                                     marloweParams' costModel network' stake'
                                     contractFile stateFile
                                     inputsFile
                                     outputFile
                                     printStats
            ExportAddress{}     -> exportAddress
                                     marloweParams' network' stake'
            ExportValidator{..} -> exportValidator
                                     marloweParams' costModel network' stake'
                                     validatorFile
                                     printHash printStats
            ExportDatum{..}     -> exportDatum
                                     contractFile stateFile
                                     datumFile
                                     printStats
            ExportRedeemer{..}  -> exportRedeemer
                                     inputsFile
                                     redeemerFile
                                     printStats
            BuildTransact{..}   -> buildSimple
                                     connection
                                     inputs outputs change
                                     bodyFile
                                     >>= printTxId
            BuildCreate{..}     -> buildIncoming
                                     connection
                                     scriptAddress
                                     outputDatumFile
                                     outputValue
                                     inputs outputs change
                                     bodyFile
                                     >>= printTxId
            BuildAdvance{..}    -> buildContinuing
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
                                     >>= printTxId
            BuildClose{..}      -> buildOutgoing
                                     connection
                                     validatorFile
                                     redeemerFile
                                     inputDatumFile
                                     signingKeyFiles
                                     inputTxIn
                                     inputs outputs collateral change
                                     minimumSlot maximumSlot
                                     bodyFile
                                     >>= printTxId
            Submit{..}          -> submit
                                     connection
                                     bodyFile
                                     signingKeyFiles
                                     >>= printTxId
            Example{..}         -> example writeFiles
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
              <> exampleCommand
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
    ("marlowe-cli " ++ showVersion version) -- FIXME: Include git hash.
    (O.long "version" <> O.help "Show version.")


-- | Parser for the "export" command.
exportMarloweCommand :: O.Mod O.CommandFields Command -- ^ The parser.
exportMarloweCommand =
  O.command "export"
    $ O.info (exportMarloweOptions O.<**> O.helper)
    $ O.progDesc "Export a Marlowe contract to a JSON file."


-- | Parser for the "export" options.
exportMarloweOptions :: O.Parser Command -- ^ The parser.
exportMarloweOptions =
  Export
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"     <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."         )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"     <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                      )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency"    <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."      )
    <*> O.strOption                                        (O.long "contract-file"     <> O.metavar "CONTRACT_FILE"   <> O.help "JSON input file for the contract."           )
    <*> O.strOption                                        (O.long "state-file"        <> O.metavar "STATE_FILE"      <> O.help "JSON input file for the contract state."     )
    <*> (O.optional . O.strOption)                         (O.long "inputs-file"       <> O.metavar "INPUTS_FILE"     <> O.help "JSON input file for redeemer inputs, if any.")
    <*> O.strOption                                        (O.long "out-file"          <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for contract."              )
    <*> O.switch                                           (O.long "print-stats"                                      <> O.help "Print statistics."                           )


-- | Parser for the "address" command.
exportAddressCommand :: O.Mod O.CommandFields Command
exportAddressCommand =
  O.command "address"
    . O.info (exportAddressOptions O.<**> O.helper)
    $ O.progDesc "Print a validator address."


-- | Parser for the "address" options.
exportAddressOptions :: O.Parser Command
exportAddressOptions =
  ExportAddress
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."   )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any.")


-- | Parser for the "validator" command.
exportValidatorCommand :: O.Mod O.CommandFields Command
exportValidatorCommand =
  O.command "validator"
    . O.info (exportValidatorOptions O.<**> O.helper)
    $ O.progDesc "Export a validator to a JSON file."


-- | Parser for the "validator" options.
exportValidatorOptions :: O.Parser Command
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."   )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any.")
    <*> O.strOption                                        (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for validator."       )
    <*> O.switch                                           (O.long "print-hash"                                    <> O.help "Print validator hash."                 )
    <*> O.switch                                           (O.long "print-stats"                                   <> O.help "Print statistics."                     )


-- | Parser for the "datum" command.
exportDatumCommand :: O.Mod O.CommandFields Command
exportDatumCommand =
  O.command "datum"
    . O.info (exportDatumOptions O.<**> O.helper)
    $ O.progDesc "Export a datum to a JSON file."


-- | Parser for the "datum" options.
exportDatumOptions :: O.Parser Command
exportDatumOptions =
  ExportDatum
    <$> O.strOption (O.long "contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract."      )
    <*> O.strOption (O.long "state-file"    <> O.metavar "STATE_FILE"    <> O.help "JSON input file for the contract state.")
    <*> O.strOption (O.long "out-file"      <> O.metavar "DATUM_FILE"    <> O.help "JSON output file for datum."            )
    <*> O.switch    (O.long "print-stats"                                <> O.help "Print statistics."                      )


-- | Parser for the "redeemer" command.
exportRedeemerCommand :: O.Mod O.CommandFields Command
exportRedeemerCommand =
  O.command "redeemer"
    . O.info (exportRedeemerOptions O.<**> O.helper)
    $ O.progDesc "Export a redeemer to a JSON file."


-- | Parser for the "redeemer" options.
exportRedeemerOptions :: O.Parser Command
exportRedeemerOptions =
  ExportRedeemer
    <$> (O.optional . O.strOption) (O.long "inputs-file" <> O.metavar "INPUTS_FILE" <> O.help "JSON input file for redeemer inputs, if any.")
    <*> O.strOption                (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer."              )
    <*> O.switch                   (O.long "print-stats"                            <> O.help "Print statistics."                           )


-- | Parser for the "transact" command.
buildSimpleCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildSimpleCommand =
  O.command "transact"
    $ O.info (buildSimpleOptions O.<**> O.helper)
    $ O.progDesc "Build a non-Marlowe transaction."


-- | Parser for the "transact" options.
buildSimpleOptions :: O.Parser Command -- ^ The parser.
buildSimpleOptions =
  BuildTransact
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"  <> O.metavar "INTEGER"          <> O.help "Network magic, or omit for mainnet."           )
    <*> O.strOption                            (O.long "socket-path"    <> O.metavar "SOCKET_FILE"      <> O.help "Location of the cardano-node socket file."     )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"          <> O.metavar "TXID#TXIX"        <> O.help "Transaction input in TxId#TxIx format."        )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"         <> O.metavar "ADDRESS+LOVELACE" <> O.help "Transaction output in ADDRESS+LOVELACE format.")
    <*> O.option parseAddressAny               (O.long "change-address" <> O.metavar "ADDRESS"          <> O.help "Address to receive ADA in excess of fee."      )
    <*> O.strOption                            (O.long "out-file"       <> O.metavar "FILE"             <> O.help "Output file for transaction body."             )


-- | Parser for the "create" command.
buildIncomingCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildIncomingCommand =
  O.command "create"
    $ O.info (buildIncomingOptions O.<**> O.helper)
    $ O.progDesc "Build a transaction that pays to a Marlowe script."


-- | Parser for the "create" options.
buildIncomingOptions :: O.Parser Command -- ^ The parser.
buildIncomingOptions =
  BuildCreate
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"     <> O.metavar "INTEGER"          <> O.help "Network magic, or omit for mainnet."            )
    <*> O.strOption                            (O.long "socket-path"       <> O.metavar "SOCKET_FILE"      <> O.help "Location of the cardano-node socket file."      )
    <*> O.option parseAddressAny               (O.long "script-address"    <> O.metavar "ADDRESS"          <> O.help "Address of the Marlowe contract."               )
    <*> O.strOption                            (O.long "tx-out-datum-file" <> O.metavar "DATUM_FILE"       <> O.help "Datum JSON file datum paid to Marlowe contract.")
    <*> O.option parseValue                    (O.long "tx-out-value"      <> O.metavar "LOVELACE"         <> O.help "Lovelace value paid to Marlowe contract."       )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"             <> O.metavar "TXID#TXIX"        <> O.help "Transaction input in TxId#TxIx format."         )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"            <> O.metavar "ADDRESS+LOVELACE" <> O.help "Transaction output in ADDRESS+LOVELACE format." )
    <*> O.option parseAddressAny               (O.long "change-address"    <> O.metavar "ADDRESS"          <> O.help "Address to receive ADA in excess of fee."       )
    <*> O.strOption                            (O.long "out-file"          <> O.metavar "FILE"             <> O.help "Output file for transaction body."              )


-- | Parser for the "advance" command.
buildContinuingCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildContinuingCommand =
  O.command "advance"
    $ O.info (buildContinuingOptions O.<**> O.helper)
    $ O.progDesc "Build a transaction that both spends from and pays to a Marlowe script."


-- | Parser for the "advance" options.
buildContinuingOptions :: O.Parser Command -- ^ The parser.
buildContinuingOptions =
  BuildAdvance
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"       <> O.metavar "INTEGER"          <> O.help "Network magic, or omit for mainnet."            )
    <*> O.strOption                            (O.long "socket-path"         <> O.metavar "SOCKET_FILE"      <> O.help "Location of the cardano-node socket file."      )
    <*> O.option parseAddressAny               (O.long "script-address"      <> O.metavar "ADDRESS"          <> O.help "Address of the Marlowe contract."               )
    <*> O.strOption                            (O.long "tx-in-script-file"   <> O.metavar "PLUTUS_FILE"      <> O.help "Plutus file for Marlowe contract."              )
    <*> O.strOption                            (O.long "tx-in-redeemer-file" <> O.metavar "REDEEMER_FILE"    <> O.help "Redeemer JSON file spent from Marlowe contract.")
    <*> O.strOption                            (O.long "tx-in-datum-file"    <> O.metavar "DATUM_FILE"       <> O.help "Datum JSON file spent from Marlowe contract."   )
    <*> (O.many . O.strOption)                 (O.long "required-signer"     <> O.metavar "SIGNING_FILE"     <> O.help "Files containing required signing keys."        )
    <*> O.option parseTxIn                     (O.long "tx-in-marlowe"       <> O.metavar "TXID#TXIX"        <> O.help "UTxO spent from Marlowe contract."              )
    <*> O.strOption                            (O.long "tx-out-datum-file"   <> O.metavar "DATUM_FILE"       <> O.help "Datum JSON file datum paid to Marlowe contract.")
    <*> O.option parseValue                    (O.long "tx-out-value"        <> O.metavar "LOVELACE"         <> O.help "Lovelace value paid to Marlowe contract."       )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"               <> O.metavar "TXID#TXIX"        <> O.help "Transaction input in TxId#TxIx format."         )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"              <> O.metavar "ADDRESS+LOVELACE" <> O.help "Transaction output in ADDRESS+LOVELACE format." )
    <*> O.option parseTxIn                     (O.long "tx-in-collateral"    <> O.metavar "TXID#TXIX"        <> O.help "Collateral for transaction."                    )
    <*> O.option parseAddressAny               (O.long "change-address"      <> O.metavar "ADDRESS"          <> O.help "Address to receive ADA in excess of fee."       )
    <*> O.option parseSlotNo                   (O.long "invalid-before"      <> O.metavar "SLOT"             <> O.help "Minimum slot for the redemption."               )
    <*> O.option parseSlotNo                   (O.long "invalid-hereafter"   <> O.metavar "SLOT"             <> O.help "Maximum slot for the redemption."               )
    <*> O.strOption                            (O.long "out-file"            <> O.metavar "FILE"             <> O.help "Output file for transaction body."              )


-- | Parser for the "close" command.
buildOutgoingCommand :: O.Mod O.CommandFields Command -- ^ The parser.
buildOutgoingCommand =
  O.command "close"
    $ O.info (buildOutgoingOptions O.<**> O.helper)
    $ O.progDesc "Build a transaction that spends from a Marlowe script."


-- | Parser for the "close" options.
buildOutgoingOptions :: O.Parser Command -- ^ The parser.
buildOutgoingOptions =
  BuildClose
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"       <> O.metavar "INTEGER"          <> O.help "Network magic, or omit for mainnet."            )
    <*> O.strOption                            (O.long "socket-path"         <> O.metavar "SOCKET_FILE"      <> O.help "Location of the cardano-node socket file."      )
    <*> O.strOption                            (O.long "tx-in-script-file"   <> O.metavar "PLUTUS_FILE"      <> O.help "Plutus file for Marlowe contract."              )
    <*> O.strOption                            (O.long "tx-in-redeemer-file" <> O.metavar "REDEEMER_FILE"    <> O.help "Redeemer JSON file spent from Marlowe contract.")
    <*> O.strOption                            (O.long "tx-in-datum-file"    <> O.metavar "DATUM_FILE"       <> O.help "Datum JSON file spent from Marlowe contract."   )
    <*> (O.many . O.strOption)                 (O.long "required-signer"     <> O.metavar "SIGNING_FILE"     <> O.help "Files containing required signing keys."        )
    <*> O.option parseTxIn                     (O.long "tx-in-marlowe"       <> O.metavar "TXID#TXIX"        <> O.help "UTxO spent from Marlowe contract."              )
    <*> (O.many . O.option parseTxIn)          (O.long "tx-in"               <> O.metavar "TXID#TXIX"        <> O.help "Transaction input in TxId#TxIx format."         )
    <*> (O.many . O.option parseTxOut)         (O.long "tx-out"              <> O.metavar "ADDRESS+LOVELACE" <> O.help "Transaction output in ADDRESS+LOVELACE format." )
    <*> O.option parseTxIn                     (O.long "tx-in-collateral"    <> O.metavar "TXID#TXIX"        <> O.help "Collateral for transaction."                    )
    <*> O.option parseAddressAny               (O.long "change-address"      <> O.metavar "ADDRESS"          <> O.help "Address to receive ADA in excess of fee."       )
    <*> O.option parseSlotNo                   (O.long "invalid-before"      <> O.metavar "SLOT"             <> O.help "Minimum slot for the redemption."               )
    <*> O.option parseSlotNo                   (O.long "invalid-hereafter"   <> O.metavar "SLOT"             <> O.help "Maximum slot for the redemption."               )
    <*> O.strOption                            (O.long "out-file"            <> O.metavar "FILE"             <> O.help "Output file for transaction body."              )


-- | Parser for the "submit" command.
submitCommand :: O.Mod O.CommandFields Command -- ^ The parser.
submitCommand =
  O.command "submit"
    $ O.info (submitOptions O.<**> O.helper)
    $ O.progDesc "Submit a transaction body."


-- | Parser for the "submit" options.
submitOptions :: O.Parser Command
submitOptions =
  Submit
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"   <> O.metavar "INTEGER"      <> O.help "Network magic, or omit for mainnet."            )
    <*> O.strOption                            (O.long "socket-path"     <> O.metavar "SOCKET_FILE"  <> O.help "Location of the cardano-node socket file."      )
    <*> O.strOption                            (O.long "tx-body-file"    <> O.metavar "BODY_FILE"    <> O.help "File containing the transaction body."  )
    <*> (O.many . O.strOption)                 (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "Files containing required signing keys.")


-- | Parser for the "example" command.
exampleCommand :: O.Mod O.CommandFields Command -- ^ The parser.
exampleCommand =
  O.command "example"
    $ O.info (exampleOptions O.<**> O.helper)
    $ O.progDesc "Hardwired example."


-- | Parser for the "example" options.
exampleOptions :: O.Parser Command
exampleOptions =
  Example
    <$> O.switch (O.long "write-files" <> O.help "Write example JSON files for states, contracts, and inputs.")
