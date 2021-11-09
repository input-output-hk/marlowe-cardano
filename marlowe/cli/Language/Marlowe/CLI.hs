
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI (
  mainCLI
) where


import           Data.Version                (Version, showVersion)
import           Language.Marlowe.CLI.Export (exportDatum, exportMarlowe, exportRedeemer, exportValidator)
import           Language.Marlowe.CLI.Types  (Command (..))

import qualified Options.Applicative         as O


mainCLI :: Version
        -> IO ()
mainCLI version =
  do
    command <- O.execParser $ parser version
    case command of
      Export{}            -> exportMarlowe -- FIXME: This is a placeholder to access the original version of the code.
      ExportValidator{..} -> exportValidator (toEnum <$> magic) stake validatorFile printAddress printHash printStats
      ExportDatum{..}     -> exportDatum accountHash accountLovelace minimumSlot datumFile printHash printStats
      ExportRedeemer{..}  -> exportRedeemer minimumSlot maximumSlot redeemerFile printHash printStats


parser :: Version
       -> O.ParserInfo Command
parser version =
  O.info
    (
          O.helper
      <*> versionOption version
      <*> O.subparser
            (
                 exportCommand
              <> exportValidatorCommand
              <> exportDatumCommand
              <> exportRedeemerCommand
            )
    )
    (
         O.fullDesc
      <> O.progDesc "Utilities for Marlowe."
      <> O.header "marlowe-cli"
    )


versionOption :: Version
              -> O.Parser (a -> a)
versionOption version =
  O.infoOption
    ("marlowe-cli " ++ showVersion version)
    (O.long "version" <> O.help "Show version.")


exportCommand :: O.Mod O.CommandFields Command
exportCommand =
  O.command "export"
    $ O.info (pure Export O.<**> O.helper)
    $ O.progDesc "Export a script, datum, and redeemer to a JSON file."


exportValidatorCommand :: O.Mod O.CommandFields Command
exportValidatorCommand =
  O.command "validator"
    . O.info (exportValidatorOptions O.<**> O.helper)
    $ O.progDesc "Export a validator to a JSON file."


exportValidatorOptions :: O.Parser Command
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option O.auto) (O.long "network-magic"  <> O.metavar "PUB_KEY_HASH" <> O.help "Network magic, or omit for mainnet." )
    <*> (O.optional . O.option O.auto) (O.long "stake-address"  <> O.metavar "ADDRESS"      <> O.help "Stake address, if any."              )
    <*> O.strOption                    (O.long "validator-file" <> O.metavar "OUTPUT_FILE"  <> O.help "JSON output file."                   )
    <*> O.switch                       (O.long "print-address"                              <> O.help "Print validator address."            )
    <*> O.switch                       (O.long "print-hash"                                 <> O.help "Print validator hash."               )
    <*> O.switch                       (O.long "print-stats"                                <> O.help "Print statistics."                   )


exportDatumCommand :: O.Mod O.CommandFields Command
exportDatumCommand =
  O.command "datum"
    . O.info (exportDatumOptions O.<**> O.helper)
    $ O.progDesc "Export a datum to a JSON file."


exportDatumOptions :: O.Parser Command
exportDatumOptions =
  ExportDatum
    <$> O.strOption     (O.long "account-hash"  <> O.metavar "PUB_KEY_HASH" <> O.help "Public key hash for the account."    )
    <*> O.option O.auto (O.long "account-value" <> O.metavar "LOVELACE"     <> O.help "Lovelace value for the account."     )
    <*> O.option O.auto (O.long "min-slot"      <> O.metavar "MIN_SLOT"     <> O.help "Minimum slot for the contract state.")
    <*> O.strOption     (O.long "datum-file"    <> O.metavar "OUTPUT_FILE"  <> O.help "JSON output file."                   )
    <*> O.switch        (O.long "print-hash"                                <> O.help "Print datum hash."                   )
    <*> O.switch        (O.long "print-stats"                               <> O.help "Print statistics."                   )


exportRedeemerCommand :: O.Mod O.CommandFields Command
exportRedeemerCommand =
  O.command "redeemer"
    . O.info (exportRedeemerOptions O.<**> O.helper)
    $ O.progDesc "Export a redeemer to a JSON file."


exportRedeemerOptions :: O.Parser Command
exportRedeemerOptions =
  ExportRedeemer
    <$> O.option O.auto (O.long "min-slot"      <> O.metavar "SLOT_NUMBER" <> O.help "Minimum slot for the redemption.")
    <*> O.option O.auto (O.long "max-slot"      <> O.metavar "SLOT_NUMBER" <> O.help "Maximum slot for the redemption.")
    <*> O.strOption     (O.long "redeemer-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file."               )
    <*> O.switch        (O.long "print-hash"                               <> O.help "Print redeemer hash."            )
    <*> O.switch        (O.long "print-stats"                              <> O.help "Print statistics."               )
