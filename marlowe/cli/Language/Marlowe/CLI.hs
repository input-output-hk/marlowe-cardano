
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI (
  mainCLI
) where


import           Cardano.Api                 (AsType (AsStakeAddress), Lovelace, NetworkId (..), NetworkMagic (..),
                                              Quantity (..), SlotNo (..), StakeAddressReference (..),
                                              deserialiseAddress, quantityToLovelace)
import           Cardano.Api.Shelley         (StakeAddress (..), fromShelleyStakeCredential)
import           Data.Maybe                  (fromMaybe)
import           Data.Version                (Version, showVersion)
import           Language.Marlowe.CLI.Export (exportDatum, exportRedeemer, exportValidator)
import           Language.Marlowe.CLI.Types  (Command (..))

import qualified Data.Text                   as T (pack)
import qualified Options.Applicative         as O


mainCLI :: Version
        -> IO ()
mainCLI version =
  do
    command <- O.execParser $ parser version
    case command of
      Export{..}          -> do
                               exportValidator
                                 (fromMaybe Mainnet network)
                                 (fromMaybe NoStakeAddress stake)
                                 validatorFile
                                 printAddress printHash printStats
                               exportDatum
                                 accountHash accountLovelace minimumSlot'
                                 datumFile
                                 printHash printStats
                               exportRedeemer
                                 minimumSlot maximumSlot
                                 redeemerFile
                                 printStats
      ExportValidator{..} -> exportValidator
                               (fromMaybe Mainnet network)
                               (fromMaybe NoStakeAddress stake)
                               validatorFile
                               printAddress printHash printStats
      ExportDatum{..}     -> exportDatum
                               accountHash accountLovelace minimumSlot'
                               datumFile
                               printHash printStats
      ExportRedeemer{..}  -> exportRedeemer
                               minimumSlot maximumSlot
                               redeemerFile
                               printStats


parser :: Version
       -> O.ParserInfo Command
parser version =
  O.info
    (
          O.helper
      <*> versionOption version
      <*> O.subparser
            (
                 exportMarloweCommand
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


exportMarloweCommand :: O.Mod O.CommandFields Command
exportMarloweCommand =
  O.command "export"
    $ O.info (exportMarloweOptions O.<**> O.helper)
    $ O.progDesc "Export a script, datum, and redeemer to a JSON file."


exportMarloweOptions :: O.Parser Command
exportMarloweOptions =
  Export
    <$> (O.optional . O.option parseNetworkId)             (O.long "network-magic"        <> O.metavar "INTEGER"      <> O.help "Network magic, or omit for mainnet." )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"        <> O.metavar "ADDRESS"      <> O.help "Stake address, if any."              )
    <*> O.strOption                                        (O.long "validator-file"       <> O.metavar "OUTPUT_FILE"  <> O.help "JSON output file for validator."     )
    <*> O.switch                                           (O.long "print-address"                                    <> O.help "Print validator address."            )
    <*> O.strOption                                        (O.long "datum-account-hash"   <> O.metavar "PUB_KEY_HASH" <> O.help "Public key hash for the account."    )
    <*> O.option parseLovelace                             (O.long "datum-account-value"  <> O.metavar "LOVELACE"     <> O.help "Lovelace value for the account."     )
    <*> O.option parseSlotNo                               (O.long "datum-min-slot"       <> O.metavar "MIN_SLOT"     <> O.help "Minimum slot for the contract state.")
    <*> O.strOption                                        (O.long "datum-file"           <> O.metavar "OUTPUT_FILE"  <> O.help "JSON output file for datum."         )
    <*> O.option parseSlotNo                               (O.long "redeemer-min-slot"    <> O.metavar "SLOT_NUMBER"  <> O.help "Minimum slot for the redemption."    )
    <*> O.option parseSlotNo                               (O.long "redeemer-max-slot"    <> O.metavar "SLOT_NUMBER"  <> O.help "Maximum slot for the redemption."    )
    <*> O.strOption                                        (O.long "redeemer-file"        <> O.metavar "OUTPUT_FILE"  <> O.help "JSON output file for redeemer."      )
    <*> O.switch                                           (O.long "print-hash"                                       <> O.help "Print hashes."                       )
    <*> O.switch                                           (O.long "print-stats"                                      <> O.help "Print statistics."                   )


exportValidatorCommand :: O.Mod O.CommandFields Command
exportValidatorCommand =
  O.command "validator"
    . O.info (exportValidatorOptions O.<**> O.helper)
    $ O.progDesc "Export a validator to a JSON file."


exportValidatorOptions :: O.Parser Command
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option parseNetworkId)             (O.long "network-magic"  <> O.metavar "INTEGER"     <> O.help "Network magic, or omit for mainnet." )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"     <> O.help "Stake address, if any."              )
    <*> O.strOption                                        (O.long "validator-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for validator."     )
    <*> O.switch                                           (O.long "print-address"                             <> O.help "Print validator address."            )
    <*> O.switch                                           (O.long "print-hash"                                <> O.help "Print validator hash."               )
    <*> O.switch                                           (O.long "print-stats"                               <> O.help "Print statistics."                   )


exportDatumCommand :: O.Mod O.CommandFields Command
exportDatumCommand =
  O.command "datum"
    . O.info (exportDatumOptions O.<**> O.helper)
    $ O.progDesc "Export a datum to a JSON file."


exportDatumOptions :: O.Parser Command
exportDatumOptions =
  ExportDatum
    <$> O.strOption            (O.long "account-hash"  <> O.metavar "PUB_KEY_HASH" <> O.help "Public key hash for the account."    )
    <*> O.option parseLovelace (O.long "account-value" <> O.metavar "LOVELACE"     <> O.help "Lovelace value for the account."     )
    <*> O.option parseSlotNo   (O.long "min-slot"      <> O.metavar "MIN_SLOT"     <> O.help "Minimum slot for the contract state.")
    <*> O.strOption            (O.long "datum-file"    <> O.metavar "OUTPUT_FILE"  <> O.help "JSON output file for datum."         )
    <*> O.switch               (O.long "print-hash"                                <> O.help "Print datum hash."                   )
    <*> O.switch               (O.long "print-stats"                               <> O.help "Print statistics."                   )


exportRedeemerCommand :: O.Mod O.CommandFields Command
exportRedeemerCommand =
  O.command "redeemer"
    . O.info (exportRedeemerOptions O.<**> O.helper)
    $ O.progDesc "Export a redeemer to a JSON file."


exportRedeemerOptions :: O.Parser Command
exportRedeemerOptions =
  ExportRedeemer
    <$> O.option parseSlotNo (O.long "min-slot"      <> O.metavar "SLOT_NUMBER" <> O.help "Minimum slot for the redemption.")
    <*> O.option parseSlotNo (O.long "max-slot"      <> O.metavar "SLOT_NUMBER" <> O.help "Maximum slot for the redemption.")
    <*> O.strOption          (O.long "redeemer-file" <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer."  )
    <*> O.switch             (O.long "print-stats"                              <> O.help "Print statistics."               )


parseNetworkId :: O.ReadM NetworkId
parseNetworkId = Testnet . NetworkMagic . toEnum <$> O.auto


parseStakeAddressReference :: O.ReadM StakeAddressReference
parseStakeAddressReference =
  O.eitherReader
    $ \s ->
      case deserialiseAddress AsStakeAddress $ T.pack s of
        Nothing                          -> Left "Invalid stake address."
        Just (StakeAddress _ credential) -> Right . StakeAddressByValue $ fromShelleyStakeCredential credential


parseLovelace :: O.ReadM Lovelace
parseLovelace = quantityToLovelace . Quantity <$> O.auto


parseSlotNo :: O.ReadM SlotNo
parseSlotNo = SlotNo <$> O.auto
