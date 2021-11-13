
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI (
  mainCLI
) where


import           Cardano.Api                 (AsType (AsStakeAddress), NetworkId (..), NetworkMagic (..), SlotNo (..),
                                              StakeAddressReference (..), deserialiseAddress)
import           Cardano.Api.Shelley         (StakeAddress (..), fromShelleyStakeCredential)
import           Data.Maybe                  (fromMaybe)
import           Data.Version                (Version, showVersion)
import           Language.Marlowe.CLI.Export (exportAddress, exportDatum, exportMarlowe, exportRedeemer,
                                              exportValidator)
import           Language.Marlowe.CLI.Types  (Command (..))
import           Language.Marlowe.Client     (defaultMarloweParams, marloweParams)
import           Plutus.V1.Ledger.Api        (CurrencySymbol (..), defaultCostModelParams, toBuiltin)

import qualified Data.ByteString.Base16      as Base16 (decode)
import qualified Data.ByteString.Char8       as BS8 (pack)
import qualified Data.Text                   as T (pack)
import qualified Options.Applicative         as O


mainCLI :: Version
        -> IO ()
mainCLI version =
  do
    command <- O.execParser $ parser version
    let
      marloweParams' = maybe defaultMarloweParams marloweParams $ rolesCurrency command
      network'       = fromMaybe Mainnet                        $ network       command
      stake'         = fromMaybe NoStakeAddress                 $ stake         command
    Just costModel <- pure defaultCostModelParams
    case command of
      Export{..}          -> exportMarlowe
                               marloweParams' costModel network' stake'
                               contractFile stateFile
                               inputsFile minimumSlot maximumSlot
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
                               inputsFile minimumSlot maximumSlot
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
              <> exportAddressCommand
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
    ("marlowe-cli " ++ showVersion version) -- FIXME: Include git hash.
    (O.long "version" <> O.help "Show version.")


exportMarloweCommand :: O.Mod O.CommandFields Command
exportMarloweCommand =
  O.command "export"
    $ O.info (exportMarloweOptions O.<**> O.helper)
    $ O.progDesc "Export a Marlowe contract to a JSON file."


exportMarloweOptions :: O.Parser Command
exportMarloweOptions =
  Export
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"     <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."         )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"     <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                      )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency"    <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any."      )
    <*> O.strOption                                        (O.long "contract-file"     <> O.metavar "CONTRACT_FILE"   <> O.help "JSON input file for the contract."           )
    <*> O.strOption                                        (O.long "state-file"        <> O.metavar "STATE_FILE"      <> O.help "JSON input file for the contract state."     )
    <*> (O.optional . O.strOption)                         (O.long "inputs-file"       <> O.metavar "INPUTS_FILE"     <> O.help "JSON input file for redeemer inputs, if any.")
    <*> O.option parseSlotNo                               (O.long "redeemer-min-slot" <> O.metavar "SLOT_NUMBER"     <> O.help "Minimum slot for the redemption."            )
    <*> O.option parseSlotNo                               (O.long "redeemer-max-slot" <> O.metavar "SLOT_NUMBER"     <> O.help "Maximum slot for the redemption."            )
    <*> O.strOption                                        (O.long "out-file"          <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for contract."              )
    <*> O.switch                                           (O.long "print-stats"                                      <> O.help "Print statistics."                           )


exportAddressCommand :: O.Mod O.CommandFields Command
exportAddressCommand =
  O.command "address"
    . O.info (exportAddressOptions O.<**> O.helper)
    $ O.progDesc "Print a validator address."


exportAddressOptions :: O.Parser Command
exportAddressOptions =
  ExportAddress
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."   )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any.")


exportValidatorCommand :: O.Mod O.CommandFields Command
exportValidatorCommand =
  O.command "validator"
    . O.info (exportValidatorOptions O.<**> O.helper)
    $ O.progDesc "Export a validator to a JSON file."


exportValidatorOptions :: O.Parser Command
exportValidatorOptions =
  ExportValidator
    <$> (O.optional . O.option parseNetworkId)             (O.long "testnet-magic"  <> O.metavar "INTEGER"         <> O.help "Network magic, or omit for mainnet."   )
    <*> (O.optional . O.option parseStakeAddressReference) (O.long "stake-address"  <> O.metavar "ADDRESS"         <> O.help "Stake address, if any."                )
    <*> (O.optional . O.option parseCurrencySymbol)        (O.long "roles-currency" <> O.metavar "CURRENCY_SYMBOL" <> O.help "The currency symbol for roles, if any.")
    <*> O.strOption                                        (O.long "out-file"       <> O.metavar "OUTPUT_FILE"     <> O.help "JSON output file for validator."       )
    <*> O.switch                                           (O.long "print-hash"                                    <> O.help "Print validator hash."                 )
    <*> O.switch                                           (O.long "print-stats"                                   <> O.help "Print statistics."                     )


exportDatumCommand :: O.Mod O.CommandFields Command
exportDatumCommand =
  O.command "datum"
    . O.info (exportDatumOptions O.<**> O.helper)
    $ O.progDesc "Export a datum to a JSON file."


exportDatumOptions :: O.Parser Command
exportDatumOptions =
  ExportDatum
    <$> O.strOption (O.long "contract-file" <> O.metavar "CONTRACT_FILE" <> O.help "JSON input file for the contract."      )
    <*> O.strOption (O.long "state-file"    <> O.metavar "STATE_FILE"    <> O.help "JSON input file for the contract state.")
    <*> O.strOption (O.long "out-file"      <> O.metavar "DATUM_FILE"    <> O.help "JSON output file for datum."            )
    <*> O.switch    (O.long "print-stats"                                <> O.help "Print statistics."                      )


exportRedeemerCommand :: O.Mod O.CommandFields Command
exportRedeemerCommand =
  O.command "redeemer"
    . O.info (exportRedeemerOptions O.<**> O.helper)
    $ O.progDesc "Export a redeemer to a JSON file."


exportRedeemerOptions :: O.Parser Command
exportRedeemerOptions =
  ExportRedeemer
    <$> (O.optional . O.strOption) (O.long "inputs-file" <> O.metavar "INPUTS_FILE" <> O.help "JSON input file for redeemer inputs, if any.")
    <*> O.option parseSlotNo       (O.long "min-slot"    <> O.metavar "SLOT_NUMBER" <> O.help "Minimum slot for the redemption."            )
    <*> O.option parseSlotNo       (O.long "max-slot"    <> O.metavar "SLOT_NUMBER" <> O.help "Maximum slot for the redemption."            )
    <*> O.strOption                (O.long "out-file"    <> O.metavar "OUTPUT_FILE" <> O.help "JSON output file for redeemer."              )
    <*> O.switch                   (O.long "print-stats"                            <> O.help "Print statistics."                           )


parseNetworkId :: O.ReadM NetworkId
parseNetworkId = Testnet . NetworkMagic . toEnum <$> O.auto


parseStakeAddressReference :: O.ReadM StakeAddressReference
parseStakeAddressReference =
  O.eitherReader
    $ \s ->
      case deserialiseAddress AsStakeAddress $ T.pack s of
        Nothing                          -> Left "Invalid stake address."
        Just (StakeAddress _ credential) -> Right . StakeAddressByValue $ fromShelleyStakeCredential credential


parseSlotNo :: O.ReadM SlotNo
parseSlotNo = SlotNo <$> O.auto


parseCurrencySymbol :: O.ReadM CurrencySymbol
parseCurrencySymbol =
  O.eitherReader
    $ \s ->
      case Base16.decode $ BS8.pack s of
        Left  message  -> Left message
        Right currency -> Right . CurrencySymbol . toBuiltin $ currency
