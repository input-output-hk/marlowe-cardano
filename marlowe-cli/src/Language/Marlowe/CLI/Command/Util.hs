-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Miscellaneous utilities in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards    #-}


module Language.Marlowe.CLI.Command.Util (
-- * Marlowe CLI Commands
  UtilCommand(..)
, parseUtilCommand
, runUtilCommand
) where


import Cardano.Api (AddressAny, ConsensusModeParams (CardanoModeParams), EpochSlots (..), LocalNodeConnectInfo (..),
                    Lovelace (..), NetworkId (..), SlotNo, TxMetadataInEra (TxMetadataNone), TxMintValue (TxMintNone),
                    lovelaceToValue)
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Codec (decodeBech32, encodeBech32)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseNetworkId, parseOutputQuery, parseSlotNo,
                                           parseTokenName)
import Language.Marlowe.CLI.Transaction (buildClean, buildFaucet', buildMinting, querySlotting, selectUtxos)
import Language.Marlowe.CLI.Types (CliError, OutputQuery)
import Plutus.V1.Ledger.Api (TokenName)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options.
data UtilCommand =
    -- | Clean UTxOs at an address.
    Clean
    {
      network         :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath      :: FilePath         -- ^ The path to the node socket.
    , signingKeyFiles :: [FilePath]       -- ^ The files containing the required signing keys.
    , lovelace        :: Lovelace         -- ^ The lovelace to send with each bundle of tokens.
    , change          :: AddressAny       -- ^ The change address.
    , bodyFile        :: FilePath         -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int        -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    }
    -- | Mint tokens.
  | Mint
    {
      network        :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath     :: FilePath         -- ^ The path to the node socket.
    , signingKeyFile :: FilePath         -- ^ The files containing the required signing keys.
    , metadataFile   :: Maybe FilePath   -- ^ The CIP-25 metadata for the minting, with keys for each token name.
    , count          :: Integer          -- ^ The number of each token to mint.
    , expires        :: Maybe SlotNo     -- ^ The slot number after which minting is no longer possible.
    , lovelace       :: Lovelace         -- ^ The lovelace to send with each token.
    , change         :: AddressAny       -- ^ The change address.
    , bodyFile       :: FilePath         -- ^ The output file for the transaction body.
    , submitTimeout  :: Maybe Int        -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , tokenNames     :: [TokenName]      -- ^ The token names.
    }
    -- | Fund an address from a faucet.
  | Faucet
    {
      network       :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath    :: FilePath         -- ^ The path to the node socket.
    , lovelace      :: Lovelace         -- ^ The lovelace to send to the address.
    , bodyFile      :: FilePath         -- ^ The output file for the transaction body.
    , submitTimeout :: Maybe Int        -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , addresses     :: [AddressAny]     -- ^ The addresses.
    }
    -- | Select UTxO by asset.
  | Output
    {
      network    :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath :: FilePath         -- ^ The path to the node socket.
    , query      :: OutputQuery      -- ^ Filter the query results.
    , address    :: AddressAny       -- ^ The addresses.
    }
    -- | Decode Bech32.
  | DecodeBech32
    {
      content :: String  -- ^ The Bech32 encoded data.
    }
    -- | Encode Bech32.
  | EncodeBech32
    {
      prefix  :: String  -- ^ The Bech32 prefix.
    , content :: String  -- ^ The base16-encoded bytes to be encoded in Bech32.
    }
    -- | Extract slot configuation.
  | Slotting
    {
      network      :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath   :: FilePath         -- ^ The path to the node socket.
    , slottingFile :: Maybe FilePath   -- ^ The output file for the slot configuration.
    }


-- | Run a miscellaneous command.
runUtilCommand :: MonadError CliError m
               => MonadIO m
               => UtilCommand  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runUtilCommand command =
  do
    let
      network' = fromMaybe Mainnet $ network command
      connection =
        LocalNodeConnectInfo
        {
          localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
        , localNodeNetworkId       = network'
        , localNodeSocketPath      = socketPath command
        }
      printTxId = liftIO . putStrLn . ("TxId " <>) . show
    case command of
      Clean{..}        -> buildClean
                            connection
                            signingKeyFiles
                            lovelace
                            change
                            Nothing
                            TxMintNone
                            TxMetadataNone
                            bodyFile
                            submitTimeout
                            >>= printTxId
      Mint{..}         -> buildMinting
                            connection
                            signingKeyFile
                            tokenNames
                            metadataFile
                            count
                            expires
                            lovelace
                            change
                            bodyFile
                            submitTimeout
      Faucet{..}       -> buildFaucet'
                            connection
                            (lovelaceToValue lovelace)
                            addresses
                            bodyFile
                            submitTimeout
                            >>= printTxId
      Output{..}       -> selectUtxos
                            connection
                            address
                            query
      DecodeBech32{..} -> decodeBech32
                            content
      EncodeBech32{..} -> encodeBech32
                            prefix
                            content
      Slotting{..}     -> querySlotting
                            connection
                            slottingFile


-- | Parser for miscellaneous commands.
parseUtilCommand :: O.Parser UtilCommand
parseUtilCommand =
  O.hsubparser
    $ O.commandGroup "Miscellaneous low-level commands:"
    <> cleanCommand
    <> decodeBechCommand
    <> encodeBechCommand
    <> faucetCommand
    <> mintCommand
    <> selectCommand
    <> slottingCommand


-- | Parser for the "clean" command.
cleanCommand :: O.Mod O.CommandFields UtilCommand
cleanCommand =
  O.command "clean"
    $ O.info cleanOptions
    $ O.progDesc "Reorganize the UTxOs at an address, separating tokens."


-- | Parser for the "clean" options.
cleanOptions :: O.Parser UtilCommand
cleanOptions =
  Clean
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"                        <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"                          <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."              )
    <*> (O.some . O.strOption)                 (O.long "required-signer"                      <> O.metavar "SIGNING_FILE"  <> O.help "File containing a required signing key."                )
    <*> (O.option $ Lovelace <$> O.auto)       (O.long "lovelace"        <> O.value 2_000_000 <> O.metavar "LOVELACE"      <> O.help "The lovelace to send with each bundle of tokens."       )
    <*> O.option parseAddressAny               (O.long "change-address"                       <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."               )
    <*> O.strOption                            (O.long "out-file"                             <> O.metavar "FILE"          <> O.help "Output file for transaction body."                      )
    <*> (O.optional . O.option O.auto)         (O.long "submit"                               <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation.")


-- | Parser for the "mint" command.
mintCommand :: O.Mod O.CommandFields UtilCommand
mintCommand =
  O.command "mint"
    $ O.info mintOptions
    $ O.progDesc "Mint native tokens."


-- | Parser for the "mint" options.
mintOptions :: O.Parser UtilCommand
mintOptions =
  Mint
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"                         <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                        )
    <*> O.strOption                            (O.long "socket-path"                           <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."                  )
    <*> O.strOption                            (O.long "required-signer"                       <> O.metavar "SIGNING_FILE"  <> O.help "File containing a required signing key."                    )
    <*> (O.optional . O.strOption)             (O.long "metadata-file"                         <> O.metavar "JSON_FILE"     <> O.help "The CIP-25 metadata, with keys for each token name."        )
    <*> O.option O.auto                        (O.long "count"           <> O.value 1          <> O.metavar "INTEGER"       <> O.help "The number of each token to mint."                          )
    <*> (O.optional . O.option parseSlotNo)    (O.long "expires"                               <> O.metavar "SLOT_NO"       <> O.help "The slot number after which miniting is no longer possible.")
    <*> (O.option $ Lovelace <$> O.auto)       (O.long "lovelace"        <> O.value 10_000_000 <> O.metavar "LOVELACE"      <> O.help "The lovelace to send with each bundle of tokens."           )
    <*> O.option parseAddressAny               (O.long "change-address"                        <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."                   )
    <*> O.strOption                            (O.long "out-file"                              <> O.metavar "FILE"          <> O.help "Output file for transaction body."                          )
    <*> (O.optional . O.option O.auto)         (O.long "submit"                                <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation."    )
    <*> O.some (O.argument parseTokenName      $                                                  O.metavar "TOKEN_NAME"    <> O.help "The name of the token."                                     )


-- | Parser for the "faucet" command.
faucetCommand :: O.Mod O.CommandFields UtilCommand
faucetCommand =
  O.command "faucet"
    $ O.info faucetOptions
    $ O.progDesc "Fund an address from a faucet."


-- | Parser for the "faucet" options.
faucetOptions :: O.Parser UtilCommand
faucetOptions =
  Faucet
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"                          <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                        )
    <*> O.strOption                            (O.long "socket-path"                            <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."                  )
    <*> (O.option $ Lovelace <$> O.auto)       (O.long "lovelace"      <> O.value 1_000_000_000 <> O.metavar "LOVELACE"      <> O.help "The lovelace to send to each address."                      )
    <*> O.strOption                            (O.long "out-file"                               <> O.metavar "FILE"          <> O.help "Output file for transaction body."                          )
    <*> (O.optional . O.option O.auto)         (O.long "submit"                                 <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation."    )
    <*> O.some (O.argument parseAddressAny     $                                                   O.metavar "ADDRESS"       <> O.help "The addresses to receive the funds."                        )


-- | Parser for the "select" command.
selectCommand :: O.Mod O.CommandFields UtilCommand
selectCommand =
  O.command "select"
    $ O.info selectOptions
    $ O.progDesc "Select UTxO by asset."


-- | Parser for the "select" options.
selectOptions :: O.Parser UtilCommand
selectOptions =
  Output
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic" <> O.metavar "INTEGER"     <> O.help "Network magic, or omit for mainnet."       )
    <*> O.strOption                            (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> O.help "Location of the cardano-node socket file." )
    <*> parseOutputQuery
    <*> O.argument parseAddressAny             (                          O.metavar "ADDRESS"     <> O.help "The address."                              )


-- | Parser for the "decode-bech32" command.
decodeBechCommand :: O.Mod O.CommandFields UtilCommand
decodeBechCommand =
  O.command "decode-bech32"
    $ O.info decodeBechOptions
    $ O.progDesc "DecodBech32 data."


-- | Parser for the "decode-bech32" options.
decodeBechOptions :: O.Parser UtilCommand
decodeBechOptions =
  DecodeBech32
    <$> O.strArgument (O.metavar "BECH32" <> O.help "The Bech32 text.")


-- | Parser for the "encode-bech32" command.
encodeBechCommand :: O.Mod O.CommandFields UtilCommand
encodeBechCommand =
  O.command "encode-bech32"
    $ O.info encodeBechOptions
    $ O.progDesc "EncodBech32 data."


-- | Parser for the "encode-bech32" options.
encodeBechOptions :: O.Parser UtilCommand
encodeBechOptions =
  EncodeBech32
    <$> O.strArgument (O.metavar "PREFIX" <> O.help "The Bech32 human-readable prefix.")
    <*> O.strArgument (O.metavar "BASE16" <> O.help "The base 16 data to be encoded."  )


-- | Parser for the "slotting" command.
slottingCommand :: O.Mod O.CommandFields UtilCommand
slottingCommand =
  O.command "slotting"
    $ O.info slottingOptions
    $ O.progDesc "Find the slot-to-time relationship for the current epoch."


-- | Parser for the "slotting" options.
slottingOptions :: O.Parser UtilCommand
slottingOptions =
  Slotting
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic" <> O.metavar "INTEGER"     <> O.help "Network magic, or omit for mainnet."      )
    <*> O.strOption                            (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> O.help "Location of the cardano-node socket file.")
    <*> (O.optional .O.strOption             ) (O.long "out-file"      <> O.metavar "FILE"        <> O.help "Output file for slot configuration."      )
