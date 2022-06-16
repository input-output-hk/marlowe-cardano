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
import Language.Marlowe.CLI.Codec (decodeBech32, encodeBech32)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseNetworkId, parseOutputQuery, parseSlotNo,
                                           parseTokenName)
import Language.Marlowe.CLI.Merkle (demerkleize, merkleize)
import Language.Marlowe.CLI.Sync (watchMarlowe)
import Language.Marlowe.CLI.Transaction (buildClean, buildFaucet', buildMinting, querySlotting, selectUtxos)
import Language.Marlowe.CLI.Types (CliError, OutputQuery)
import Plutus.V1.Ledger.Api (TokenName)

import qualified Options.Applicative as O


-- | Marlowe CLI commands and options.
data UtilCommand =
    -- | Clean UTxOs at an address.
    Clean
    {
      network         :: NetworkId   -- ^ The network ID, if any.
    , socketPath      :: FilePath    -- ^ The path to the node socket.
    , signingKeyFiles :: [FilePath]  -- ^ The files containing the required signing keys.
    , lovelace        :: Lovelace    -- ^ The lovelace to send with each bundle of tokens.
    , change          :: AddressAny  -- ^ The change address.
    , bodyFile        :: FilePath    -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int   -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    }
    -- | Mint tokens.
  | Mint
    {
      network        :: NetworkId       -- ^ The network ID, if any.
    , socketPath     :: FilePath        -- ^ The path to the node socket.
    , signingKeyFile :: FilePath        -- ^ The files containing the required signing keys.
    , metadataFile   :: Maybe FilePath  -- ^ The CIP-25 metadata for the minting, with keys for each token name.
    , count          :: Integer         -- ^ The number of each token to mint.
    , expires        :: Maybe SlotNo    -- ^ The slot number after which minting is no longer possible.
    , lovelace       :: Lovelace        -- ^ The lovelace to send with each token.
    , change         :: AddressAny      -- ^ The change address.
    , bodyFile       :: FilePath        -- ^ The output file for the transaction body.
    , submitTimeout  :: Maybe Int       -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    , tokenNames     :: [TokenName]     -- ^ The token names.
    }
    -- | Fund an address from a faucet.
  | Faucet
    {
      network       :: NetworkId     -- ^ The network ID, if any.
    , socketPath    :: FilePath      -- ^ The path to the node socket.
    , lovelace      :: Lovelace      -- ^ The lovelace to send to the address.
    , bodyFile      :: FilePath      -- ^ The output file for the transaction body.
    , submitTimeout :: Maybe Int     -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    , addresses     :: [AddressAny]  -- ^ The addresses.
    }
    -- | Select UTxO by asset.
  | Output
    {
      network    :: NetworkId    -- ^ The network ID, if any.
    , socketPath :: FilePath     -- ^ The path to the node socket.
    , query      :: OutputQuery  -- ^ Filter the query results.
    , address    :: AddressAny   -- ^ The addresses.
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
      network      :: NetworkId       -- ^ The network ID, if any.
    , socketPath   :: FilePath        -- ^ The path to the node socket.
    , slottingFile :: Maybe FilePath  -- ^ The output file for the slot configuration.
    }
    -- | Watch Marlowe transactions.
  | Watch
    {
      network     :: NetworkId        -- ^ The network ID, if any.
    , socketPath  :: FilePath         -- ^ The path to the node socket.
    , includeAll  :: Bool             -- ^ Whether to include non-Marlowe transactions.
    , cbor        :: Bool             -- ^ Whether to output CBOR instead of JSON.
    , continue    :: Bool             -- ^ Whether to continue watching when the tip of the chain is reached.
    , restartFile :: Maybe FilePath   -- ^ File for restoring and saving current point on the chain.
    , outputFile  :: Maybe FilePath   -- ^ File for recording Marlowe transactions.
    }
    -- | Merkleize a contract.
  | Merkleize
    {
      marloweFile :: FilePath        -- ^ The Marlowe JSON file containing the contract to be merkleized.
    , outputFile  :: Maybe FilePath  -- ^ The output file for the Marlowe JSON containing the merkleized contract.
    }
    -- | Demerkleize a contract.
  | Demerkleize
    {
      marloweFile :: FilePath        -- ^ The Marlowe JSON file containing the contract to be demerkleized.
    , outputFile  :: Maybe FilePath  -- ^ The output file for the Marlowe JSON containing the demerkleized contract, if any.
    }


-- | Run a miscellaneous command.
runUtilCommand :: MonadError CliError m
               => MonadIO m
               => UtilCommand  -- ^ The command.
               -> m ()         -- ^ Action for running the command.
runUtilCommand command =
  do
    let
      network' = network command
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
      Watch{..}        -> watchMarlowe
                            connection
                            includeAll
                            cbor
                            continue
                            restartFile
                            outputFile
      Merkleize{..}    -> merkleize
                            marloweFile
                            outputFile
      Demerkleize{..}  -> demerkleize
                            marloweFile
                            outputFile


-- | Parser for miscellaneous commands.
parseUtilCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
parseUtilCommand network socket =
  O.hsubparser
    $ O.commandGroup "Miscellaneous low-level commands:"
    <> cleanCommand network socket
    <> decodeBechCommand
    <> demerkleizeCommand
    <> encodeBechCommand
    <> faucetCommand network socket
    <> merkleizeCommand
    <> mintCommand network socket
    <> selectCommand network socket
    <> slottingCommand network socket
    <> watchCommand network socket


-- | Parser for the "clean" command.
cleanCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields UtilCommand
cleanCommand network socket =
  O.command "clean"
    $ O.info (cleanOptions network socket)
    $ O.progDesc "Reorganize the UTxOs at an address, separating tokens."


-- | Parser for the "clean" options.
cleanOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
cleanOptions network socket =
  Clean
    <$> O.option parseNetworkId           (O.long "testnet-magic"   <> network           <> O.metavar "INTEGER"      <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption                       (O.long "socket-path"     <> socket            <> O.metavar "SOCKET_FILE"  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> (O.some . O.strOption)            (O.long "required-signer"                      <> O.metavar "SIGNING_FILE" <> O.help "File containing a required signing key."                                                                         )
    <*> (O.option $ Lovelace <$> O.auto)  (O.long "lovelace"        <> O.value 2_000_000 <> O.metavar "LOVELACE"     <> O.help "The lovelace to send with each bundle of tokens."                                                                )
    <*> O.option parseAddressAny          (O.long "change-address"                       <> O.metavar "ADDRESS"      <> O.help "Address to receive ADA in excess of fee."                                                                        )
    <*> O.strOption                       (O.long "out-file"                             <> O.metavar "FILE"         <> O.help "Output file for transaction body."                                                                               )
    <*> (O.optional . O.option O.auto)    (O.long "submit"                               <> O.metavar "SECONDS"      <> O.help "Also submit the transaction, and wait for confirmation."                                                         )


-- | Parser for the "mint" command.
mintCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields UtilCommand
mintCommand network socket =
  O.command "mint"
    $ O.info (mintOptions network socket)
    $ O.progDesc "Mint native tokens."


-- | Parser for the "mint" options.
mintOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
mintOptions network socket =
  Mint
    <$> O.option parseNetworkId              (O.long "testnet-magic"   <> O.metavar "INTEGER"      <> network            <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption                          (O.long "socket-path"     <> O.metavar "SOCKET_FILE"  <> socket             <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.strOption                          (O.long "required-signer" <> O.metavar "SIGNING_FILE"                       <> O.help "File containing a required signing key."                                                                         )
    <*> (O.optional . O.strOption)           (O.long "metadata-file"   <> O.metavar "JSON_FILE"                          <> O.help "The CIP-25 metadata, with keys for each token name."                                                             )
    <*> O.option O.auto                      (O.long "count"           <> O.metavar "INTEGER"      <> O.value 1          <> O.help "The number of each token to mint."                                                                               )
    <*> (O.optional . O.option parseSlotNo)  (O.long "expires"         <> O.metavar "SLOT_NO"                            <> O.help "The slot number after which miniting is no longer possible."                                                     )
    <*> (O.option $ Lovelace <$> O.auto)     (O.long "lovelace"        <> O.metavar "LOVELACE"     <> O.value 10_000_000 <> O.help "The lovelace to send with each bundle of tokens."                                                                )
    <*> O.option parseAddressAny             (O.long "change-address"  <> O.metavar "ADDRESS"                            <> O.help "Address to receive ADA in excess of fee."                                                                        )
    <*> O.strOption                          (O.long "out-file"        <> O.metavar "FILE"                               <> O.help "Output file for transaction body."                                                                               )
    <*> (O.optional . O.option O.auto)       (O.long "submit"          <> O.metavar "SECONDS"                            <> O.help "Also submit the transaction, and wait for confirmation."                                                         )
    <*> O.some (O.argument parseTokenName    $                            O.metavar "TOKEN_NAME"                         <> O.help "The name of the token."                                                                                          )


-- | Parser for the "faucet" command.
faucetCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields UtilCommand
faucetCommand network socket =
  O.command "faucet"
    $ O.info (faucetOptions network socket)
    $ O.progDesc "Fund an address from a faucet. Note that the faucet is only funded on the private developer testnet for Marlowe, and that this command will not supply funds on public networks."


-- | Parser for the "faucet" options.
faucetOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
faucetOptions network socket =
  Faucet
    <$> O.option parseNetworkId            (O.long "testnet-magic" <> O.metavar "INTEGER"     <> network               <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption                        (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket                <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> (O.option $ Lovelace <$> O.auto)   (O.long "lovelace"      <> O.metavar "LOVELACE"    <> O.value 1_000_000_000 <> O.help "The lovelace to send to each address."                                                                           )
    <*> O.strOption                        (O.long "out-file"      <> O.metavar "FILE"                                 <> O.help "Output file for transaction body."                                                                               )
    <*> (O.optional . O.option O.auto)     (O.long "submit"        <> O.metavar "SECONDS"                              <> O.help "Also submit the transaction, and wait for confirmation."                                                         )
    <*> O.some (O.argument parseAddressAny $                          O.metavar "ADDRESS"                              <> O.help "The addresses to receive the funds."                                                                             )


-- | Parser for the "select" command.
selectCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields UtilCommand
selectCommand network socket =
  O.command "select"
    $ O.info (selectOptions network socket)
    $ O.progDesc "Select UTxO by asset."


-- | Parser for the "select" options.
selectOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
selectOptions network socket =
  Output
    <$> O.option parseNetworkId    (O.long "testnet-magic" <> O.metavar "INTEGER"     <> network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                               )
    <*> O.strOption                (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value." )
    <*> parseOutputQuery
    <*> O.argument parseAddressAny (                          O.metavar "ADDRESS"                <> O.help "The address."                                                                                                     )


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
slottingCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields UtilCommand
slottingCommand network socket =
  O.command "slotting"
    $ O.info (slottingOptions network socket)
    $ O.progDesc "Find the slot-to-time relationship for the current epoch."


-- | Parser for the "slotting" options.
slottingOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
slottingOptions network socket =
  Slotting
    <$> O.option parseNetworkId   (O.long "testnet-magic" <> O.metavar "INTEGER"     <> network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption               (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> (O.optional .O.strOption) (O.long "out-file"      <> O.metavar "FILE"                   <> O.help "Output file for slot configuration."                                                                             )


-- | Parser for the "watch" command.
watchCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields UtilCommand
watchCommand network socket =
  O.command "watch"
    $ O.info (watchOptions network socket)
    $ O.progDesc "Watch Marlowe transactions on a Cardano node."


-- | Parser for the "watch" options.
watchOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser UtilCommand
watchOptions network socket =
  Watch
    <$> O.option parseNetworkId    (O.long "testnet-magic" <> O.metavar "INTEGER"     <> network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value."                              )
    <*> O.strOption                (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.switch                   (O.long "all"                                                 <> O.help "Whether to also output non-Marlowe transactions."                                                                )
    <*> O.switch                   (O.long "cbor"                                                <> O.help "Whether to output CBOR instead of JSON."                                                                         )
    <*> O.switch                   (O.long "continue"                                            <> O.help "Whether to continue when the current tip of the chain is reached."                                               )
    <*> (O.optional . O.strOption) (O.long "restart"       <> O.metavar "POINT_FILE"             <> O.help "File for restoring and saving current point on the chain."                                                       )
    <*> (O.optional . O.strOption) (O.long "out-file"      <> O.metavar "OUTPUT_FILE"            <> O.help "File in which to store records of Marlowe transactions."                                                         )


-- | Parser for the "merkleize" command.
merkleizeCommand :: O.Mod O.CommandFields UtilCommand
merkleizeCommand =
  O.command "merkleize"
    $ O.info merkleizeOptions
    $ O.progDesc "Merkleize a Marlowe contract."


-- | Parser for the "merkleize" options.
merkleizeOptions :: O.Parser UtilCommand
merkleizeOptions =
  Merkleize
    <$> O.strOption                (O.long "in-file"  <> O.metavar "MARLOWE_FILE"  <> O.help "The Marlowe JSON file containing the contract to be merkleized.")
    <*> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "MARLOWE_FILE"  <> O.help "Output file Marlowe JSON containing the merkleized contract."   )


-- | Parser for the "demerkleize" command.
demerkleizeCommand :: O.Mod O.CommandFields UtilCommand
demerkleizeCommand =
  O.command "demerkleize"
    $ O.info demerkleizeOptions
    $ O.progDesc "Demerkleize a Marlowe contract."


-- | Parser for the "demerkleize" options.
demerkleizeOptions :: O.Parser UtilCommand
demerkleizeOptions =
  Demerkleize
    <$> O.strOption                (O.long "in-file"  <> O.metavar "MARLOWE_FILE"  <> O.help "The Marlowe JSON file containing the contract to be demerkleized.")
    <*> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "MARLOWE_FILE"  <> O.help "Output file Marlowe JSON containing the demerkleized contract."   )
