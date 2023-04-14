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


{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}


module Language.Marlowe.CLI.Command.Util
  ( -- * Marlowe CLI Commands
    UtilCommand(..)
  , parseUtilCommand
  , runUtilCommand
  ) where


import Cardano.Api
  ( AddressInEra
  , ConsensusModeParams(CardanoModeParams)
  , EpochSlots(..)
  , IsShelleyBasedEra
  , LocalNodeConnectInfo(..)
  , Lovelace(..)
  , NetworkId(..)
  , SlotNo
  , TxMetadataInEra(TxMetadataNone)
  , TxMintValue(TxMintNone)
  , lovelaceToValue
  )
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Language.Marlowe.CLI.Codec (decodeBech32, encodeBech32)
import Language.Marlowe.CLI.Command.Parse
  ( parseAddress
  , parseNetworkId
  , parseOutputQuery
  , parseSlotNo
  , readAddressEither
  , readTokenName
  , requiredSignersOpt
  , txBodyFileOpt
  , walletOpt
  )
import Language.Marlowe.CLI.Merkle (demerkleize, merkleize)
import Language.Marlowe.CLI.Sync (watchMarlowe)
import Language.Marlowe.CLI.Transaction (buildClean, buildFaucet, buildMinting, querySlotting, selectUtxos)
import Language.Marlowe.CLI.Types (CliEnv, CliError, OutputQuery, OutputQueryResult, SigningKeyFile, TxBodyFile)
import Plutus.V1.Ledger.Api (TokenName)

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Monad.Reader (MonadReader)
import qualified Data.List.NonEmpty as L
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Time.Units (Second)
import GHC.Natural (Natural)
import qualified Options.Applicative as O
import qualified Options.Applicative.NonEmpty as O


-- | Marlowe CLI commands and options.
data UtilCommand era =
    -- | Clean UTxOs at an address.
    Clean
    {
      network         :: NetworkId                                      -- ^ The network ID, if any.
    , socketPath      :: FilePath                                       -- ^ The path to the node socket.
    , signingKeyFiles :: [SigningKeyFile]                               -- ^ The files containing the required signing keys.
    , lovelace        :: Lovelace                                       -- ^ The lovelace to send with each bundle of tokens.
    , change          :: AddressInEra era                               -- ^ The change address.
    , bodyFile        :: TxBodyFile                                     -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Second                                   -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    }
    -- | Mint tokens.
  | Mint
    {
      network        :: NetworkId                                       -- ^ The network ID, if any.
    , socketPath     :: FilePath                                        -- ^ The path to the node socket.
    , issuer         :: (AddressInEra era, SigningKeyFile)              -- ^ The change address.
    , providers      :: [(AddressInEra era, SigningKeyFile)]            -- ^ Additional token providers.
    , metadataFile   :: Maybe FilePath                                  -- ^ The CIP-25 metadata for the minting, with keys for each token name.
    , count          :: Natural                                         -- ^ The number of each token to mint.
    , expires        :: Maybe SlotNo                                    -- ^ The slot number after which minting is no longer possible.
    , bodyFile       :: TxBodyFile                                      -- ^ The output file for the transaction body.
    , submitTimeout  :: Maybe Second                                    -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    , tokenDistribution :: L.NonEmpty (TokenName, AddressInEra era)                            -- ^ The token names.
    }
  | Burn
    {
      network               :: NetworkId                                -- ^ The network ID, if any.
    , socketPath            :: FilePath                                 -- ^ The path to the node socket.
    , issuer                :: (AddressInEra era, SigningKeyFile)       -- ^ The change address.
    , providers             :: [(AddressInEra era, SigningKeyFile)]     -- ^ Additional token providers.
    , metadataFile          :: Maybe FilePath                           -- ^ The CIP-25 metadata for the minting, with keys for each token name.
    , bodyFile              :: TxBodyFile            -- ^ The output file for the transaction body.
    , expires               :: Maybe SlotNo                             -- ^ The slot number after which minting is no longer possible.
    , submitTimeout         :: Maybe Second          -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    }
    -- | Fund an address from a source wallet.
  | Fund
    {
      network            :: NetworkId                                     -- ^ The network ID, if any.
    , socketPath         :: FilePath                                      -- ^ The path to the node socket.
    , amount             :: Maybe Lovelace                                -- ^ The lovelace to send to the address. By default we drain out the address.
    , bodyFile           :: TxBodyFile                                    -- ^ The output file for the transaction body.
    , submitTimeout      :: Maybe Second                                  -- ^ Whether to submit the transaction, and its confirmation timeout in seconds.
    , faucetCredentials  :: (AddressInEra era, SigningKeyFile)            -- ^ The change address.
    , destAddresses      :: [AddressInEra era]                            -- ^ The addresses.
    }
    -- | Select UTxO by asset.
  | Output
    {
      network    :: NetworkId                                       -- ^ The network ID, if any.
    , socketPath :: FilePath                                        -- ^ The path to the node socket.
    , query      :: Maybe (OutputQuery era (OutputQueryResult era)) -- ^ Filter the query results.
    , address    :: AddressInEra era                                -- ^ The addresses.
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
               => MonadReader (CliEnv era) m
               => MonadIO m
               => UtilCommand era -- ^ The command.
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
      Mint{..}         -> do
                            let
                              step (tokenName, address) = (tokenName, count, address)
                              tokenDistribution' = step <$> tokenDistribution
                              (addr, skeyFile) = issuer
                            buildMinting
                              connection
                              skeyFile
                              (Right tokenDistribution')
                              metadataFile
                              expires
                              addr
                              bodyFile
                              submitTimeout
      Burn{..}         -> do
                            let
                              (addr, skeyFile) = issuer
                            buildMinting
                              connection
                              skeyFile
                              (Left providers)
                              metadataFile
                              expires
                              addr
                              bodyFile
                              submitTimeout
      Fund{..}       -> do
                            let
                              (addr, skeyFile) = faucetCredentials
                            buildFaucet
                              connection
                              (lovelaceToValue <$> amount)
                              destAddresses
                              addr
                              skeyFile
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

--
-- | Parser for miscellaneous commands.
parseUtilCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
parseUtilCommand network socket =
  O.hsubparser
    $ O.commandGroup "Miscellaneous low-level commands:"
    <> cleanCommand network socket
    <> decodeBechCommand
    <> demerkleizeCommand
    <> encodeBechCommand
    <> fundAddressCommand network socket
    <> merkleizeCommand
    <> mintCommand network socket
    <> burnCommand network socket
    <> selectCommand network socket
    <> slottingCommand network socket
    <> watchCommand network socket


-- | Parser for the "clean" command.
cleanCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
cleanCommand network socket =
  O.command "clean"
    $ O.info (cleanOptions network socket)
    $ O.progDesc "Reorganize the UTxOs at an address, separating tokens."


-- | Parser for the "clean" options.
cleanOptions :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
cleanOptions network socket =
  Clean
    <$> parseNetworkId network
    <*> O.strOption                       (O.long "socket-path"     <> socket            <> O.metavar "SOCKET_FILE"  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> requiredSignersOpt

    <*> (O.option $ Lovelace <$> O.auto)  (O.long "lovelace"        <> O.value 2_000_000 <> O.metavar "LOVELACE"     <> O.help "The lovelace to send with each bundle of tokens."                                                                )
    <*> O.option parseAddress             (O.long "change-address"                       <> O.metavar "ADDRESS"      <> O.help "Address to receive ADA in excess of fee."                                                                        )
    <*> txBodyFileOpt

    <*> (O.optional . O.option O.auto)    (O.long "submit"                               <> O.metavar "SECONDS"      <> O.help "Also submit the transaction, and wait for confirmation."                                                         )


-- | Parser for the "mint" command.
mintCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
mintCommand network socket =
  O.command "mint"
    $ O.info (mintOptions network socket)
    $ O.progDesc "Mint native tokens."


-- | Parser for the "mint" options.
mintOptions :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
mintOptions network socket =
  Mint
    <$> parseNetworkId network
    <*> O.strOption                          (O.long "socket-path"     <> O.metavar "SOCKET_FILE"  <> socket             <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> walletOpt                            (O.long "issuer"          <> O.metavar "ADDRESS:SIGNING_FILE"               <> O.help "Issuer wallet info")
    <*> tokenProviderOpt
    <*> (O.optional . O.strOption)           (O.long "metadata-file"   <> O.metavar "JSON_FILE"                          <> O.help "The CIP-25 metadata, with keys for each token name."                                                             )
    <*> O.option O.auto                      (O.long "count"           <> O.metavar "INTEGER"      <> O.value 1          <> O.help "The number of each token to mint."                                                                               )
    <*> (O.optional . O.option parseSlotNo)  (O.long "expires"         <> O.metavar "SLOT_NO"                            <> O.help "The slot number after which miniting is no longer possible."                                                     )
    <*> txBodyFileOpt

    <*> (O.optional . O.option O.auto)       (O.long "submit"          <> O.metavar "SECONDS"                            <> O.help "Also submit the transaction, and wait for confirmation."                                                         )
    -- TODO:  help message and metavar
    <*> O.some1 (O.argument parseTokenDistribution $                            O.metavar "TOKEN_NAME:ADDRESS"                         <> O.help "The name of the token." )
  where
    tokenProviderOpt =
      fmap (fromMaybe []) $ (O.optional . O.some . walletOpt) (O.long "token-provider" <> O.metavar "ADDRESS:SIGNING_FILE" <> O.help "Additional tokens owners info.")
    parseTokenDistribution :: IsShelleyBasedEra era => O.ReadM (TokenName, AddressInEra era)
    parseTokenDistribution =
      O.eitherReader
        $ splitOn ":" >>> \case
            [tokenName, address] -> do
                address' <- readAddressEither address
                let
                  tokenName' = readTokenName tokenName
                pure (tokenName', address')
            _  -> Left "Expecting token name and recipient address in the following format: TOKENNAME:ADDRESS"

-- | Parser for the "mint" command.
burnCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
burnCommand network socket =
  O.command "burn"
    $ O.info (burnOptions network socket)
    $ O.progDesc "Burn native tokens."


-- | Parser for the "mint" options.
burnOptions :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
burnOptions network socket =
  Burn
    <$> parseNetworkId network
    <*> O.strOption                          (O.long "socket-path"     <> O.metavar "SOCKET_FILE"  <> socket             <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> walletOpt                            (O.long "issuer"          <> O.metavar "ADDRESS:SIGNING_FILE"               <> O.help "Issuer wallet info")

    <*> tokenProviderOpt

    <*> (O.optional . O.strOption)           (O.long "metadata-file"   <> O.metavar "JSON_FILE"                          <> O.help "The CIP-25 metadata, with keys for each token name."                                                             )
    <*> txBodyFileOpt

    <*> (O.optional . O.option parseSlotNo)  (O.long "expires"         <> O.metavar "SLOT_NO"                            <> O.help "The slot number after which miniting is no longer possible."                                                     )

    <*> (O.optional . O.option O.auto)       (O.long "submit"          <> O.metavar "SECONDS"                            <> O.help "Also submit the transaction, and wait for confirmation."                                                         )
  where
    tokenProviderOpt =
      fmap (fromMaybe []) $ (O.optional . O.some . walletOpt) (O.long "token-provider" <> O.metavar "ADDRESS:SIGNING_FILE" <> O.help "Additional tokens owners info.")


-- | Parser for the "fund-address" command.
fundAddressCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
fundAddressCommand network socket =
  O.command "fund-address"
    $ O.info (fundAddressOptions network socket)
    $ O.progDesc "Fund an address from a source wallet. If the source wallet is a faucet, note that the faucet is only funded on the private developer testnet for Marlowe, and that this command will not supply funds on public networks."


-- | Parser for the "fund-address" options.
fundAddressOptions :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
fundAddressOptions network socket =
  Fund
    <$> parseNetworkId network
    <*> O.strOption                        (O.long "socket-path"     <> O.metavar "SOCKET_FILE" <> socket                <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> (lovelaceOpt <|> sendAllOpt)
    <*> txBodyFileOpt
    <*> (O.optional . O.option O.auto)     (O.long "submit"          <> O.metavar "SECONDS"                              <> O.help "Also submit the transaction, and wait for confirmation."                                                         )
    <*> walletOpt                          (O.long "source-wallet-credentials" <> O.metavar "ADDRESS:SIGNING_FILE"       <> O.help "Credentials for the source wallet that will send the funds")
    <*> O.some                             (O.argument parseAddress $ O.metavar "ADDRESS"                                <> O.help "The addresses to receive the funds."                                                                               )
  where
    lovelaceOpt = fmap Just . (O.option $ Lovelace <$> O.auto) $
      O.long "lovelace"
      <> O.metavar "LOVELACE"
      <> O.help "The lovelace to send to each address."
    sendAllOpt = O.flag' Nothing $
      O.long "send-all"
      <> O.help "Send all available funds to the address."

-- | Parser for the "select" command.
selectCommand :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
selectCommand network socket =
  O.command "select"
    $ O.info (selectOptions network socket)
    $ O.progDesc "Select UTxO by asset."


-- | Parser for the "select" options.
selectOptions :: IsShelleyBasedEra era => O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
selectOptions network socket =
  Output
    <$> parseNetworkId network
    <*> O.strOption                (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value." )
    <*> parseOutputQuery
    <*> O.argument parseAddress    (                          O.metavar "ADDRESS"                <> O.help "The address."                                                                                                     )


-- | Parser for the "decode-bech32" command.
decodeBechCommand :: O.Mod O.CommandFields (UtilCommand era)
decodeBechCommand =
  O.command "decode-bech32"
    $ O.info decodeBechOptions
    $ O.progDesc "DecodBech32 data."


-- | Parser for the "decode-bech32" options.
decodeBechOptions :: O.Parser (UtilCommand era)
decodeBechOptions =
  DecodeBech32
    <$> O.strArgument (O.metavar "BECH32" <> O.help "The Bech32 text.")


-- | Parser for the "encode-bech32" command.
encodeBechCommand :: O.Mod O.CommandFields (UtilCommand era)
encodeBechCommand =
  O.command "encode-bech32"
    $ O.info encodeBechOptions
    $ O.progDesc "EncodBech32 data."


-- | Parser for the "encode-bech32" options.
encodeBechOptions :: O.Parser (UtilCommand era)
encodeBechOptions =
  EncodeBech32
    <$> O.strArgument (O.metavar "PREFIX" <> O.help "The Bech32 human-readable prefix.")
    <*> O.strArgument (O.metavar "BASE16" <> O.help "The base 16 data to be encoded."  )


-- | Parser for the "slotting" command.
slottingCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
slottingCommand network socket =
  O.command "slotting"
    $ O.info (slottingOptions network socket)
    $ O.progDesc "Find the slot-to-time relationship for the current epoch."


-- | Parser for the "slotting" options.
slottingOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
slottingOptions network socket =
  Slotting
    <$> parseNetworkId network
    <*> O.strOption               (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> (O.optional .O.strOption) (O.long "out-file"      <> O.metavar "FILE"                   <> O.help "Output file for slot configuration."                                                                             )


-- | Parser for the "watch" command.
watchCommand :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Mod O.CommandFields (UtilCommand era)
watchCommand network socket =
  O.command "watch"
    $ O.info (watchOptions network socket)
    $ O.progDesc "Watch Marlowe transactions on a Cardano node."


-- | Parser for the "watch" options.
watchOptions :: O.Mod O.OptionFields NetworkId -> O.Mod O.OptionFields FilePath -> O.Parser (UtilCommand era)
watchOptions network socket =
  Watch
    <$> parseNetworkId network
    <*> O.strOption                (O.long "socket-path"   <> O.metavar "SOCKET_FILE" <> socket  <> O.help "Location of the cardano-node socket file. Defaults to the CARDANO_NODE_SOCKET_PATH environment variable's value.")
    <*> O.switch                   (O.long "all"                                                 <> O.help "Whether to also output non-Marlowe transactions."                                                                )
    <*> O.switch                   (O.long "cbor"                                                <> O.help "Whether to output CBOR instead of JSON."                                                                         )
    <*> O.switch                   (O.long "continue"                                            <> O.help "Whether to continue when the current tip of the chain is reached."                                               )
    <*> (O.optional . O.strOption) (O.long "restart"       <> O.metavar "POINT_FILE"             <> O.help "File for restoring and saving current point on the chain."                                                       )
    <*> (O.optional . O.strOption) (O.long "out-file"      <> O.metavar "OUTPUT_FILE"            <> O.help "File in which to store records of Marlowe transactions."                                                         )


-- | Parser for the "merkleize" command.
merkleizeCommand :: O.Mod O.CommandFields (UtilCommand era)
merkleizeCommand =
  O.command "merkleize"
    $ O.info merkleizeOptions
    $ O.progDesc "Merkleize a Marlowe contract."


-- | Parser for the "merkleize" options.
merkleizeOptions :: O.Parser (UtilCommand era)
merkleizeOptions =
  Merkleize
    <$> O.strOption                (O.long "in-file"  <> O.metavar "MARLOWE_FILE"  <> O.help "The Marlowe JSON file containing the contract to be merkleized.")
    <*> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "MARLOWE_FILE"  <> O.help "Output file Marlowe JSON containing the merkleized contract."   )


-- | Parser for the "demerkleize" command.
demerkleizeCommand :: O.Mod O.CommandFields (UtilCommand era)
demerkleizeCommand =
  O.command "demerkleize"
    $ O.info demerkleizeOptions
    $ O.progDesc "Demerkleize a Marlowe contract."


-- | Parser for the "demerkleize" options.
demerkleizeOptions :: O.Parser (UtilCommand era)
demerkleizeOptions =
  Demerkleize
    <$> O.strOption                (O.long "in-file"  <> O.metavar "MARLOWE_FILE"  <> O.help "The Marlowe JSON file containing the contract to be demerkleized.")
    <*> (O.optional . O.strOption) (O.long "out-file" <> O.metavar "MARLOWE_FILE"  <> O.help "Output file Marlowe JSON containing the demerkleized contract."   )
