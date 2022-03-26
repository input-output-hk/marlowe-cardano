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
                    Lovelace (..), NetworkId (..), SlotNo, TxMetadataInEra (TxMetadataNone), TxMintValue (TxMintNone))
import Control.Monad.Except (MonadError, MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseNetworkId, parseSlotNo, parseTokenName)
import Language.Marlowe.CLI.Transaction (buildClean, buildMinting)
import Language.Marlowe.CLI.Types (CliError)
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
      Clean{..} -> buildClean
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
      Mint{..}  -> buildMinting
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
                     >>= printTxId


-- | Parser for miscellaneous commands.
parseUtilCommand :: O.Parser UtilCommand
parseUtilCommand =
  O.hsubparser
    $ O.commandGroup "Miscellaneous low-level commands:"
    <> cleanCommand
    <> mintCommand


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
    <*> (O.many . O.strOption)                 (O.long "required-signer"                      <> O.metavar "SIGNING_FILE"  <> O.help "File containing a required signing key."                )
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
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"                        <> O.metavar "INTEGER"       <> O.help "Network magic, or omit for mainnet."                        )
    <*> O.strOption                            (O.long "socket-path"                          <> O.metavar "SOCKET_FILE"   <> O.help "Location of the cardano-node socket file."                  )
    <*> O.strOption                            (O.long "required-signer"                      <> O.metavar "SIGNING_FILE"  <> O.help "File containing a required signing key."                    )
    <*> (O.optional . O.strOption)             (O.long "metadata-file"                        <> O.metavar "JSON_FILE"     <> O.help "The CIP-25 metadata, with keys for each token name."        )
    <*> O.option O.auto                        (O.long "count"           <> O.value 1         <> O.metavar "INTEGER"       <> O.help "The number of each token to mint."                          )
    <*> (O.optional . O.option parseSlotNo)    (O.long "expires"                              <> O.metavar "SLOT_NO"       <> O.help "The slot number after which miniting is no longer possible.")
    <*> (O.option $ Lovelace <$> O.auto)       (O.long "lovelace"        <> O.value 2_000_000 <> O.metavar "LOVELACE"      <> O.help "The lovelace to send with each bundle of tokens."           )
    <*> O.option parseAddressAny               (O.long "change-address"                       <> O.metavar "ADDRESS"       <> O.help "Address to receive ADA in excess of fee."                   )
    <*> O.strOption                            (O.long "out-file"                             <> O.metavar "FILE"          <> O.help "Output file for transaction body."                          )
    <*> (O.optional . O.option O.auto)         (O.long "submit"                               <> O.metavar "SECONDS"       <> O.help "Also submit the transaction, and wait for confirmation."    )
    <*> O.many (O.argument parseTokenName      $                                                 O.metavar "TOKEN_NAME"    <> O.help "The name of the token."                                     )
