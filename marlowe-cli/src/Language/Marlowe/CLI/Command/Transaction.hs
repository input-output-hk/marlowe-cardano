-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Transaction-related commands in the Marlowe CLI tool.
--
-----------------------------------------------------------------------------


{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module Language.Marlowe.CLI.Command.Transaction (
-- * Marlowe CLI Commands
  TransactionCommand(..)
, parseTransactionCommand
, runTransactionCommand
) where


import           Cardano.Api                        (AddressAny, ConsensusModeParams (CardanoModeParams),
                                                     EpochSlots (..), LocalNodeConnectInfo (..), NetworkId (..), SlotNo,
                                                     TxIn)
import           Control.Monad                      (when)
import           Control.Monad.Except               (MonadError, MonadIO, liftIO, throwError)
import           Data.Maybe                         (fromMaybe)
import           Language.Marlowe.CLI.Command.Parse (parseAddressAny, parseNetworkId, parseSlotNo, parseTxIn,
                                                     parseTxOut, parseValue)
import           Language.Marlowe.CLI.Transaction   (buildContinuing, buildIncoming, buildOutgoing, buildSimple, submit)
import           Language.Marlowe.CLI.Types         (CliError)

import qualified Cardano.Api                        as Api (Value)
import qualified Options.Applicative                as O


-- | Marlowe CLI commands and options.
data TransactionCommand =
    -- | Build a non-Marlowe transaction.
    BuildTransact
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , signingKeyFiles :: [FilePath]                 -- ^ The files containing the required signing keys.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , change          :: AddressAny                 -- ^ The change address.
    , bodyFile        :: FilePath                   -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int                  -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , printStats      :: Bool                       -- ^ Whether to print statistics about the contract and transaction.
    , invalid         :: Bool                       -- ^ Assertion that the transaction is invalid.
    }
    -- | Build a transaction paying into a Marlowe contract.
  | BuildCreate
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , scriptAddress   :: AddressAny                 -- ^ The script address.
    , signingKeyFiles :: [FilePath]                 -- ^ The files containing the required signing keys.
    , outputDatumFile :: FilePath                   -- ^ The file containing the datum for the payment to the script.
    , outputValue     :: Api.Value                  -- ^ The value to be paid to the script.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , change          :: AddressAny                 -- ^ The change address.
    , bodyFile        :: FilePath                   -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int                  -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , printStats      :: Bool                       -- ^ Whether to print statistics about the contract and transaction.
    , invalid         :: Bool                       -- ^ Assertion that the transaction is invalid.
    }
    -- | Build a transaction that spends from and pays to a Marlowe contract.
  | BuildAdvance
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , scriptAddress   :: AddressAny                 -- ^ The script address.
    , validatorFile   :: FilePath                   -- ^ The file containing the script validator.
    , redeemerFile    :: FilePath                   -- ^ The file containing the redeemer.
    , inputDatumFile  :: FilePath                   -- ^ The file containing the datum for spending from the script.
    , signingKeyFiles :: [FilePath]                 -- ^ The files containing the required signing keys.
    , inputTxIn       :: TxIn                       -- ^ The script eUTxO to be spent.
    , outputDatumFile :: FilePath                   -- ^ The file containing the datum for the payment to the script.
    , outputValue     :: Api.Value                  -- ^ The value to be paid to the script.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , collateral      :: TxIn                       -- ^ The collateral.
    , change          :: AddressAny                 -- ^ The change address.
    , minimumSlot     :: SlotNo                     -- ^ The first valid slot for the transaction.
    , maximumSlot     :: SlotNo                     -- ^ The last valid slot for the transaction.
    , bodyFile        :: FilePath                   -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int                  -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , printStats      :: Bool                       -- ^ Whether to print statistics about the contract and transaction.
    , invalid         :: Bool                       -- ^ Assertion that the transaction is invalid.
    }
    -- | Build a transaction spending from a Marlowe contract.
  | BuildClose
    {
      network         :: Maybe NetworkId            -- ^ The network ID, if any.
    , socketPath      :: FilePath                   -- ^ The path to the node socket.
    , validatorFile   :: FilePath                   -- ^ The file containing the script validator.
    , redeemerFile    :: FilePath                   -- ^ The file containing the redeemer.
    , inputDatumFile  :: FilePath                   -- ^ The file containing the datum for spending from the script.
    , signingKeyFiles :: [FilePath]                 -- ^ The files containing the required signing keys.
    , inputTxIn       :: TxIn                       -- ^ The script eUTxO to be spent.
    , inputs          :: [TxIn]                     -- ^ The transaction inputs.
    , outputs         :: [(AddressAny, Api.Value)]  -- ^ The transaction outputs.
    , collateral      :: TxIn                       -- ^ The collateral.
    , change          :: AddressAny                 -- ^ The change address.
    , minimumSlot     :: SlotNo                     -- ^ The first valid slot for the transaction.
    , maximumSlot     :: SlotNo                     -- ^ The last valid slot for the transaction.
    , bodyFile        :: FilePath                   -- ^ The output file for the transaction body.
    , submitTimeout   :: Maybe Int                  -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    , printStats      :: Bool                       -- ^ Whether to print statistics about the contract and transaction.
    , invalid         :: Bool                       -- ^ Assertion that the transaction is invalid.
    }
    -- | Submit a transaction.
  | Submit
    {
      network         :: Maybe NetworkId  -- ^ The network ID, if any.
    , socketPath      :: FilePath         -- ^ The path to the node socket.
    , bodyFile        :: FilePath         -- ^ The JSON file containing the transaction body.
    , signingKeyFiles :: [FilePath]       -- ^ The signing key files.
    , submitTimeout   :: Maybe Int        -- ^ Whether to submit the transaction, and its confirmation timeout in secontds.
    }


-- | Run a transaction-related command.
runTransactionCommand :: MonadError CliError m
                      => MonadIO m
                      => TransactionCommand  -- ^ The command.
                      -> m ()                -- ^ Action for running the command.
runTransactionCommand command =
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
      guardMainnet = when (network' == Mainnet) $ throwError "Mainnet usage is not supported."
      padTxOut (address, value) = (address, Nothing, value)
      outputs' = padTxOut <$> outputs command
    case command of
      BuildTransact{..}          -> guardMainnet
                                      >> buildSimple
                                        connection
                                        signingKeyFiles
                                        inputs outputs' change
                                        bodyFile
                                        submitTimeout
                                        printStats
                                        invalid
                                      >>= printTxId
      BuildCreate{..}            -> guardMainnet
                                      >> buildIncoming
                                        connection
                                        scriptAddress
                                        signingKeyFiles
                                        outputDatumFile
                                        outputValue
                                        inputs outputs' change
                                        bodyFile
                                        submitTimeout
                                        printStats
                                        invalid
                                      >>= printTxId
      BuildAdvance{..}           -> guardMainnet
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
                                        inputs outputs' collateral change
                                        minimumSlot maximumSlot
                                        bodyFile
                                        submitTimeout
                                        printStats
                                        invalid
                                      >>= printTxId
      BuildClose{..}             -> guardMainnet
                                      >> buildOutgoing
                                        connection
                                        validatorFile
                                        redeemerFile
                                        inputDatumFile
                                        signingKeyFiles
                                        inputTxIn
                                        inputs outputs' collateral change
                                        minimumSlot maximumSlot
                                        bodyFile
                                        submitTimeout
                                        printStats
                                        invalid
                                      >>= printTxId
      Submit{..}                 -> guardMainnet
                                      >> submit
                                        connection
                                        bodyFile
                                        signingKeyFiles
                                        (fromMaybe 0 submitTimeout)
                                      >>= printTxId


-- | Parser for transaction-related commands.
parseTransactionCommand :: O.Parser TransactionCommand
parseTransactionCommand =
  O.hsubparser
    $ O.commandGroup "Low-level commands for creating and submitting transactions:"
    <> buildContinuingCommand
    <> buildOutgoingCommand
    <> buildIncomingCommand
    <> buildSimpleCommand
    <> submitCommand


-- | Parser for the "simple" command.
buildSimpleCommand :: O.Mod O.CommandFields TransactionCommand
buildSimpleCommand =
  O.command "simple"
    $ O.info buildSimpleOptions
    $ O.progDesc "Build a non-Marlowe transaction."


-- | Parser for the "simple" options.
buildSimpleOptions :: O.Parser TransactionCommand
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
buildIncomingCommand :: O.Mod O.CommandFields TransactionCommand
buildIncomingCommand =
  O.command "create"
    $ O.info buildIncomingOptions
    $ O.progDesc "Build a transaction that pays to a Marlowe script."


-- | Parser for the "create" options.
buildIncomingOptions :: O.Parser TransactionCommand
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
buildContinuingCommand :: O.Mod O.CommandFields TransactionCommand
buildContinuingCommand =
  O.command "advance"
    $ O.info buildContinuingOptions
    $ O.progDesc "Build a transaction that both spends from and pays to a Marlowe script."


-- | Parser for the "advance" options.
buildContinuingOptions :: O.Parser TransactionCommand
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
buildOutgoingCommand :: O.Mod O.CommandFields TransactionCommand
buildOutgoingCommand =
  O.command "close"
    $ O.info buildOutgoingOptions
    $ O.progDesc "Build a transaction that spends from a Marlowe script."


-- | Parser for the "close" options.
buildOutgoingOptions :: O.Parser TransactionCommand
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
submitCommand :: O.Mod O.CommandFields TransactionCommand
submitCommand =
  O.command "submit"
    $ O.info submitOptions
    $ O.progDesc "Submit a transaction body."


-- | Parser for the "submit" options.
submitOptions :: O.Parser TransactionCommand
submitOptions =
  Submit
    <$> (O.optional . O.option parseNetworkId) (O.long "testnet-magic"   <> O.metavar "INTEGER"      <> O.help "Network magic, or omit for mainnet."                    )
    <*> O.strOption                            (O.long "socket-path"     <> O.metavar "SOCKET_FILE"  <> O.help "Location of the cardano-node socket file."              )
    <*> O.strOption                            (O.long "tx-body-file"    <> O.metavar "BODY_FILE"    <> O.help "File containing the transaction body."                  )
    <*> (O.many . O.strOption)                 (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "Files containing required signing keys."                )
    <*> (O.optional . O.option O.auto)         (O.long "timeout"         <> O.metavar "SECONDS"      <> O.help "Also submit the transaction, and wait for confirmation.")
