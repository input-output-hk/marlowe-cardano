{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Parsing Cardano types in command-line options.
--
-----------------------------------------------------------------------------

module Language.Marlowe.CLI.Command.Parse
  ( -- * Parsers
    parseAddress
  , parseAssetId
  , parseByteString
  , parseCurrencySymbol
  , parseInput
  , parseInputContent
  , parseLovelaceValue
  , parseNetworkId
  , parseOutputQuery
  , parsePOSIXTime
  , parseParty
  , parseProtocolVersion
  , parseRole
  , parseSlot
  , parseSlotNo
  , parseStakeAddressReference
  , parseTimeout
  , parseToken
  , parseTokenName
  , parseTxId
  , parseTxIn
  , parseTxIx
  , parseTxOut
  , parseUrl
  , parseValue
  , parseWallet
  , protocolVersionOpt
  , publishingStrategyOpt
  , readAddressEither
  , readTokenName
  , requiredSignerOpt
  , requiredSignersOpt
  , timeoutHelpMsg
  , txBodyFileOpt
  , walletOpt
  ) where


import Cardano.Api
  ( AddressInEra
  , AsType(..)
  , AssetId(..)
  , AssetName(..)
  , IsShelleyBasedEra
  , Lovelace(..)
  , NetworkId(..)
  , NetworkMagic(..)
  , Quantity(..)
  , ShelleyBasedEra(..)
  , SlotNo(..)
  , StakeAddressReference(..)
  , TxId(..)
  , TxIn(..)
  , TxIx(..)
  , Value
  , deserialiseAddress
  , deserialiseFromRawBytesHex
  , lovelaceToValue
  , quantityToLovelace
  , shelleyBasedEra
  , valueFromList
  )
import Cardano.Api.Shelley (StakeAddress(..), fromShelleyStakeCredential)
import Control.Applicative ((<|>))
import Data.List.Split (splitOn)
import Language.Marlowe.CLI.Types
  ( OutputQuery(..)
  , OutputQueryResult
  , PublishingStrategy(PublishAtAddress, PublishPermanently)
  , SigningKeyFile(SigningKeyFile)
  , SomeTimeout(..)
  , TxBodyFile(TxBodyFile)
  )
import Language.Marlowe.Core.V1.Semantics.Types (ChoiceId(..), Input(..), InputContent(..), Party(..), Token(..))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddressBech32)
import Ledger (POSIXTime(..))
import Plutus.V1.Ledger.Ada (adaSymbol, adaToken)
import Plutus.V1.Ledger.Api (BuiltinByteString, CurrencySymbol(..), TokenName(..), toBuiltin)
import Plutus.V1.Ledger.Slot (Slot(..))
import Servant.Client (BaseUrl, parseBaseUrl)
import Text.Read (readEither)
import Text.Regex.Posix ((=~))

import qualified Cardano.Api as C
import Control.Category ((>>>))
import qualified Data.ByteString.Base16 as Base16 (decode)
import qualified Data.ByteString.Char8 as BS8 (pack)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (pack)
import qualified Options.Applicative as O
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.ProtocolVersions (alonzoPV, vasilPV)


-- | Parser for network ID.
parseNetworkId :: O.Mod O.OptionFields NetworkId -> O.Parser NetworkId
parseNetworkId network =
  parseMainnet <|> parseTestnet
    where
      parseMainnet = O.flag' Mainnet (O.long "mainnet" <> O.help "Execute on mainnet.")
      parseTestnet = O.option (Testnet . NetworkMagic . toEnum <$> O.auto) (O.long "testnet-magic" <> O.metavar "INTEGER" <> network <> O.help "Network magic. Defaults to the CARDANO_TESTNET_MAGIC environment variable's value.")


-- | Parser for stake address reference.
parseStakeAddressReference :: O.ReadM StakeAddressReference
parseStakeAddressReference =
  O.eitherReader
    $ \s ->
      case deserialiseAddress AsStakeAddress $ T.pack s of
        Just (StakeAddress _ credential) -> Right . StakeAddressByValue . fromShelleyStakeCredential $ credential
        Nothing                          -> Left "Invalid Bech32 stake address."


-- | Parser for slot number.
parseSlotNo :: O.ReadM SlotNo
parseSlotNo = SlotNo <$> O.auto


-- | Parser for slot number.
parseSlot :: O.ReadM Slot
parseSlot = Slot <$> O.auto


-- | Parser for POSIXTime.
parsePOSIXTime :: O.ReadM POSIXTime
parsePOSIXTime = POSIXTime <$> O.auto


-- | Parser for Timeout.
parseTimeout :: O.ReadM SomeTimeout
parseTimeout = O.eitherReader $ \s -> case s =~ "^([1-9][[:digit:]]*|0)([s|m|h|d|w]?)$" of
    [[_, instant, ""]] -> do
      timeout <- readEither instant
      pure $ AbsoluteTimeout (fromInteger timeout)
    [[_, instant, unit]] -> do
      duration <- readEither instant
      pure $ RelativeTimeout $ unitMultiplier unit * fromInteger duration
    result -> Left $ "Invalid timeout value. " <> show result <> timeoutHelpMsg
  where
    unitMultiplier "m" = 60
    unitMultiplier "h" = 60 * 60
    unitMultiplier "d" = 60 * 60 * 24
    unitMultiplier "w" = 60 * 60 * 24 * 7
    unitMultiplier _   = 1


timeoutHelpMsg :: String
timeoutHelpMsg = "POSIX milliseconds or duration: `INTEGER[s|m|d|w|h]`."

-- | Parser for currency symbol.
parseCurrencySymbol :: O.ReadM CurrencySymbol
parseCurrencySymbol = CurrencySymbol <$> parseByteString


-- | Parser for `TxIn`.
parseTxIn :: O.ReadM TxIn
parseTxIn =
  O.eitherReader
    $ \s ->
      case s =~ "^([[:xdigit:]]{64})#([1-9][[:digit:]]*|0)$" of
        [[_, txId, txIx]] -> do
                               txId' <- readTxIdEither txId
                               txIx' <- TxIx <$> readEither txIx
                               pure $ TxIn txId' txIx'
        _                 -> Left "Invalid transaction input."


-- | Parser for `TxId`.
parseTxId :: O.ReadM TxId
parseTxId = O.eitherReader readTxIdEither


-- | Reader for `TxId`.
readTxIdEither :: String              -- ^ The string to be read.
               -> Either String TxId  -- ^ Either the transaction ID or an error message.
readTxIdEither s =
  case deserialiseFromRawBytesHex AsTxId $ BS8.pack s of
    Left msg   -> Left ("Invalid transaction ID: " <> show msg)
    Right txId -> Right txId


-- | Parser for `TxIx`.
parseTxIx :: O.ReadM TxIx
parseTxIx = TxIx <$> O.auto


-- | Parser for `TxOut` information.
parseTxOut :: IsShelleyBasedEra era => O.ReadM (AddressInEra era, Value)
parseTxOut =
  O.eitherReader
    $ \s ->
      case splitOn "+" s of
        address : lovelace' : tokens -> do
                                          address' <- readAddressEither address
                                          lovelace'' <- readLovelaceEither lovelace'
                                          tokens' <- mapM readAssetValueEither tokens
                                          pure (address', lovelace'' <> mconcat tokens')
        _                            -> Left "Invalid transaction output."


-- | Parser for `Value`.
parseValue :: O.ReadM Value
parseValue =
  O.eitherReader
    $ \s ->
      case splitOn "+" s of
        lovelace' : tokens -> do
                                lovelace'' <- readLovelaceEither lovelace'
                                tokens' <- mapM readAssetValueEither tokens
                                pure $ lovelace'' <> mconcat tokens'
        _                  -> Left "Invalid transaction output."


-- | Parser for lovelace `Value`.
parseLovelaceValue :: O.ReadM Value
parseLovelaceValue = O.eitherReader readLovelaceEither


-- | Reader for lovelace `Value`.
readLovelaceEither :: String               -- ^ The string to be read.
                   -> Either String Value  -- ^ Either the lovelace value or an error message.
readLovelaceEither =
  fmap (lovelaceToValue . quantityToLovelace . Quantity)
    . readEither


-- | Reader for an asset and its value.
readAssetValueEither :: String               -- ^ The string to be read.
                     -> Either String Value  -- ^ Either the value or an error message.
readAssetValueEither s =
  case break (== ' ') s of
    (amount, ' ' : token) -> do
                               token' <- readAssetIdEither $ dropWhile (== ' ') token
                               amount' <- Quantity <$> readEither amount
                               pure $ valueFromList [(token', amount')]
    _                     -> Left "Invalid asset value."


-- | Parser for `AssetId`.
parseAssetId :: O.ReadM AssetId
parseAssetId = O.eitherReader readAssetIdEither


-- | Reader for `AssetId`.
readAssetIdEither :: String                 -- ^ The string to be read.
                  -> Either String AssetId  -- ^ Either the asset ID or an error message.
readAssetIdEither s =
  case s =~ "^([[:xdigit:]]{56})\\.([^+]+)$" of
    [[_, symbol, name]] -> case deserialiseFromRawBytesHex AsPolicyId $ BS8.pack symbol of
                             Right symbol' -> Right
                                                $ AssetId symbol'
                                                  (AssetName . BS8.pack $ name)
                             Left msg      -> Left ("Invalid policy ID: " <> show msg)
    _                   -> Left "Invalid token."



-- | Parser for `AddressInEra era`.
parseAddress :: IsShelleyBasedEra era => O.ReadM (AddressInEra era)
parseAddress = O.eitherReader readAddressEither


-- | Parser for `AddressInEra era`.
readAddressEither :: forall era
                   . IsShelleyBasedEra era
                  => String                    -- ^ The string to be read.
                  -> Either String (AddressInEra era)  -- ^ Either the address or an error message.
readAddressEither s = do
  era <- eraAsType
  case deserialiseAddress (AsAddressInEra era) $ T.pack s of
    Nothing      -> Left "Invalid address."
    Just address -> Right address
  where
    eraAsType :: Either String (AsType era)
    eraAsType = case shelleyBasedEra :: ShelleyBasedEra era of
      ShelleyBasedEraAlonzo  -> Right AsAlonzo
      ShelleyBasedEraBabbage -> Right AsBabbage
      era                    -> Left $ "unsupported era: " <> show era


-- | Parser for `Party`.
parseParty :: O.ReadM Party
parseParty =
        O.eitherReader readPartyAddressEither
    <|> O.eitherReader readPartyRoleEither
    <|> O.readerError "Invalid party."


-- | Reader for `Party` `Address`.
readPartyAddressEither :: String               -- ^ The string to be read.
                       -> Either String Party  -- ^ Either the address party or an error message.
readPartyAddressEither s =
  case deserialiseAddressBech32 $ T.pack s of
    Just (network, address) -> Right $ Address network address
    _                       -> Left "Invalid Bech32 address for party."


-- | Reader for `Party` `Role`.
readPartyRoleEither :: String               -- ^ The string to be read.
                    -> Either String Party  -- ^ Either the role party or an error message.
readPartyRoleEither s =
  if length s <= 32
    then Right . Role . TokenName . toBuiltin $ BS8.pack s
    else Left "Invalid role name for party."


-- | Parser for `Token`.
parseToken :: O.ReadM Token
parseToken = O.eitherReader readTokenEither


-- | Reader for `Token`.
readTokenEither :: String               -- ^ The string to be read.
                -> Either String Token  -- ^ Either the token or an error message.
readTokenEither s =
  case s =~ "^([[:xdigit:]]{56})\\.([^+]+)$" of
    [[_, symbol, name]] -> case Base16.decode $ BS8.pack symbol of
                             Right symbol' -> Right
                                                $ Token
                                                  (CurrencySymbol . toBuiltin $ symbol')
                                                  (TokenName . toBuiltin . BS8.pack $ name)
                             Left message -> Left message
    _                   -> Left "Invalid token."



-- | Parser for `TokenName`.
parseTokenName :: O.ReadM TokenName
parseTokenName = O.eitherReader $ Right . readTokenName


-- | Reader for `TokenName`.
readTokenName :: String
              -> TokenName
readTokenName = TokenName . toBuiltin . BS8.pack


-- | Parser for `BuiltinByteString`.
parseByteString :: O.ReadM BuiltinByteString
parseByteString =
  O.eitherReader
    $ \s ->
      case Base16.decode $ BS8.pack s of
        Left  message  -> Left message
        Right currency -> Right . toBuiltin $ currency



-- | Parse input to a contract.
parseInput :: O.Parser Input
parseInput = NormalInput <$> parseInputContent


-- | Parse input to a contract.
parseInputContent :: O.Parser InputContent
parseInputContent =
  parseDeposit <|> parseChoice <|> parseNotify
    where
      parseDeposit =
        IDeposit
          <$> O.option parseParty (O.long "deposit-account" <> O.metavar "PARTY"                                         <> O.help "The account for the deposit."          )
          <*> O.option parseParty (O.long "deposit-party"   <> O.metavar "PARTY"                                         <> O.help "The party making the deposit."         )
          <*> O.option parseToken (O.long "deposit-token"   <> O.metavar "TOKEN"   <> O.value (Token adaSymbol adaToken) <> O.help "The token being deposited, if not Ada.")
          <*> O.option O.auto     (O.long "deposit-amount"  <> O.metavar "INTEGER"                                       <> O.help "The amount of token being deposited."  )
      parseChoice =
        IChoice
          <$> (
                ChoiceId
                  <$> O.strOption         (O.long "choice-name"   <> O.metavar "NAME"    <> O.help "The name of the choice made.")
                  <*> O.option parseParty (O.long "choice-party"  <> O.metavar "PARTY"   <> O.help "The party making the choice.")
              )
          <*> O.option O.auto             (O.long "choice-number" <> O.metavar "INTEGER" <> O.help "The number chosen."          )
      parseNotify =
        INotify
          <$ O.flag' () (O.long "notify" <> O.help "Notify the contract.")


-- | Parse a URL.
parseUrl :: O.ReadM BaseUrl
parseUrl =
  O.eitherReader
    $ either (Left . show) Right
    . parseBaseUrl


-- | Parse a role.
parseRole :: IsShelleyBasedEra era => O.ReadM (TokenName, AddressInEra era)
parseRole =
  O.eitherReader
    $ \s ->
      case splitOn "=" s of
        [name, address] -> do
                             address' <- readAddressEither address
                             pure (readTokenName name, address')
        _               -> Left "Invalid role assigment."


-- | Parse an address query.
parseOutputQuery :: O.Parser (Maybe (OutputQuery era (OutputQueryResult era)))
parseOutputQuery =
  Just <$> parseLovelaceOnly <|> Just <$> parseAssetOnly <|> parseAllOutput
    where
      parseAllOutput =
        Nothing
          <$ O.flag' () (O.long "all" <> O.help "Report all output.")
      parseLovelaceOnly =
        LovelaceOnly . (<=) . Lovelace
          <$> O.option O.auto (O.long "lovelace-only" <> O.metavar "LOVELACE" <> O.help "The minimum Lovelace that must be the sole asset in the output value.")
      parseAssetOnly =
        AssetOnly
          <$> O.option parseAssetId (O.long "asset-only" <> O.metavar "CURRENCY_SYMBOL.TOKEN_NAME" <> O.help "The current symbol and token name for the sole native asset in the value.")


parseProtocolVersion :: O.ReadM ProtocolVersion
parseProtocolVersion = O.eitherReader \case
  "alonzo" -> pure alonzoPV
  "vasil"  -> pure vasilPV
  s        -> Left $ "Invalid protocol version: " <> s <> ". Expecting [alonzo|vasil]."


protocolVersionOpt :: O.Parser ProtocolVersion
protocolVersionOpt = fromMaybe vasilPV <$> (O.optional $ O.option parseProtocolVersion (O.long "protocol-version" <> O.metavar "PROTOCOL_VERSION" <> O.help "Protocol version: [alonzo|vasil]"))


requiredSignerOpt :: O.Parser SigningKeyFile
requiredSignerOpt = SigningKeyFile <$> O.strOption (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "File containing a required signing key.")


requiredSignersOpt :: O.Parser [SigningKeyFile]
requiredSignersOpt =  map SigningKeyFile <$> (O.some . O.strOption) (O.long "required-signer" <> O.metavar "SIGNING_FILE" <> O.help "File containing a required signing key.")

parseWallet :: IsShelleyBasedEra era => O.ReadM (AddressInEra era, SigningKeyFile)
parseWallet =
  O.eitherReader
    $ splitOn ":" >>> \case
        [address, signingKeyFile] -> do
            address' <- readAddressEither address
            pure (address', SigningKeyFile signingKeyFile)
        _  -> Left "Expecting address and signing key file path: ADDRESS:SIGNING_FILE"

walletOpt :: IsShelleyBasedEra era => O.Mod O.OptionFields (AddressInEra era, SigningKeyFile) -> O.Parser (AddressInEra era, SigningKeyFile)
walletOpt = O.option parseWallet

txBodyFileOpt :: O.Parser TxBodyFile
txBodyFileOpt = TxBodyFile <$> O.strOption (O.long "out-file" <> O.metavar "FILE"         <> O.help "Output file for transaction body.")


publishingStrategyOpt :: forall era. C.IsShelleyBasedEra era => O.Parser (PublishingStrategy era)
publishingStrategyOpt =
      PublishAtAddress <$> O.option parseAddress                 (O.long "at-address"                     <> O.metavar "ADDRESS"          <> O.help "Publish script at a given address. This is a default strategy which uses change address as a destination.")
  <|> PublishPermanently <$> O.option parseStakeAddressReference (O.long "permanently"                    <> O.metavar "STAKING_ADDRESS"  <> O.help "Publish permanently at unspendable script address staking the min. ADA value.")
  <|> O.flag' (PublishPermanently C.NoStakeAddress)              (O.long "permanently-without-staking"                                    <> O.help "Publish permanently at unspendable script address without min. ADA staking.")

