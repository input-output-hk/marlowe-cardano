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


module Language.Marlowe.CLI.Command.Parse (
-- * Parsers
  parseAddressAny
, parseByteString
, parseCurrencySymbol
, parseInput
, parseLovelaceValue
, parseNetworkId
, parseParty
, parseRoleOutput
, parseSlot
, parseSlotNo
, parseStakeAddressReference
, parseToken
, parseTokenName
, parseTxId
, parseTxIn
, parseTxIx
, parseTxOut
, parseValue
) where


import           Cardano.Api                     (AddressAny, AsType (AsAddressAny, AsPolicyId, AsStakeAddress, AsTxId),
                                                  AssetId (..), AssetName (..), NetworkId (..), NetworkMagic (..),
                                                  Quantity (..), SlotNo (..), StakeAddressReference (..), TxId (..),
                                                  TxIn (..), TxIx (..), Value, deserialiseAddress,
                                                  deserialiseFromRawBytesHex, lovelaceToValue, quantityToLovelace,
                                                  valueFromList)
import           Cardano.Api.Shelley             (StakeAddress (..), fromShelleyStakeCredential)
import           Control.Applicative             ((<|>))
import           Data.List.Split                 (splitOn)
import           Language.Marlowe.SemanticsTypes (ChoiceId (..), Input (..), Party (..), Token (..))
import           Plutus.V1.Ledger.Ada            (adaSymbol, adaToken)
import           Plutus.V1.Ledger.Api            (BuiltinByteString, CurrencySymbol (..), PubKeyHash (..),
                                                  TokenName (..), toBuiltin)
import           Plutus.V1.Ledger.Slot           (Slot (..))
import           Text.Read                       (readEither)
import           Text.Regex.Posix                ((=~))

import qualified Data.ByteString.Base16          as Base16 (decode)
import qualified Data.ByteString.Char8           as BS8 (pack)
import qualified Data.Text                       as T (pack)
import qualified Options.Applicative             as O


-- | Parser for network ID.
parseNetworkId :: O.ReadM NetworkId
parseNetworkId = Testnet . NetworkMagic . toEnum <$> O.auto


-- | Parser for stake address reference.
parseStakeAddressReference :: O.ReadM StakeAddressReference
parseStakeAddressReference =
  O.eitherReader
    $ \s ->
      case deserialiseAddress AsStakeAddress $ T.pack s of
        Just (StakeAddress _ credential) -> Right . StakeAddressByValue . fromShelleyStakeCredential $ credential
        Nothing                          -> Left "Invalid stake address."


-- | Parser for slot number.
parseSlotNo :: O.ReadM SlotNo
parseSlotNo = SlotNo <$> O.auto


-- | Parser for slot number.
parseSlot :: O.ReadM Slot
parseSlot = Slot <$> O.auto


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
    Nothing   -> Left "Invalid transaction ID."
    Just txId -> Right txId


-- | Parser for `TxIx`.
parseTxIx :: O.ReadM TxIx
parseTxIx = TxIx <$> O.auto


-- | Parser for `TxOut` information.
parseTxOut :: O.ReadM (AddressAny, Value)
parseTxOut =
  O.eitherReader
    $ \s ->
      case splitOn "+" s of
        address : lovelace : tokens -> do
                                         address' <- readAddressAnyEither address
                                         lovelace' <- readLovelaceEither lovelace
                                         tokens' <- mapM readAssetValueEither tokens
                                         pure (address', lovelace' <> mconcat tokens')
        _                           -> Left "Invalid transaction output."


-- | Parser for role output information.
parseRoleOutput :: O.ReadM (TokenName, Value)
parseRoleOutput =
  O.eitherReader
    $ \s ->
      case splitOn "+" s of
        role : lovelace : tokens -> do
                                      let
                                         role' = readTokenName role
                                      lovelace' <- readLovelaceEither lovelace
                                      tokens' <- mapM readAssetValueEither tokens
                                      pure (role', lovelace' <> mconcat tokens')
        _                        -> Left "Invalid role output."


-- | Parser for `Value`.
parseValue :: O.ReadM Value
parseValue =
  O.eitherReader
    $ \s ->
      case splitOn "+" s of
        lovelace : tokens -> do
                               lovelace' <- readLovelaceEither lovelace
                               tokens' <- mapM readAssetValueEither tokens
                               pure $ lovelace' <> mconcat tokens'
        _                 -> Left "Invalid transaction output."


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
  case words s of
    [amount, token] -> do
                         token' <- readAssetIdEither token
                         amount' <- Quantity <$> readEither amount
                         pure $ valueFromList [(token', amount')]
    _               -> Left "Invalid asset value."


-- | Reader for `AssetId`.
readAssetIdEither :: String                 -- ^ The string to be read.
                  -> Either String AssetId  -- ^ Either the asset ID or an error message.
readAssetIdEither s =
  case s =~ "^([[:xdigit:]]{56})\\.([^+]+)$" of
    [[_, symbol, name]] -> case deserialiseFromRawBytesHex AsPolicyId $ BS8.pack symbol of
                             Just symbol' -> Right
                                                $ AssetId symbol'
                                                  (AssetName . BS8.pack $ name)
                             Nothing      -> Left "Invalid policy ID."
    _                   -> Left "Invalid token."



-- | Parser for `AddressAny`.
parseAddressAny :: O.ReadM AddressAny
parseAddressAny = O.eitherReader readAddressAnyEither


-- | Parser for `AddressAny`.
readAddressAnyEither :: String                    -- ^ The string to be read.
                     -> Either String AddressAny  -- ^ Either the address or an error message.
readAddressAnyEither s =
  case deserialiseAddress AsAddressAny $ T.pack s of
    Nothing      -> Left "Invalid address."
    Just address -> Right address


-- | Parser for `Party`.
parseParty :: O.ReadM Party
parseParty =
        O.eitherReader readPartyPkEither
    <|> O.eitherReader readPartyRoleEither
    <|> O.readerError "Invalid party."


-- | Reader for `Party` `PK`.
readPartyPkEither :: String               -- ^ The string to be read.
                  -> Either String Party  -- ^ Either the public key hash role or an error message.
readPartyPkEither s =
  case s =~ "^PK=([[:xdigit:]]{56})$" of
    [[_, pubKeyHash]] -> case Base16.decode $ BS8.pack pubKeyHash of
                           Right pubKeyHash' -> Right . PK . PubKeyHash . toBuiltin $ pubKeyHash'
                           Left  message     -> Left message
    _                 -> Left "Invalid public key hash for party."


-- | Reader for `Party` `Role`.
readPartyRoleEither :: String               -- ^ The string to be read.
                    -> Either String Party  -- ^ Either the party role or an error message.
readPartyRoleEither s =
  case s =~ "^Role=(.+)$" of
    [[_, role]] -> Right . Role . TokenName . toBuiltin . BS8.pack $ role
    _           -> Left "Invalid role for party."


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
parseTokenName = readTokenName <$> O.auto


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
parseInput =
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
      parseNotify = INotify <$ O.flag' () (O.long "notify" <> O.help "Notify the contract.")
