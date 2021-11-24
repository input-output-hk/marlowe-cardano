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


module Language.Marlowe.CLI.Parse (
-- * Parsers
  parseAddressAny
, parseCurrencySymbol
, parseNetworkId
, parseSlotNo
, parseStakeAddressReference
, parseTxId
, parseTxIn
, parseTxIx
, parseTxOut
, parseValue
) where


import           Cardano.Api            (AddressAny, AsType (AsAddressAny, AsStakeAddress, AsTxId), NetworkId (..),
                                         NetworkMagic (..), Quantity (..), SlotNo (..), StakeAddressReference (..),
                                         TxId (..), TxIn (..), TxIx (..), Value, deserialiseAddress,
                                         deserialiseFromRawBytesHex, lovelaceToValue, quantityToLovelace)
import           Cardano.Api.Shelley    (StakeAddress (..), fromShelleyStakeCredential)
import           Plutus.V1.Ledger.Api   (CurrencySymbol (..), toBuiltin)

import qualified Data.ByteString.Base16 as Base16 (decode)
import qualified Data.ByteString.Char8  as BS8 (pack)
import qualified Data.Text              as T (pack)
import qualified Options.Applicative    as O


-- | Parser for network ID.
parseNetworkId :: O.ReadM NetworkId
parseNetworkId = Testnet . NetworkMagic . toEnum <$> O.auto


-- | Parser for stake address reference.
parseStakeAddressReference :: O.ReadM StakeAddressReference
parseStakeAddressReference =
  O.eitherReader
    $ \s ->
      case deserialiseAddress AsStakeAddress $ T.pack s of
        Nothing                          -> Left "Invalid stake address."
        Just (StakeAddress _ credential) -> Right . StakeAddressByValue $ fromShelleyStakeCredential credential


-- | Parser for slot number.
parseSlotNo :: O.ReadM SlotNo
parseSlotNo = SlotNo <$> O.auto


-- | Parser for currency symbol.
parseCurrencySymbol :: O.ReadM CurrencySymbol
parseCurrencySymbol =
  O.eitherReader
    $ \s ->
      case Base16.decode $ BS8.pack s of
        Left  message  -> Left message
        Right currency -> Right . CurrencySymbol . toBuiltin $ currency


-- | Parser for `TxIn`.
parseTxIn :: O.ReadM TxIn
parseTxIn =
  O.eitherReader -- FIXME: Use a monadic approach.
    $ \s ->
      do
        (txId, txIx) <-
           case break (== '#') s of
             (_ , [])           -> Left "Missing transaction index."
             (txId', _ : txIx') -> Right (txId', txIx')
        txId'' <-
          case deserialiseFromRawBytesHex AsTxId $ BS8.pack txId of
            Nothing      -> Left "Invalid transaction ID."
            Just txId''' -> Right txId'''
        txIx'' <-
          case reads txIx of
            [(txIx''', "")] -> Right $ TxIx txIx'''
            _               -> Left "Invalid transaction index."
        pure $ TxIn txId'' txIx''


-- | Parser for `TxId`.
parseTxId :: O.ReadM TxId
parseTxId =
  O.eitherReader
    $ \s ->
      case deserialiseFromRawBytesHex AsTxId $ BS8.pack s of
        Nothing   -> Left "Invalid transaction ID."
        Just txId -> Right txId


-- | Parser for `TxIx`.
parseTxIx :: O.ReadM TxIx
parseTxIx = TxIx <$> O.auto


-- | Parser for `TxOut` information.
parseTxOut :: O.ReadM (AddressAny, Value)  -- FIXME: Also parse the datum of the eUTxO.
parseTxOut =
  O.eitherReader -- FIXME: Use a monadic approach.
    $ \s ->
      do
        (address, value) <-
           case break (== '+') s of
             (_ , [])               -> Left "Missing transaction index."
             (address', _ : value') -> Right (address', value')
        address'' <-
          case deserialiseAddress AsAddressAny $ T.pack address of
            Nothing         -> Left "Invalid address."
            Just address''' -> Right address'''
        value'' <-
          case reads value of
            [(value''', "")] -> Right value'''
            _                -> Left "Invalid value."
        pure (address'', lovelaceToValue . quantityToLovelace . Quantity $ value'')


-- | Parser for `Value`.
parseValue :: O.ReadM Value
parseValue = lovelaceToValue . quantityToLovelace . Quantity <$> O.auto


-- | Parser for `AddressAny`.
parseAddressAny :: O.ReadM AddressAny
parseAddressAny =
  O.eitherReader
    $ \s ->
      case deserialiseAddress AsAddressAny $ T.pack s of
        Nothing      -> Left "Invalid address."
        Just address -> Right address
