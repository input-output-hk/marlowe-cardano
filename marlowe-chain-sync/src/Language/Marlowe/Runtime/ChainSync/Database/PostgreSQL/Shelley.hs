{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Shelley where

import qualified Cardano.Crypto.Hash as Hash
import Cardano.Ledger.Address (BootstrapAddress (..), putPtr, serialiseAddr)
import Cardano.Ledger.BaseTypes (TxIx (..), shelleyProtVer)
import Cardano.Ledger.Binary (serialize')
import Cardano.Ledger.Crypto
import Cardano.Ledger.SafeHash (SafeToHash (..))
import Cardano.Ledger.Shelley
import Cardano.Ledger.Shelley.API
import Data.Binary.Put (runPut)
import Data.Bits ((.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (Foldable (..))
import Data.Int
import qualified Data.Set as Set
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Byron (AddressFields (..), byronAddressFields)
import Language.Marlowe.Runtime.ChainSync.Database.PostgreSQL.Types

shelleyTxRow :: Int64 -> Bytea -> Bytea -> ShelleyTx (ShelleyEra StandardCrypto) -> TxRowGroup
shelleyTxRow slotNo blockHash txId ShelleyTx{..} =
  ( TxRow
      { blockHash
      , txId
      , slotNo
      , validityUpperBound = Just $ convertSlotNo $ stbTTL body
      , validityLowerBound = Nothing
      , metadata =
          -- This is an awkward workaround that is summarized by this comment
          -- https://input-output-hk.github.io/cardano-api/cardano-api/lib_internal/src/Cardano.Api.TxMetadata.html#line-118
          mapStrictMaybe
            ( \(ShelleyTxAuxData md) ->
                Bytea $ serialize' shelleyProtVer md
            )
            auxiliaryData
      , isValid = SqlBool True
      }
  , shelleyTxInRow slotNo txId <$> Set.toAscList (stbInputs body)
  , zipWith (shelleyTxOutRow slotNo txId) [0 ..] $ toList $ stbOutputs body
  , []
  )

mapStrictMaybe :: (a -> b) -> StrictMaybe a -> Maybe b
mapStrictMaybe f = \case
  SNothing -> Nothing
  SJust a -> Just $ f a

shelleyTxInRow :: Int64 -> Bytea -> TxIn StandardCrypto -> TxInRow
shelleyTxInRow slotNo txInId (TxIn txId txIx) =
  TxInRow
    { txOutId = originalBytea $ unTxId txId
    , txOutIx = convertTxIx txIx
    , txInId
    , slotNo
    , redeemerDatumBytes = Nothing
    , isCollateral = SqlBool False
    }

convertTxIx :: TxIx -> Int16
convertTxIx (TxIx txIx) = fromIntegral txIx

shelleyTxOutRow :: Int64 -> Bytea -> Int16 -> ShelleyTxOut (ShelleyEra StandardCrypto) -> TxOutRowGroup
shelleyTxOutRow slotNo txId txIx (ShelleyTxOut addr value) =
  ( TxOutRow
      { txId
      , txIx
      , slotNo
      , address
      , lovelace = coinToLovelace value
      , datumHash = Nothing
      , datumBytes = Nothing
      , isCollateral = SqlBool False
      , addressHeader
      , addressPaymentCredential
      , addressStakeAddressReference
      }
  , []
  )
  where
    AddressFields{..} = addressFields addr

coinToLovelace :: Coin -> Int64
coinToLovelace = fromInteger . unCoin

addressFields :: Addr StandardCrypto -> AddressFields
addressFields = \case
  Addr network payment stake ->
    AddressFields
      { address = Bytea $ serialiseAddr $ Addr network payment stake
      , addressHeader =
          let networkTag = case network of
                Testnet -> 0b00000000
                Mainnet -> 0b00000001
              headerType = case (payment, stake) of
                (KeyHashObj{}, StakeRefBase KeyHashObj{}) -> 0b00000000
                (ScriptHashObj{}, StakeRefBase KeyHashObj{}) -> 0b00010000
                (KeyHashObj{}, StakeRefBase ScriptHashObj{}) -> 0b00100000
                (ScriptHashObj{}, StakeRefBase ScriptHashObj{}) -> 0b00110000
                (KeyHashObj{}, StakeRefPtr{}) -> 0b01000000
                (ScriptHashObj{}, StakeRefPtr{}) -> 0b01010000
                (KeyHashObj{}, StakeRefNull) -> 0b01100000
                (ScriptHashObj{}, StakeRefNull) -> 0b01110000
           in Bytea $ BS.pack [headerType .|. networkTag]
      , addressPaymentCredential = Just case payment of
          KeyHashObj (KeyHash hash) -> hashToBytea hash
          ScriptHashObj (ScriptHash hash) -> hashToBytea hash
      , addressStakeAddressReference = case stake of
          StakeRefBase (KeyHashObj (KeyHash hash)) -> Just $ hashToBytea hash
          StakeRefBase (ScriptHashObj (ScriptHash hash)) -> Just $ hashToBytea hash
          StakeRefPtr ptr -> Just $ Bytea $ BL.toStrict $ runPut $ putPtr ptr
          StakeRefNull -> Nothing
      }
  AddrBootstrap (BootstrapAddress addr) -> byronAddressFields addr

hashToBytea :: Hash.Hash al a -> Bytea
hashToBytea = Bytea . Hash.hashToBytes

originalBytea :: (SafeToHash a) => a -> Bytea
originalBytea = Bytea . originalBytes
