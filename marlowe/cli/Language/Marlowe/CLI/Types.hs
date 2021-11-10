
module Language.Marlowe.CLI.Types (
  Command(..)
, ValidatorInfo(..)
, DatumInfo(..)
, RedeemerInfo(..)
) where


import           Cardano.Api                (AddressInEra, Lovelace, NetworkId, SlotNo, StakeAddressReference)
import           Data.Aeson                 (Value)
import           Language.Marlowe.Semantics (MarloweData (..))
import           Ledger.Typed.Scripts       (TypedValidator)
import           Plutus.V1.Ledger.Api       (Datum, DatumHash, ExBudget, PubKeyHash, Redeemer, Script, ValidatorHash)


data ValidatorInfo era =
  ValidatorInfo
  {
    viValidator :: TypedValidator MarloweData
  , viScript    :: Script
  , viHash      :: ValidatorHash
  , viAddress   :: AddressInEra era
  , viSize      :: Int
  , viCost      :: ExBudget
  }
    deriving (Eq, Show)


data DatumInfo =
  DatumInfo
  {
    diDatum :: Datum
  , diJson  :: Value
  , diHash  :: DatumHash
  , diSize  :: Int
  }
    deriving (Eq, Show)


data RedeemerInfo =
  RedeemerInfo
  {
    riRedeemer :: Redeemer
  , riJson     :: Value
  , riSize     :: Int
  }
    deriving (Eq, Show)


data Command =
    Export
    {
      network         :: Maybe NetworkId
    , stake           :: Maybe StakeAddressReference
    , validatorFile   :: FilePath
    , printAddress    :: Bool
    , accountHash     :: PubKeyHash
    , accountLovelace :: Lovelace
    , minimumSlot'    :: SlotNo
    , datumFile       :: FilePath
    , minimumSlot     :: SlotNo
    , maximumSlot     :: SlotNo
    , redeemerFile    :: FilePath
    , printHash       :: Bool
    , printStats      :: Bool
    }
  | ExportValidator
    {
      network       :: Maybe NetworkId
    , stake         :: Maybe StakeAddressReference
    , validatorFile :: FilePath
    , printAddress  :: Bool
    , printHash     :: Bool
    , printStats    :: Bool
    }
  | ExportDatum
    {
      accountHash     :: PubKeyHash
    , accountLovelace :: Lovelace
    , minimumSlot'    :: SlotNo
    , datumFile       :: FilePath
    , printHash       :: Bool
    , printStats      :: Bool
    }
  | ExportRedeemer
    {
      minimumSlot  :: SlotNo
    , maximumSlot  :: SlotNo
    , redeemerFile :: FilePath
    , printStats   :: Bool
    }
    deriving (Eq, Show)
