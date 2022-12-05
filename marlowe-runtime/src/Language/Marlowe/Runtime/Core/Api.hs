{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Language.Marlowe.Runtime.Core.Api
  where

import Data.Aeson (FromJSON(..), Result(..), ToJSON(..), Value(..), eitherDecode, encode)
import Data.Aeson.Types (Parser, parse, parseFail)
import Data.Binary (Binary(..), Get, Put)
import Data.Binary.Get (getWord32be)
import Data.Binary.Put (putWord32be)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import Data.Type.Equality (TestEquality(..), type (:~:)(Refl))
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api
  ( BlockHeader
  , TokenName(..)
  , TxId(..)
  , TxOutRef(..)
  , getUTCTime
  , parseTxOutRef
  , putUTCTime
  , renderTxOutRef
  , unPolicyId
  )
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Plutus.V1.Ledger.Api as Plutus
import qualified Plutus.V1.Ledger.Value as Plutus

-- | The ID of a contract is the TxId and TxIx of the UTxO that first created
-- the contract.
newtype ContractId = ContractId { unContractId :: TxOutRef }
  deriving stock (Show, Eq, Ord, Generic)
  deriving newtype (IsString)
  deriving anyclass (Binary)

parseContractId :: String -> Maybe ContractId
parseContractId = fmap ContractId . parseTxOutRef . T.pack

renderContractId :: ContractId -> Text
renderContractId = renderTxOutRef . unContractId

instance ToJSON ContractId where
  toJSON = String . renderContractId

data MarloweVersionTag
  = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1


instance TestEquality MarloweVersion where
  testEquality MarloweV1 MarloweV1 = Just Refl

assertVersionsEqual :: MarloweVersion v -> MarloweVersion v' -> v :~: v'
assertVersionsEqual v1 v2 = fromMaybe
  (error $ "getNextSteps: Marlowe version mismatch. Expected " <> show v1 <> ", got " <> show v2)
  (testEquality v1 v2)

deriving instance Show (MarloweVersion v)
deriving instance Eq (MarloweVersion v)
deriving instance Ord (MarloweVersion v)

class IsMarloweVersion (v :: MarloweVersionTag) where
  type Contract v :: *
  type TransactionError v :: *
  type Datum v :: *
  type Redeemer (v :: MarloweVersionTag) :: *
  type PayoutDatum v :: *
  marloweVersion :: MarloweVersion v

instance IsMarloweVersion 'V1 where
  type Contract 'V1 = V1.Contract
  type TransactionError 'V1 = V1.TransactionError
  type Datum 'V1 = V1.MarloweData
  type Redeemer 'V1 = [V1.Input]
  type PayoutDatum 'V1 = Chain.AssetId
  marloweVersion = MarloweV1

data Transaction v = Transaction
  { transactionId      :: TxId
  , contractId         :: ContractId
  , blockHeader        :: BlockHeader
  , validityLowerBound :: UTCTime
  , validityUpperBound :: UTCTime
  , redeemer           :: Redeemer v
  , output             :: TransactionOutput v
  }

deriving instance Show (Transaction 'V1)
deriving instance Eq (Transaction 'V1)

instance Binary (Transaction 'V1) where
  put Transaction{..} = do
    put transactionId
    put contractId
    put blockHeader
    putUTCTime validityLowerBound
    putUTCTime validityUpperBound
    putRedeemer MarloweV1 redeemer
    put output
  get = Transaction
    <$> get
    <*> get
    <*> get
    <*> getUTCTime
    <*> getUTCTime
    <*> getRedeemer MarloweV1
    <*> get

data TransactionOutput v = TransactionOutput
  { payouts      :: Map Chain.TxOutRef (Payout v)
  , scriptOutput :: Maybe (TransactionScriptOutput v)
  }

deriving instance Show (TransactionOutput 'V1)
deriving instance Eq (TransactionOutput 'V1)

instance Binary (TransactionOutput 'V1) where
  put TransactionOutput{..} = do
    put payouts
    put scriptOutput
  get = TransactionOutput <$> get <*> get

data Payout v = Payout
  { address :: Chain.Address
  , assets :: Chain.Assets
  , datum :: PayoutDatum v
  }

deriving instance Show (Payout 'V1)
deriving instance Eq (Payout 'V1)

instance Binary (Payout 'V1) where
  put Payout{..} = do
    put address
    put assets
    put datum
  get = Payout <$> get <*> get <*> get

data TransactionScriptOutput v = TransactionScriptOutput
  { address :: Chain.Address
  , assets :: Chain.Assets
  , utxo  :: TxOutRef
  , datum :: Datum v
  }

deriving instance Show (TransactionScriptOutput 'V1)
deriving instance Eq (TransactionScriptOutput 'V1)

instance Binary (TransactionScriptOutput 'V1) where
  put TransactionScriptOutput{..} = do
    put address
    put assets
    put utxo
    putDatum MarloweV1 datum
  get = TransactionScriptOutput <$> get <*> get <*> get <*> getDatum MarloweV1

data SomeMarloweVersion = forall v. SomeMarloweVersion (MarloweVersion v)

instance Eq SomeMarloweVersion where
  SomeMarloweVersion MarloweV1 == SomeMarloweVersion MarloweV1 = True

instance Ord SomeMarloweVersion where
  compare (SomeMarloweVersion MarloweV1) (SomeMarloweVersion MarloweV1) = EQ

instance Bounded SomeMarloweVersion where
  minBound = SomeMarloweVersion MarloweV1
  maxBound = SomeMarloweVersion MarloweV1

instance Enum SomeMarloweVersion where
  toEnum = \case
    0 -> SomeMarloweVersion MarloweV1
    _ -> error "toEnum: value out of range of Marlowe versions"
  fromEnum (SomeMarloweVersion version) = case version of
    MarloweV1 -> 0

instance Show SomeMarloweVersion where
  showsPrec p (SomeMarloweVersion version) = showParen (p >= 11)
    $ showString "SomeMarloweVersion " . showsPrec 11 version

withSomeMarloweVersion :: (forall v. MarloweVersion v -> r) -> SomeMarloweVersion -> r
withSomeMarloweVersion f (SomeMarloweVersion v) = f v

withMarloweVersion :: MarloweVersion v -> (IsMarloweVersion v => a) -> a
withMarloweVersion = \case
  MarloweV1 -> id

instance ToJSON (MarloweVersion v) where
  toJSON = String . \case
    MarloweV1 -> "v1"

instance FromJSON SomeMarloweVersion where
  parseJSON json = do
    s :: Text <- parseJSON json
    case s of
      "v1" -> pure $ SomeMarloweVersion MarloweV1
      _    -> fail "Invalid marlowe version"

instance Binary SomeMarloweVersion where
  put (SomeMarloweVersion v) = case v of
    MarloweV1 -> putWord32be 0x01
  get = getWord32be >>= \case
    0x01 -> pure $ SomeMarloweVersion MarloweV1
    _    -> fail "Invalid marlowe version bytes"

putContract :: MarloweVersion v -> Contract v -> Put
putContract v = put . encode . contractToJSON v

getContract :: MarloweVersion v -> Get (Contract v)
getContract v = do
  bytes <- get
  case eitherDecode bytes of
    Left err -> fail err
    Right json -> case parse (contractFromJSON v) json of
      Error err -> fail err
      Success c -> pure c

putRedeemer :: MarloweVersion v -> Redeemer v -> Put
putRedeemer v = put . toChainRedeemer v

getRedeemer :: MarloweVersion v -> Get (Redeemer v)
getRedeemer v = do
  raw <- get
  case fromChainRedeemer v raw of
    Nothing -> fail "failed to decode redeemer"
    Just r -> pure r

putDatum :: MarloweVersion v -> Datum v -> Put
putDatum v = put . toChainDatum v

getDatum :: MarloweVersion v -> Get (Datum v)
getDatum v = do
  raw <- get
  case fromChainDatum v raw of
    Nothing -> fail "failed to decode datum"
    Just d -> pure d

putPayoutDatum :: MarloweVersion v -> PayoutDatum v -> Put
putPayoutDatum MarloweV1 = put

getPayoutDatum :: MarloweVersion v -> Get (PayoutDatum v)
getPayoutDatum MarloweV1 = get

contractToJSON :: MarloweVersion v -> Contract v -> Value
contractToJSON = \case
  MarloweV1 -> toJSON

contractFromJSON :: MarloweVersion v -> Value -> Parser (Contract v)
contractFromJSON = \case
  MarloweV1 -> parseJSON

payoutDatumToJSON :: MarloweVersion v -> PayoutDatum v -> Value
payoutDatumToJSON = \case
  MarloweV1 -> \case
    Chain.AssetId policyId tokenName -> toJSON
      ( String . encodeBase16 . unPolicyId $ policyId
      , String . encodeBase16 . unTokenName $ tokenName
      )

payoutDatumFromJSON :: MarloweVersion v -> Value -> Parser (PayoutDatum v)
payoutDatumFromJSON = \case
  MarloweV1 -> \json -> do
    (p, t) <- parseJSON json
    p' <- either (parseFail . T.unpack) (pure . Chain.PolicyId) . decodeBase16 . encodeUtf8 $ t
    t' <- either (parseFail . T.unpack) (pure . Chain.TokenName) . decodeBase16 . encodeUtf8 $ p
    pure $ Chain.AssetId p' t'

datumToJSON :: MarloweVersion v -> Datum v -> Value
datumToJSON = \case
  MarloweV1 -> toJSON

datumFromJSON :: MarloweVersion v -> Value -> Parser (Datum v)
datumFromJSON = \case
  MarloweV1 -> parseJSON

toChainMerkleizedContinuationDatum :: MarloweVersion v -> Contract v -> Chain.Datum
toChainMerkleizedContinuationDatum = \case
  MarloweV1 -> Chain.toDatum

fromChainMerkleizedContinuationDatum :: MarloweVersion v -> Chain.Datum -> Maybe (Contract v)
fromChainMerkleizedContinuationDatum = \case
  MarloweV1 -> Chain.fromDatum

toChainPayoutDatum :: MarloweVersion v -> PayoutDatum v -> Chain.Datum
toChainPayoutDatum = \case
  MarloweV1 -> \case
    Chain.AssetId policyId tokenName -> do
      let
        currencySymbol = Plutus.currencySymbol . unPolicyId $ policyId
        tokenName' = Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName $ tokenName
      Chain.toDatum (currencySymbol, tokenName')

fromChainPayoutDatum :: MarloweVersion v -> Chain.Datum -> Maybe (PayoutDatum v)
fromChainPayoutDatum = \case
  MarloweV1 -> \datum -> do
    (p, t) <- Chain.fromDatum datum
    let
      p' = Chain.PolicyId . Plutus.fromBuiltin . Plutus.unCurrencySymbol $ p
      t' = Chain.TokenName . Plutus.fromBuiltin . Plutus.unTokenName $ t
    pure $ Chain.AssetId p' t'

toChainDatum :: MarloweVersion v -> Datum v -> Chain.Datum
toChainDatum = \case
  MarloweV1 -> Chain.toDatum

fromChainDatum :: MarloweVersion v -> Chain.Datum -> Maybe (Datum v)
fromChainDatum = \case
  MarloweV1 -> Chain.fromDatum

toChainRedeemer :: MarloweVersion v -> Redeemer v -> Chain.Redeemer
toChainRedeemer = \case
  MarloweV1 -> Chain.toRedeemer

fromChainRedeemer :: MarloweVersion v -> Chain.Redeemer -> Maybe (Redeemer v)
fromChainRedeemer = \case
  MarloweV1 -> Chain.fromRedeemer
