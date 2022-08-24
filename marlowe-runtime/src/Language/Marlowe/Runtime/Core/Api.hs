{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Marlowe.Runtime.Core.Api where

import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), eitherDecode, encode, object, withText, (.:),
                   (.=))
import Data.Aeson.Types (Parser, parse, parseFail)
import Data.Binary (Binary (..))
import Data.Binary.Get (getWord32be)
import Data.Binary.Put (putWord32be)
import Data.ByteString.Base16 (decodeBase16, encodeBase16)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ScriptHash, TokenName (..), TxId (..), TxIx (..),
                                               TxOutRef (..))
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import qualified Plutus.V1.Ledger.Api as Plutus
import Text.Read (readMaybe)

-- | The ID of a contract is the TxId and TxIx of the UTxO that first created
-- the contract.
newtype ContractId = ContractId { unContractId :: TxOutRef }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

parseTxOutRef :: String -> Maybe TxOutRef
parseTxOutRef val = case splitOn "#" val of
  [txId, txIx] -> TxOutRef
    <$> (TxId <$> either (const Nothing) Just (decodeBase16 $ encodeUtf8 $ T.pack txId))
    <*> (TxIx <$> readMaybe txIx)
  _ -> Nothing

renderTxOutRef :: TxOutRef -> Text
renderTxOutRef TxOutRef{..} = mconcat
  [ encodeBase16 $ unTxId txId
  , "#"
  , T.pack $ show $ unTxIx txIx
  ]

parseContractId :: String -> Maybe ContractId
parseContractId = fmap ContractId . parseTxOutRef

renderContractId :: ContractId -> Text
renderContractId = renderTxOutRef . unContractId

data MarloweVersionTag
  = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

deriving instance Show (MarloweVersion v)
deriving instance Eq (MarloweVersion v)
deriving instance Ord (MarloweVersion v)

class IsMarloweVersion (v :: MarloweVersionTag) where
  type Contract v :: *
  type Datum v :: *
  type Redeemer v :: *
  type PayoutDatum v :: *
  marloweVersion :: MarloweVersion v

instance IsMarloweVersion 'V1 where
  type Contract 'V1 = V1.Contract
  type Datum 'V1 = V1.MarloweData
  type Redeemer 'V1 = [V1.Input]
  type PayoutDatum 'V1 = TokenName
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

data TransactionOutput v = TransactionOutput
  { payouts      :: Map Chain.TxOutRef (Payout v)
  , scriptOutput :: Maybe (TransactionScriptOutput v)
  }

deriving instance Show (TransactionOutput 'V1)
deriving instance Eq (TransactionOutput 'V1)

data Payout v = Payout
  { assets :: Chain.Assets
  , datum  :: PayoutDatum v
  }

deriving instance Show (Payout 'V1)
deriving instance Eq (Payout 'V1)

data TransactionScriptOutput v = TransactionScriptOutput
  { utxo  :: TxOutRef
  , datum :: Datum v
  }

deriving instance Show (TransactionScriptOutput 'V1)
deriving instance Eq (TransactionScriptOutput 'V1)

data SomeMarloweVersion = forall v. SomeMarloweVersion (MarloweVersion v)

instance Eq SomeMarloweVersion where
  SomeMarloweVersion MarloweV1 == SomeMarloweVersion MarloweV1 = True

instance Show SomeMarloweVersion where
  showsPrec p (SomeMarloweVersion version) = showParen (p >= 11)
    $ showString "SomeMarloweVersion " . showsPrec 11 version

data ContractInVersion v = ContractInVersion (MarloweVersion v) (Contract v)
data SomeContractInVersion = forall v. SomeContractInVersion (ContractInVersion v)

data RedeemerInVersion v = RedeemerInVersion (MarloweVersion v) (Redeemer v)
data SomeRedeemerInVersion = forall v. SomeRedeemerInVersion (RedeemerInVersion v)

data DatumInVersion v = DatumInVersion (MarloweVersion v) (Datum v)
data SomeDatumInVersion = forall v. SomeDatumInVersion (DatumInVersion v)

data PayoutDatumInVersion v = PayoutDatumInVersion (MarloweVersion v) (PayoutDatum v)
data SomePayoutDatumInVersion = forall v. SomePayoutDatumInVersion (PayoutDatumInVersion v)

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

instance ToJSON (ContractInVersion v) where
  toJSON (ContractInVersion v c) = object
    [ "version" .= v
    , "contract" .= contractToJSON v c
    ]

instance FromJSON SomeContractInVersion where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    c <- contractFromJSON v =<< obj .: "contract"
    pure $ SomeContractInVersion $ ContractInVersion v c

instance Binary SomeContractInVersion where
  put (SomeContractInVersion (ContractInVersion v c)) = do
    put $ SomeMarloweVersion v
    put $ encode $ contractToJSON v c
  get = do
    SomeMarloweVersion v <- get
    bytes <- get
    case eitherDecode bytes of
      Left err -> fail err
      Right json -> case parse (contractFromJSON v) json of
        Error err -> fail err
        Success c -> pure $ SomeContractInVersion $ ContractInVersion v c

instance ToJSON (RedeemerInVersion v) where
  toJSON (RedeemerInVersion v r) = object
    [ "version" .= v
    , "redeemer" .= redeemerToJSON v r
    ]

instance FromJSON SomeRedeemerInVersion where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    r <- redeemerFromJSON v =<< obj .: "redeemer"
    pure $ SomeRedeemerInVersion $ RedeemerInVersion v r

instance Binary SomeRedeemerInVersion where
  put (SomeRedeemerInVersion (RedeemerInVersion v c)) = do
    put $ SomeMarloweVersion v
    put $ encode $ redeemerToJSON v c
  get = do
    SomeMarloweVersion v <- get
    bytes <- get
    case eitherDecode bytes of
      Left err -> fail err
      Right json -> case parse (redeemerFromJSON v) json of
        Error err -> fail err
        Success c -> pure $ SomeRedeemerInVersion $ RedeemerInVersion v c

instance ToJSON (DatumInVersion v) where
  toJSON (DatumInVersion v d) = object
    [ "version" .= v
    , "datum" .= datumToJSON v d
    ]

instance FromJSON SomeDatumInVersion where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    d <- datumFromJSON v =<< obj .: "datum"
    pure $ SomeDatumInVersion $ DatumInVersion v d

instance Binary SomeDatumInVersion where
  put (SomeDatumInVersion (DatumInVersion v c)) = do
    put $ SomeMarloweVersion v
    put $ encode $ datumToJSON v c
  get = do
    SomeMarloweVersion v <- get
    bytes <- get
    case eitherDecode bytes of
      Left err -> fail err
      Right json -> case parse (datumFromJSON v) json of
        Error err -> fail err
        Success c -> pure $ SomeDatumInVersion $ DatumInVersion v c

instance ToJSON (PayoutDatumInVersion v) where
  toJSON (PayoutDatumInVersion v d) = object
    [ "version" .= v
    , "datum" .= payoutDatumToJSON v d
    ]

instance FromJSON SomePayoutDatumInVersion where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    d <- payoutDatumFromJSON v =<< obj .: "datum"
    pure $ SomePayoutDatumInVersion $ PayoutDatumInVersion v d

instance Binary SomePayoutDatumInVersion where
  put (SomePayoutDatumInVersion (PayoutDatumInVersion v c)) = do
    put $ SomeMarloweVersion v
    case v of
      MarloweV1 -> put c
  get = do
    SomeMarloweVersion v <- get
    SomePayoutDatumInVersion . PayoutDatumInVersion v <$> case v of
      MarloweV1 -> get

contractToJSON :: MarloweVersion v -> Contract v -> Value
contractToJSON = \case
  MarloweV1 -> toJSON

contractFromJSON :: MarloweVersion v -> Value -> Parser (Contract v)
contractFromJSON = \case
  MarloweV1 -> parseJSON

redeemerToJSON :: MarloweVersion v -> Redeemer v -> Value
redeemerToJSON = \case
  MarloweV1 -> toJSON

redeemerFromJSON :: MarloweVersion v -> Value -> Parser (Redeemer v)
redeemerFromJSON = \case
  MarloweV1 -> parseJSON

payoutDatumToJSON :: MarloweVersion v -> PayoutDatum v -> Value
payoutDatumToJSON = \case
  MarloweV1 -> String . encodeBase16 . unTokenName

payoutDatumFromJSON :: MarloweVersion v -> Value -> Parser (PayoutDatum v)
payoutDatumFromJSON = \case
  MarloweV1 -> withText "TokenName"
    $ either (parseFail . T.unpack) (pure . TokenName) . decodeBase16 . encodeUtf8

datumToJSON :: MarloweVersion v -> Datum v -> Value
datumToJSON = \case
  MarloweV1 -> toJSON

datumFromJSON :: MarloweVersion v -> Value -> Parser (Datum v)
datumFromJSON = \case
  MarloweV1 -> parseJSON

toChainPayoutDatum :: MarloweVersion v -> PayoutDatum v -> Chain.Datum
toChainPayoutDatum = \case
  MarloweV1 -> Chain.toDatum . Plutus.TokenName . Plutus.toBuiltin . Chain.unTokenName

fromChainPayoutDatum :: MarloweVersion v -> Chain.Datum -> Maybe (PayoutDatum v)
fromChainPayoutDatum = \case
  MarloweV1 -> fmap (Chain.TokenName . Plutus.fromBuiltin . Plutus.unTokenName) . Chain.fromDatum

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

getMarloweVersion :: ScriptHash -> Maybe (SomeMarloweVersion, ScriptHash)
getMarloweVersion hash
  | hash == "62c56ccfc6217aff5692e1d3ebe89c21053d31fc11882cb21bfdd307" =
      Just (SomeMarloweVersion MarloweV1, "") --TODO
  | otherwise = Nothing

getScriptHashes :: MarloweVersion v -> Set ScriptHash
getScriptHashes = mempty
