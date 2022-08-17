{-# LANGUAGE DataKinds             #-}
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
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ScriptHash, TokenName (..), TxId (..), TxIx (..),
                                               TxOutRef (..), ValidityRange)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
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

parseTransactionId :: String -> Maybe TransactionId
parseTransactionId = fmap TransactionId . parseTxOutRef

renderContractId :: ContractId -> Text
renderContractId = renderTxOutRef . unContractId

renderTransactionId :: TransactionId -> Text
renderTransactionId = renderTxOutRef . unTransactionId

-- | The ID of a transaction is the TxId and TxIx of the script UTxO that it
-- consumes.
newtype TransactionId = TransactionId { unTransactionId :: TxOutRef }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary)

data MarloweVersionTag
  = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

class IsMarloweVersion (v :: MarloweVersionTag) where
  type Contract v :: *
  type Datum v :: *
  type Redeemer v :: *
  type PayoutDatum v :: *
  type Payment v :: *
  marloweVersion :: MarloweVersion v

instance IsMarloweVersion 'V1 where
  type Contract 'V1 = V1.Contract
  type Datum 'V1 = V1.MarloweData
  type Redeemer 'V1 = [V1.Input]
  type PayoutDatum 'V1 = TokenName
  type Payment 'V1 = V1.Payment
  marloweVersion = MarloweV1

data Transaction v = Transaction
  { transactionId :: TransactionId
  , contractId    :: ContractId
  , blockHeader   :: BlockHeader
  , validityRange :: ValidityRange
  , redeemer      :: Redeemer v
  , output        :: TransactionOutput v
  }

data TransactionOutput v = TransactionOutput
  { payouts      :: [Payment v]
  , scriptOutput :: Maybe (TransactionScriptOutput v)
  }

data TransactionScriptOutput v = TransactionScriptOutput
  { utxo  :: TxOutRef
  , datum :: Datum v
  }

data SomeMarloweVersion = forall v. SomeMarloweVersion (MarloweVersion v)

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

datumToData :: MarloweVersion v -> Datum v -> Chain.Datum
datumToData = \case
  MarloweV1 -> Chain.toDatum

datumFromData :: MarloweVersion v -> Chain.Datum -> Maybe (Datum v)
datumFromData = \case
  MarloweV1 -> Chain.fromDatum

getMarloweVersion :: ScriptHash -> Maybe SomeMarloweVersion
getMarloweVersion _ = Nothing

getScriptHashes :: MarloweVersion v -> Set ScriptHash
getScriptHashes = mempty

getRoleValidatorHashes :: MarloweVersion v -> Set ScriptHash
getRoleValidatorHashes = mempty
