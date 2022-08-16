{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Language.Marlowe.Runtime.Core.Api where

import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), eitherDecode, encode, object, (.:), (.=))
import Data.Aeson.Types (Parser, parse)
import Data.Binary (Binary (..))
import Data.Binary.Get (getWord32be)
import Data.Binary.Put (putWord32be)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Language.Marlowe.Core.V1.Semantics as V1
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.ChainSync.Api (TxOutRef, ValidatorHash)

newtype ContractId = ContractId { unContractId :: TxOutRef }
  deriving stock (Show, Read, Eq, Ord, Generic)
  deriving anyclass (Binary)

data MarloweVersionTag
  = V1

data MarloweVersion (v :: MarloweVersionTag) where
  MarloweV1 :: MarloweVersion 'V1

class IsMarloweVersion (v :: MarloweVersionTag) where
  type Contract v :: *
  type State v :: *
  type Params v :: *
  type Redeemer v :: *
  marloweVersion :: MarloweVersion v

instance IsMarloweVersion 'V1 where
  type Contract 'V1 = V1.Contract
  type State 'V1 = V1.State
  type Params 'V1 = V1.MarloweParams
  type Redeemer 'V1 = [V1.Input]
  marloweVersion = MarloweV1

data Datum v = Datum
  { datumContract :: Contract v
  , datumParams   :: Params v
  , datumState    :: State v
  }

contractToJSON :: MarloweVersion v -> Contract v -> Value
contractToJSON = \case
  MarloweV1 -> toJSON

contractFromJSON :: MarloweVersion v -> Value -> Parser (Contract v)
contractFromJSON = \case
  MarloweV1 -> parseJSON

stateToJSON :: MarloweVersion v -> State v -> Value
stateToJSON = \case
  MarloweV1 -> toJSON

stateFromJSON :: MarloweVersion v -> Value -> Parser (State v)
stateFromJSON = \case
  MarloweV1 -> parseJSON

paramsToJSON :: MarloweVersion v -> Params v -> Value
paramsToJSON = \case
  MarloweV1 -> toJSON

paramsFromJSON :: MarloweVersion v -> Value -> Parser (Params v)
paramsFromJSON = \case
  MarloweV1 -> parseJSON

redeemerToJSON :: MarloweVersion v -> Redeemer v -> Value
redeemerToJSON = \case
  MarloweV1 -> toJSON

redeemerFromJSON :: MarloweVersion v -> Value -> Parser (Redeemer v)
redeemerFromJSON = \case
  MarloweV1 -> parseJSON

datumToJSON :: MarloweVersion v -> Datum v -> Value
datumToJSON v Datum{..} = object
  [ "marloweContract" .= contractToJSON v datumContract
  , "marloweParams" .= paramsToJSON v datumParams
  , "marloweState" .= stateToJSON v datumState
  ]

datumFromJSON :: MarloweVersion v -> Value -> Parser (Datum v)
datumFromJSON v json = do
  obj <- parseJSON json
  datumContract <- contractFromJSON v =<< obj .: "marloweContract"
  datumParams <- paramsFromJSON v =<< obj .: "marloweParams"
  datumState <- stateFromJSON v =<< obj .: "marloweState"
  pure Datum{..}

data SomeMarloweVersion = forall v. SomeMarloweVersion (MarloweVersion v)

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


data VersionedContract v = VersionedContract (MarloweVersion v) (Contract v)
data SomeVersionedContract = forall v. SomeVersionedContract (VersionedContract v)

instance ToJSON (VersionedContract v) where
  toJSON (VersionedContract v c) = object
    [ "version" .= v
    , "contract" .= contractToJSON v c
    ]

instance FromJSON SomeVersionedContract where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    c <- contractFromJSON v =<< obj .: "contract"
    pure $ SomeVersionedContract $ VersionedContract v c

instance Binary SomeVersionedContract where
  put (SomeVersionedContract (VersionedContract v c)) = do
    put $ SomeMarloweVersion v
    put $ encode $ contractToJSON v c
  get = do
    SomeMarloweVersion v <- get
    bytes <- get
    case eitherDecode bytes of
      Left err -> fail err
      Right json -> case parse (contractFromJSON v) json of
        Error err -> fail err
        Success c -> pure $ SomeVersionedContract $ VersionedContract v c

data VersionedRedeemer v = VersionedRedeemer (MarloweVersion v) (Redeemer v)
data SomeVersionedRedeemer = forall v. SomeVersionedRedeemer (VersionedRedeemer v)

instance ToJSON (VersionedRedeemer v) where
  toJSON (VersionedRedeemer v r) = object
    [ "version" .= v
    , "redeemer" .= redeemerToJSON v r
    ]

instance FromJSON SomeVersionedRedeemer where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    r <- redeemerFromJSON v =<< obj .: "redeemer"
    pure $ SomeVersionedRedeemer $ VersionedRedeemer v r

instance Binary SomeVersionedRedeemer where
  put (SomeVersionedRedeemer (VersionedRedeemer v c)) = do
    put $ SomeMarloweVersion v
    put $ encode $ redeemerToJSON v c
  get = do
    SomeMarloweVersion v <- get
    bytes <- get
    case eitherDecode bytes of
      Left err -> fail err
      Right json -> case parse (redeemerFromJSON v) json of
        Error err -> fail err
        Success c -> pure $ SomeVersionedRedeemer $ VersionedRedeemer v c

data VersionedDatum v = VersionedDatum (MarloweVersion v) (Datum v)
data SomeVersionedDatum = forall v. SomeVersionedDatum (VersionedDatum v)

instance ToJSON (VersionedDatum v) where
  toJSON (VersionedDatum v d) = object
    [ "version" .= v
    , "datum" .= datumToJSON v d
    ]

instance FromJSON SomeVersionedDatum where
  parseJSON json = do
    obj <- parseJSON json
    SomeMarloweVersion v <- obj .: "version"
    d <- datumFromJSON v =<< obj .: "datum"
    pure $ SomeVersionedDatum $ VersionedDatum v d

instance Binary SomeVersionedDatum where
  put (SomeVersionedDatum (VersionedDatum v c)) = do
    put $ SomeMarloweVersion v
    put $ encode $ datumToJSON v c
  get = do
    SomeMarloweVersion v <- get
    bytes <- get
    case eitherDecode bytes of
      Left err -> fail err
      Right json -> case parse (datumFromJSON v) json of
        Error err -> fail err
        Success c -> pure $ SomeVersionedDatum $ VersionedDatum v c

getMarloweVersion :: ValidatorHash -> Maybe SomeMarloweVersion
getMarloweVersion = error "not implemented"

getScriptHash :: MarloweVersion v -> ValidatorHash
getScriptHash = error "not implemented"
