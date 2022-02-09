-- File auto generated by purescript-bridge! --
module Plutus.V1.Ledger.Tx where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut (encodeJson, jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.BigInt.Argonaut (BigInt)
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Plutus.V1.Ledger.Address (Address)
import Plutus.V1.Ledger.Crypto (PubKey, Signature)
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (DatumHash, MintingPolicy, Validator)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.TxId (TxId)
import Plutus.V1.Ledger.Value (Value)
import Type.Proxy (Proxy(Proxy))

data RedeemerPtr = RedeemerPtr ScriptTag BigInt

instance Show RedeemerPtr where
  show a = genericShow a

derive instance Eq RedeemerPtr

derive instance Ord RedeemerPtr

instance EncodeJson RedeemerPtr where
  encodeJson = defer \_ -> E.encode $ (case _ of RedeemerPtr a b -> (a /\ b))
    >$< (E.tuple (E.value >/\< E.value))

instance DecodeJson RedeemerPtr where
  decodeJson = defer \_ -> D.decode $
    (D.tuple $ RedeemerPtr </$\> D.value </*\> D.value)

derive instance Generic RedeemerPtr _

--------------------------------------------------------------------------------

_RedeemerPtr :: Iso' RedeemerPtr { a :: ScriptTag, b :: BigInt }
_RedeemerPtr = iso (\(RedeemerPtr a b) -> { a, b })
  (\{ a, b } -> (RedeemerPtr a b))

--------------------------------------------------------------------------------

data ScriptTag
  = Spend
  | Mint
  | Cert
  | Reward

instance Show ScriptTag where
  show a = genericShow a

derive instance Eq ScriptTag

derive instance Ord ScriptTag

instance EncodeJson ScriptTag where
  encodeJson = defer \_ -> E.encode E.enum

instance DecodeJson ScriptTag where
  decodeJson = defer \_ -> D.decode D.enum

derive instance Generic ScriptTag _

instance Enum ScriptTag where
  succ = genericSucc
  pred = genericPred

instance Bounded ScriptTag where
  bottom = genericBottom
  top = genericTop

--------------------------------------------------------------------------------

_Spend :: Prism' ScriptTag Unit
_Spend = prism' (const Spend) case _ of
  Spend -> Just unit
  _ -> Nothing

_Mint :: Prism' ScriptTag Unit
_Mint = prism' (const Mint) case _ of
  Mint -> Just unit
  _ -> Nothing

_Cert :: Prism' ScriptTag Unit
_Cert = prism' (const Cert) case _ of
  Cert -> Just unit
  _ -> Nothing

_Reward :: Prism' ScriptTag Unit
_Reward = prism' (const Reward) case _ of
  Reward -> Just unit
  _ -> Nothing

--------------------------------------------------------------------------------

newtype Tx = Tx
  { txInputs :: Set TxIn
  , txCollateral :: Set TxIn
  , txOutputs :: Array TxOut
  , txMint :: Value
  , txFee :: Value
  , txValidRange :: Interval Slot
  , txMintScripts :: Set MintingPolicy
  , txSignatures :: Map PubKey Signature
  , txRedeemers :: Map RedeemerPtr String
  , txData :: Map DatumHash String
  }

derive instance Eq Tx

instance Show Tx where
  show a = genericShow a

instance EncodeJson Tx where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { txInputs: E.value :: _ (Set TxIn)
        , txCollateral: E.value :: _ (Set TxIn)
        , txOutputs: E.value :: _ (Array TxOut)
        , txMint: E.value :: _ Value
        , txFee: E.value :: _ Value
        , txValidRange: E.value :: _ (Interval Slot)
        , txMintScripts: E.value :: _ (Set MintingPolicy)
        , txSignatures:
            (E.dictionary E.value E.value) :: _ (Map PubKey Signature)
        , txRedeemers:
            (E.dictionary E.value E.value) :: _ (Map RedeemerPtr String)
        , txData: (E.dictionary E.value E.value) :: _ (Map DatumHash String)
        }
    )

instance DecodeJson Tx where
  decodeJson = defer \_ -> D.decode $
    ( Tx <$> D.record "Tx"
        { txInputs: D.value :: _ (Set TxIn)
        , txCollateral: D.value :: _ (Set TxIn)
        , txOutputs: D.value :: _ (Array TxOut)
        , txMint: D.value :: _ Value
        , txFee: D.value :: _ Value
        , txValidRange: D.value :: _ (Interval Slot)
        , txMintScripts: D.value :: _ (Set MintingPolicy)
        , txSignatures:
            (D.dictionary D.value D.value) :: _ (Map PubKey Signature)
        , txRedeemers:
            (D.dictionary D.value D.value) :: _ (Map RedeemerPtr String)
        , txData: (D.dictionary D.value D.value) :: _ (Map DatumHash String)
        }
    )

derive instance Generic Tx _

derive instance Newtype Tx _

--------------------------------------------------------------------------------

_Tx :: Iso' Tx
  { txInputs :: Set TxIn
  , txCollateral :: Set TxIn
  , txOutputs :: Array TxOut
  , txMint :: Value
  , txFee :: Value
  , txValidRange :: Interval Slot
  , txMintScripts :: Set MintingPolicy
  , txSignatures :: Map PubKey Signature
  , txRedeemers :: Map RedeemerPtr String
  , txData :: Map DatumHash String
  }
_Tx = _Newtype

--------------------------------------------------------------------------------

newtype TxIn = TxIn
  { txInRef :: TxOutRef
  , txInType :: Maybe TxInType
  }

derive instance Eq TxIn

derive instance Ord TxIn

instance Show TxIn where
  show a = genericShow a

instance EncodeJson TxIn where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { txInRef: E.value :: _ TxOutRef
        , txInType: (E.maybe E.value) :: _ (Maybe TxInType)
        }
    )

instance DecodeJson TxIn where
  decodeJson = defer \_ -> D.decode $
    ( TxIn <$> D.record "TxIn"
        { txInRef: D.value :: _ TxOutRef
        , txInType: (D.maybe D.value) :: _ (Maybe TxInType)
        }
    )

derive instance Generic TxIn _

derive instance Newtype TxIn _

--------------------------------------------------------------------------------

_TxIn :: Iso' TxIn { txInRef :: TxOutRef, txInType :: Maybe TxInType }
_TxIn = _Newtype

--------------------------------------------------------------------------------

data TxInType
  = ConsumeScriptAddress Validator String String
  | ConsumePublicKeyAddress
  | ConsumeSimpleScriptAddress

instance Show TxInType where
  show a = genericShow a

derive instance Eq TxInType

derive instance Ord TxInType

instance EncodeJson TxInType where
  encodeJson = defer \_ -> case _ of
    ConsumeScriptAddress a b c -> E.encodeTagged "ConsumeScriptAddress"
      (a /\ b /\ c)
      (E.tuple (E.value >/\< E.value >/\< E.value))
    ConsumePublicKeyAddress -> encodeJson
      { tag: "ConsumePublicKeyAddress", contents: jsonNull }
    ConsumeSimpleScriptAddress -> encodeJson
      { tag: "ConsumeSimpleScriptAddress", contents: jsonNull }

instance DecodeJson TxInType where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "TxInType"
    $ Map.fromFoldable
        [ "ConsumeScriptAddress" /\ D.content
            ( D.tuple $ ConsumeScriptAddress </$\> D.value </*\> D.value </*\>
                D.value
            )
        , "ConsumePublicKeyAddress" /\ pure ConsumePublicKeyAddress
        , "ConsumeSimpleScriptAddress" /\ pure ConsumeSimpleScriptAddress
        ]

derive instance Generic TxInType _

--------------------------------------------------------------------------------

_ConsumeScriptAddress :: Prism' TxInType
  { a :: Validator, b :: String, c :: String }
_ConsumeScriptAddress = prism' (\{ a, b, c } -> (ConsumeScriptAddress a b c))
  case _ of
    (ConsumeScriptAddress a b c) -> Just { a, b, c }
    _ -> Nothing

_ConsumePublicKeyAddress :: Prism' TxInType Unit
_ConsumePublicKeyAddress = prism' (const ConsumePublicKeyAddress) case _ of
  ConsumePublicKeyAddress -> Just unit
  _ -> Nothing

_ConsumeSimpleScriptAddress :: Prism' TxInType Unit
_ConsumeSimpleScriptAddress = prism' (const ConsumeSimpleScriptAddress)
  case _ of
    ConsumeSimpleScriptAddress -> Just unit
    _ -> Nothing

--------------------------------------------------------------------------------

newtype TxOut = TxOut
  { txOutAddress :: Address
  , txOutValue :: Value
  , txOutDatumHash :: Maybe DatumHash
  }

derive instance Eq TxOut

instance Show TxOut where
  show a = genericShow a

instance EncodeJson TxOut where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { txOutAddress: E.value :: _ Address
        , txOutValue: E.value :: _ Value
        , txOutDatumHash: (E.maybe E.value) :: _ (Maybe DatumHash)
        }
    )

instance DecodeJson TxOut where
  decodeJson = defer \_ -> D.decode $
    ( TxOut <$> D.record "TxOut"
        { txOutAddress: D.value :: _ Address
        , txOutValue: D.value :: _ Value
        , txOutDatumHash: (D.maybe D.value) :: _ (Maybe DatumHash)
        }
    )

derive instance Generic TxOut _

derive instance Newtype TxOut _

--------------------------------------------------------------------------------

_TxOut :: Iso' TxOut
  { txOutAddress :: Address
  , txOutValue :: Value
  , txOutDatumHash :: Maybe DatumHash
  }
_TxOut = _Newtype

--------------------------------------------------------------------------------

newtype TxOutRef = TxOutRef
  { txOutRefId :: TxId
  , txOutRefIdx :: BigInt
  }

derive instance Eq TxOutRef

derive instance Ord TxOutRef

instance Show TxOutRef where
  show a = genericShow a

instance EncodeJson TxOutRef where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { txOutRefId: E.value :: _ TxId
        , txOutRefIdx: E.value :: _ BigInt
        }
    )

instance DecodeJson TxOutRef where
  decodeJson = defer \_ -> D.decode $
    ( TxOutRef <$> D.record "TxOutRef"
        { txOutRefId: D.value :: _ TxId
        , txOutRefIdx: D.value :: _ BigInt
        }
    )

derive instance Generic TxOutRef _

derive instance Newtype TxOutRef _

--------------------------------------------------------------------------------

_TxOutRef :: Iso' TxOutRef { txOutRefId :: TxId, txOutRefIdx :: BigInt }
_TxOutRef = _Newtype

--------------------------------------------------------------------------------

newtype TxOutTx = TxOutTx
  { txOutTxTx :: Tx
  , txOutTxOut :: TxOut
  }

derive instance Eq TxOutTx

instance Show TxOutTx where
  show a = genericShow a

instance EncodeJson TxOutTx where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { txOutTxTx: E.value :: _ Tx
        , txOutTxOut: E.value :: _ TxOut
        }
    )

instance DecodeJson TxOutTx where
  decodeJson = defer \_ -> D.decode $
    ( TxOutTx <$> D.record "TxOutTx"
        { txOutTxTx: D.value :: _ Tx
        , txOutTxOut: D.value :: _ TxOut
        }
    )

derive instance Generic TxOutTx _

derive instance Newtype TxOutTx _

--------------------------------------------------------------------------------

_TxOutTx :: Iso' TxOutTx { txOutTxTx :: Tx, txOutTxOut :: TxOut }
_TxOutTx = _Newtype
