-- File auto generated by purescript-bridge! --
module Plutus.ChainIndex.Tx where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Plutus.V1.Ledger.Interval (Interval)
import Plutus.V1.Ledger.Scripts (DatumHash)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Tx (TxIn, TxOut)
import Plutus.V1.Ledger.TxId (TxId)
import Type.Proxy (Proxy(Proxy))

newtype ChainIndexTx = ChainIndexTx
  { _citxTxId :: TxId
  , _citxInputs :: Set TxIn
  , _citxOutputs :: ChainIndexTxOutputs
  , _citxValidRange :: Interval Slot
  , _citxData :: Map DatumHash String
  , _citxRedeemers :: Map String String
  , _citxScripts :: Map String String
  , _citxCardanoTx :: Maybe RawJson
  }

derive instance eqChainIndexTx :: Eq ChainIndexTx

instance showChainIndexTx :: Show ChainIndexTx where
  show a = genericShow a

instance encodeJsonChainIndexTx :: EncodeJson ChainIndexTx where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { _citxTxId: E.value :: _ TxId
        , _citxInputs: E.value :: _ (Set TxIn)
        , _citxOutputs: E.value :: _ ChainIndexTxOutputs
        , _citxValidRange: E.value :: _ (Interval Slot)
        , _citxData: (E.dictionary E.value E.value) :: _ (Map DatumHash String)
        , _citxRedeemers:
            (E.dictionary E.value E.value) :: _ (Map String String)
        , _citxScripts: (E.dictionary E.value E.value) :: _ (Map String String)
        , _citxCardanoTx: (E.maybe E.value) :: _ (Maybe RawJson)
        }
    )

instance decodeJsonChainIndexTx :: DecodeJson ChainIndexTx where
  decodeJson = defer \_ -> D.decode $
    ( ChainIndexTx <$> D.record "ChainIndexTx"
        { _citxTxId: D.value :: _ TxId
        , _citxInputs: D.value :: _ (Set TxIn)
        , _citxOutputs: D.value :: _ ChainIndexTxOutputs
        , _citxValidRange: D.value :: _ (Interval Slot)
        , _citxData: (D.dictionary D.value D.value) :: _ (Map DatumHash String)
        , _citxRedeemers:
            (D.dictionary D.value D.value) :: _ (Map String String)
        , _citxScripts: (D.dictionary D.value D.value) :: _ (Map String String)
        , _citxCardanoTx: (D.maybe D.value) :: _ (Maybe RawJson)
        }
    )

derive instance genericChainIndexTx :: Generic ChainIndexTx _

derive instance newtypeChainIndexTx :: Newtype ChainIndexTx _

--------------------------------------------------------------------------------

_ChainIndexTx :: Iso' ChainIndexTx
  { _citxTxId :: TxId
  , _citxInputs :: Set TxIn
  , _citxOutputs :: ChainIndexTxOutputs
  , _citxValidRange :: Interval Slot
  , _citxData :: Map DatumHash String
  , _citxRedeemers :: Map String String
  , _citxScripts :: Map String String
  , _citxCardanoTx :: Maybe RawJson
  }
_ChainIndexTx = _Newtype

citxTxId :: Lens' ChainIndexTx TxId
citxTxId = _Newtype <<< prop (Proxy :: _ "_citxTxId")

citxInputs :: Lens' ChainIndexTx (Set TxIn)
citxInputs = _Newtype <<< prop (Proxy :: _ "_citxInputs")

citxOutputs :: Lens' ChainIndexTx ChainIndexTxOutputs
citxOutputs = _Newtype <<< prop (Proxy :: _ "_citxOutputs")

citxValidRange :: Lens' ChainIndexTx (Interval Slot)
citxValidRange = _Newtype <<< prop (Proxy :: _ "_citxValidRange")

citxData :: Lens' ChainIndexTx (Map DatumHash String)
citxData = _Newtype <<< prop (Proxy :: _ "_citxData")

citxRedeemers :: Lens' ChainIndexTx (Map String String)
citxRedeemers = _Newtype <<< prop (Proxy :: _ "_citxRedeemers")

citxScripts :: Lens' ChainIndexTx (Map String String)
citxScripts = _Newtype <<< prop (Proxy :: _ "_citxScripts")

citxCardanoTx :: Lens' ChainIndexTx (Maybe RawJson)
citxCardanoTx = _Newtype <<< prop (Proxy :: _ "_citxCardanoTx")

--------------------------------------------------------------------------------

data ChainIndexTxOutputs
  = InvalidTx
  | ValidTx (Array TxOut)

derive instance eqChainIndexTxOutputs :: Eq ChainIndexTxOutputs

instance showChainIndexTxOutputs :: Show ChainIndexTxOutputs where
  show a = genericShow a

instance encodeJsonChainIndexTxOutputs :: EncodeJson ChainIndexTxOutputs where
  encodeJson = defer \_ -> case _ of
    InvalidTx -> encodeJson { tag: "InvalidTx", contents: jsonNull }
    ValidTx a -> E.encodeTagged "ValidTx" a E.value

instance decodeJsonChainIndexTxOutputs :: DecodeJson ChainIndexTxOutputs where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "ChainIndexTxOutputs"
    $ Map.fromFoldable
        [ "InvalidTx" /\ pure InvalidTx
        , "ValidTx" /\ D.content (ValidTx <$> D.value)
        ]

derive instance genericChainIndexTxOutputs :: Generic ChainIndexTxOutputs _

--------------------------------------------------------------------------------

_InvalidTx :: Prism' ChainIndexTxOutputs Unit
_InvalidTx = prism' (const InvalidTx) case _ of
  InvalidTx -> Just unit
  _ -> Nothing

_ValidTx :: Prism' ChainIndexTxOutputs (Array TxOut)
_ValidTx = prism' ValidTx case _ of
  (ValidTx a) -> Just a
  _ -> Nothing