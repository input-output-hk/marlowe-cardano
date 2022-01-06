-- File auto generated by purescript-bridge! --
module Plutus.PAB.Webserver.Types where

import Prelude

import Control.Lazy (defer)
import Data.Argonaut.Core (jsonNull)
import Data.Argonaut.Decode (class DecodeJson)
import Data.Argonaut.Decode.Aeson ((</$\>), (</*\>), (</\>))
import Data.Argonaut.Decode.Aeson as D
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Argonaut.Encode.Aeson ((>$<), (>/\<))
import Data.Argonaut.Encode.Aeson as E
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Lens', Prism', iso, prism')
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.RawJson (RawJson)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Ledger.Index (UtxoIndex)
import Playground.Types (FunctionSchema)
import Plutus.Contract.Effects (ActiveEndpoint, PABReq)
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse)
import Plutus.V1.Ledger.Crypto (PubKeyHash)
import Plutus.V1.Ledger.Slot (Slot)
import Plutus.V1.Ledger.Tx (Tx)
import Plutus.V1.Ledger.TxId (TxId)
import Schema (FormSchema)
import Type.Proxy (Proxy(Proxy))
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Rollup.Types (AnnotatedTx)
import Wallet.Types (ContractActivityStatus, ContractInstanceId)

newtype FullReport a = FullReport
  { contractReport :: ContractReport a
  , chainReport :: ChainReport
  }

derive instance eqFullReport :: (Eq a) => Eq (FullReport a)

instance showFullReport :: (Show a) => Show (FullReport a) where
  show a = genericShow a

instance encodeJsonFullReport :: (EncodeJson a) => EncodeJson (FullReport a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { contractReport: E.value :: _ (ContractReport a)
        , chainReport: E.value :: _ ChainReport
        }
    )

instance decodeJsonFullReport :: (DecodeJson a) => DecodeJson (FullReport a) where
  decodeJson = defer \_ -> D.decode $
    ( FullReport <$> D.record "FullReport"
        { contractReport: D.value :: _ (ContractReport a)
        , chainReport: D.value :: _ ChainReport
        }
    )

derive instance genericFullReport :: Generic (FullReport a) _

derive instance newtypeFullReport :: Newtype (FullReport a) _

--------------------------------------------------------------------------------

_FullReport
  :: forall a
   . Iso' (FullReport a)
       { contractReport :: ContractReport a, chainReport :: ChainReport }
_FullReport = _Newtype

--------------------------------------------------------------------------------

newtype ChainReport = ChainReport
  { transactionMap :: Map TxId Tx
  , utxoIndex :: UtxoIndex
  , annotatedBlockchain :: Array (Array AnnotatedTx)
  }

derive instance eqChainReport :: Eq ChainReport

instance showChainReport :: Show ChainReport where
  show a = genericShow a

instance encodeJsonChainReport :: EncodeJson ChainReport where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { transactionMap: (E.dictionary E.value E.value) :: _ (Map TxId Tx)
        , utxoIndex: E.value :: _ UtxoIndex
        , annotatedBlockchain: E.value :: _ (Array (Array AnnotatedTx))
        }
    )

instance decodeJsonChainReport :: DecodeJson ChainReport where
  decodeJson = defer \_ -> D.decode $
    ( ChainReport <$> D.record "ChainReport"
        { transactionMap: (D.dictionary D.value D.value) :: _ (Map TxId Tx)
        , utxoIndex: D.value :: _ UtxoIndex
        , annotatedBlockchain: D.value :: _ (Array (Array AnnotatedTx))
        }
    )

derive instance genericChainReport :: Generic ChainReport _

derive instance newtypeChainReport :: Newtype ChainReport _

--------------------------------------------------------------------------------

_ChainReport :: Iso' ChainReport
  { transactionMap :: Map TxId Tx
  , utxoIndex :: UtxoIndex
  , annotatedBlockchain :: Array (Array AnnotatedTx)
  }
_ChainReport = _Newtype

--------------------------------------------------------------------------------

newtype ContractReport a = ContractReport
  { crAvailableContracts :: Array (ContractSignatureResponse a)
  , crActiveContractStates ::
      Array (Tuple ContractInstanceId (PartiallyDecodedResponse PABReq))
  }

derive instance eqContractReport :: (Eq a) => Eq (ContractReport a)

instance showContractReport :: (Show a) => Show (ContractReport a) where
  show a = genericShow a

instance encodeJsonContractReport ::
  ( EncodeJson a
  ) =>
  EncodeJson (ContractReport a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { crAvailableContracts:
            E.value :: _ (Array (ContractSignatureResponse a))
        , crActiveContractStates:
            E.value :: _
              ( Array
                  (Tuple ContractInstanceId (PartiallyDecodedResponse PABReq))
              )
        }
    )

instance decodeJsonContractReport ::
  ( DecodeJson a
  ) =>
  DecodeJson (ContractReport a) where
  decodeJson = defer \_ -> D.decode $
    ( ContractReport <$> D.record "ContractReport"
        { crAvailableContracts:
            D.value :: _ (Array (ContractSignatureResponse a))
        , crActiveContractStates:
            D.value :: _
              ( Array
                  (Tuple ContractInstanceId (PartiallyDecodedResponse PABReq))
              )
        }
    )

derive instance genericContractReport :: Generic (ContractReport a) _

derive instance newtypeContractReport :: Newtype (ContractReport a) _

--------------------------------------------------------------------------------

_ContractReport
  :: forall a
   . Iso' (ContractReport a)
       { crAvailableContracts :: Array (ContractSignatureResponse a)
       , crActiveContractStates ::
           Array (Tuple ContractInstanceId (PartiallyDecodedResponse PABReq))
       }
_ContractReport = _Newtype

--------------------------------------------------------------------------------

newtype ContractSignatureResponse a = ContractSignatureResponse
  { csrDefinition :: a
  , csrSchemas :: Array (FunctionSchema FormSchema)
  }

derive instance eqContractSignatureResponse ::
  ( Eq a
  ) =>
  Eq (ContractSignatureResponse a)

instance showContractSignatureResponse ::
  ( Show a
  ) =>
  Show (ContractSignatureResponse a) where
  show a = genericShow a

instance encodeJsonContractSignatureResponse ::
  ( EncodeJson a
  ) =>
  EncodeJson (ContractSignatureResponse a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { csrDefinition: E.value :: _ a
        , csrSchemas: E.value :: _ (Array (FunctionSchema FormSchema))
        }
    )

instance decodeJsonContractSignatureResponse ::
  ( DecodeJson a
  ) =>
  DecodeJson (ContractSignatureResponse a) where
  decodeJson = defer \_ -> D.decode $
    ( ContractSignatureResponse <$> D.record "ContractSignatureResponse"
        { csrDefinition: D.value :: _ a
        , csrSchemas: D.value :: _ (Array (FunctionSchema FormSchema))
        }
    )

derive instance genericContractSignatureResponse ::
  Generic (ContractSignatureResponse a) _

derive instance newtypeContractSignatureResponse ::
  Newtype (ContractSignatureResponse a) _

--------------------------------------------------------------------------------

_ContractSignatureResponse
  :: forall a
   . Iso' (ContractSignatureResponse a)
       { csrDefinition :: a, csrSchemas :: Array (FunctionSchema FormSchema) }
_ContractSignatureResponse = _Newtype

--------------------------------------------------------------------------------

newtype ContractActivationArgs a = ContractActivationArgs
  { caID :: a
  , caWallet :: Maybe Wallet
  }

derive instance eqContractActivationArgs ::
  ( Eq a
  ) =>
  Eq (ContractActivationArgs a)

instance showContractActivationArgs ::
  ( Show a
  ) =>
  Show (ContractActivationArgs a) where
  show a = genericShow a

instance encodeJsonContractActivationArgs ::
  ( EncodeJson a
  ) =>
  EncodeJson (ContractActivationArgs a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { caID: E.value :: _ a
        , caWallet: (E.maybe E.value) :: _ (Maybe Wallet)
        }
    )

instance decodeJsonContractActivationArgs ::
  ( DecodeJson a
  ) =>
  DecodeJson (ContractActivationArgs a) where
  decodeJson = defer \_ -> D.decode $
    ( ContractActivationArgs <$> D.record "ContractActivationArgs"
        { caID: D.value :: _ a
        , caWallet: (D.maybe D.value) :: _ (Maybe Wallet)
        }
    )

derive instance genericContractActivationArgs ::
  Generic (ContractActivationArgs a) _

derive instance newtypeContractActivationArgs ::
  Newtype (ContractActivationArgs a) _

--------------------------------------------------------------------------------

_ContractActivationArgs
  :: forall a
   . Iso' (ContractActivationArgs a) { caID :: a, caWallet :: Maybe Wallet }
_ContractActivationArgs = _Newtype

--------------------------------------------------------------------------------

newtype ContractInstanceClientState a = ContractInstanceClientState
  { cicContract :: ContractInstanceId
  , cicCurrentState :: PartiallyDecodedResponse ActiveEndpoint
  , cicWallet :: Wallet
  , cicDefinition :: a
  , cicStatus :: ContractActivityStatus
  }

instance showContractInstanceClientState ::
  ( Show a
  ) =>
  Show (ContractInstanceClientState a) where
  show a = genericShow a

instance encodeJsonContractInstanceClientState ::
  ( EncodeJson a
  ) =>
  EncodeJson (ContractInstanceClientState a) where
  encodeJson = defer \_ -> E.encode $ unwrap >$<
    ( E.record
        { cicContract: E.value :: _ ContractInstanceId
        , cicCurrentState:
            E.value :: _ (PartiallyDecodedResponse ActiveEndpoint)
        , cicWallet: E.value :: _ Wallet
        , cicDefinition: E.value :: _ a
        , cicStatus: E.value :: _ ContractActivityStatus
        }
    )

instance decodeJsonContractInstanceClientState ::
  ( DecodeJson a
  ) =>
  DecodeJson (ContractInstanceClientState a) where
  decodeJson = defer \_ -> D.decode $
    ( ContractInstanceClientState <$> D.record "ContractInstanceClientState"
        { cicContract: D.value :: _ ContractInstanceId
        , cicCurrentState:
            D.value :: _ (PartiallyDecodedResponse ActiveEndpoint)
        , cicWallet: D.value :: _ Wallet
        , cicDefinition: D.value :: _ a
        , cicStatus: D.value :: _ ContractActivityStatus
        }
    )

derive instance genericContractInstanceClientState ::
  Generic (ContractInstanceClientState a) _

derive instance newtypeContractInstanceClientState ::
  Newtype (ContractInstanceClientState a) _

--------------------------------------------------------------------------------

_ContractInstanceClientState
  :: forall a
   . Iso' (ContractInstanceClientState a)
       { cicContract :: ContractInstanceId
       , cicCurrentState :: PartiallyDecodedResponse ActiveEndpoint
       , cicWallet :: Wallet
       , cicDefinition :: a
       , cicStatus :: ContractActivityStatus
       }
_ContractInstanceClientState = _Newtype

--------------------------------------------------------------------------------

data InstanceStatusToClient
  = NewObservableState RawJson
  | NewActiveEndpoints (Array ActiveEndpoint)
  | ContractFinished (Maybe RawJson)

instance showInstanceStatusToClient :: Show InstanceStatusToClient where
  show a = genericShow a

instance encodeJsonInstanceStatusToClient :: EncodeJson InstanceStatusToClient where
  encodeJson = defer \_ -> case _ of
    NewObservableState a -> E.encodeTagged "NewObservableState" a E.value
    NewActiveEndpoints a -> E.encodeTagged "NewActiveEndpoints" a E.value
    ContractFinished a -> E.encodeTagged "ContractFinished" a (E.maybe E.value)

instance decodeJsonInstanceStatusToClient :: DecodeJson InstanceStatusToClient where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "InstanceStatusToClient"
    $ Map.fromFoldable
        [ "NewObservableState" /\ D.content (NewObservableState <$> D.value)
        , "NewActiveEndpoints" /\ D.content (NewActiveEndpoints <$> D.value)
        , "ContractFinished" /\ D.content
            (ContractFinished <$> (D.maybe D.value))
        ]

derive instance genericInstanceStatusToClient ::
  Generic InstanceStatusToClient _

--------------------------------------------------------------------------------

_NewObservableState :: Prism' InstanceStatusToClient RawJson
_NewObservableState = prism' NewObservableState case _ of
  (NewObservableState a) -> Just a
  _ -> Nothing

_NewActiveEndpoints :: Prism' InstanceStatusToClient (Array ActiveEndpoint)
_NewActiveEndpoints = prism' NewActiveEndpoints case _ of
  (NewActiveEndpoints a) -> Just a
  _ -> Nothing

_ContractFinished :: Prism' InstanceStatusToClient (Maybe RawJson)
_ContractFinished = prism' ContractFinished case _ of
  (ContractFinished a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data CombinedWSStreamToClient
  = InstanceUpdate ContractInstanceId InstanceStatusToClient
  | SlotChange Slot

instance showCombinedWSStreamToClient :: Show CombinedWSStreamToClient where
  show a = genericShow a

instance encodeJsonCombinedWSStreamToClient ::
  EncodeJson CombinedWSStreamToClient where
  encodeJson = defer \_ -> case _ of
    InstanceUpdate a b -> E.encodeTagged "InstanceUpdate" (a /\ b)
      (E.tuple (E.value >/\< E.value))
    SlotChange a -> E.encodeTagged "SlotChange" a E.value

instance decodeJsonCombinedWSStreamToClient ::
  DecodeJson CombinedWSStreamToClient where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "CombinedWSStreamToClient"
    $ Map.fromFoldable
        [ "InstanceUpdate" /\ D.content
            (D.tuple $ InstanceUpdate </$\> D.value </*\> D.value)
        , "SlotChange" /\ D.content (SlotChange <$> D.value)
        ]

derive instance genericCombinedWSStreamToClient ::
  Generic CombinedWSStreamToClient _

--------------------------------------------------------------------------------

_InstanceUpdate :: Prism' CombinedWSStreamToClient
  { a :: ContractInstanceId, b :: InstanceStatusToClient }
_InstanceUpdate = prism' (\{ a, b } -> (InstanceUpdate a b)) case _ of
  (InstanceUpdate a b) -> Just { a, b }
  _ -> Nothing

_SlotChange :: Prism' CombinedWSStreamToClient Slot
_SlotChange = prism' SlotChange case _ of
  (SlotChange a) -> Just a
  _ -> Nothing

--------------------------------------------------------------------------------

data CombinedWSStreamToServer
  = Subscribe (Either ContractInstanceId PubKeyHash)
  | Unsubscribe (Either ContractInstanceId PubKeyHash)

instance showCombinedWSStreamToServer :: Show CombinedWSStreamToServer where
  show a = genericShow a

instance encodeJsonCombinedWSStreamToServer ::
  EncodeJson CombinedWSStreamToServer where
  encodeJson = defer \_ -> case _ of
    Subscribe a -> E.encodeTagged "Subscribe" a (E.either E.value E.value)
    Unsubscribe a -> E.encodeTagged "Unsubscribe" a (E.either E.value E.value)

instance decodeJsonCombinedWSStreamToServer ::
  DecodeJson CombinedWSStreamToServer where
  decodeJson = defer \_ -> D.decode
    $ D.sumType "CombinedWSStreamToServer"
    $ Map.fromFoldable
        [ "Subscribe" /\ D.content (Subscribe <$> (D.either D.value D.value))
        , "Unsubscribe" /\ D.content
            (Unsubscribe <$> (D.either D.value D.value))
        ]

derive instance genericCombinedWSStreamToServer ::
  Generic CombinedWSStreamToServer _

--------------------------------------------------------------------------------

_Subscribe :: Prism' CombinedWSStreamToServer
  (Either ContractInstanceId PubKeyHash)
_Subscribe = prism' Subscribe case _ of
  (Subscribe a) -> Just a
  _ -> Nothing

_Unsubscribe :: Prism' CombinedWSStreamToServer
  (Either ContractInstanceId PubKeyHash)
_Unsubscribe = prism' Unsubscribe case _ of
  (Unsubscribe a) -> Just a
  _ -> Nothing