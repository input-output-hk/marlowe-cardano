{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Query.Types
  where

import Control.Monad (join)
import Data.Aeson (FromJSON, ToJSON(..), Value(..), object, (.=))
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (Binary(..), Get, Put, getWord8, putWord8)
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import GHC.Generics (Generic)
import GHC.Show (showCommaSpace, showSpace)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, PolicyId, TokenName, TxId, TxOutRef)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId
  , MarloweMetadataTag
  , MarloweTransactionMetadata
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Payout
  , SomeMarloweVersion(..)
  , Transaction
  , TransactionScriptOutput
  )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Codec (BinaryMessage(..))
import Network.Protocol.Codec.Spec
  (MessageEq(..), MessageVariations(..), ShowProtocol(..), SomePeerHasAgency(..), Variations(..), varyAp)
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.TypedProtocol
import Network.TypedProtocol.Codec (AnyMessageAndAgency(..))
import Observe.Event.Network.Protocol (MessageToJSON(..))

data MarloweQuery where
  StReq :: MarloweQuery
  StRes :: a -> MarloweQuery
  StDone :: MarloweQuery

instance HasSignature MarloweQuery where
  signature _ = "MarloweQuery"

newtype WithdrawalFilter = WithdrawalFilter
  { roleCurrencies :: Set PolicyId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, Binary, Variations)
  deriving newtype (Semigroup, Monoid)

data ContractFilter = ContractFilter
  { tags :: Set MarloweMetadataTag
  , roleCurrencies :: Set PolicyId
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, Binary, Variations)

instance Semigroup ContractFilter where
  a <> b = ContractFilter
    { tags = on (<>) tags a b
    , roleCurrencies = on (<>) (\ContractFilter{..} -> roleCurrencies) a b
    }

instance Monoid ContractFilter where
  mempty = ContractFilter
    { tags = mempty
    , roleCurrencies = mempty
    }

data Request a where
  ReqContractHeaders :: ContractFilter -> Range ContractId -> Request (Maybe (Page ContractId ContractHeader))
  ReqContractState :: ContractId -> Request (Maybe SomeContractState)
  ReqTransaction :: TxId -> Request (Maybe SomeTransaction)
  ReqTransactions :: ContractId -> Request (Maybe SomeTransactions)
  ReqWithdrawal :: TxId -> Request (Maybe Withdrawal)
  ReqWithdrawals :: WithdrawalFilter -> Range TxId -> Request (Maybe (Page TxId Withdrawal))
  ReqBoth :: Request a -> Request b -> Request (a, b)

data SomeRequest where
  SomeRequest :: Request a -> SomeRequest

instance Binary SomeRequest where
  get = do
    tag <- getWord8
    case tag of
      0x00 -> do
        SomeRequest a <- get
        SomeRequest b <- get
        pure $ SomeRequest $ ReqBoth a b
      0x01 -> SomeRequest <$> (ReqContractHeaders <$> get <*> get)
      0x02 -> SomeRequest . ReqContractState <$> get
      0x03 -> SomeRequest . ReqTransaction <$> get
      0x04 -> SomeRequest . ReqTransactions <$> get
      0x05 -> SomeRequest . ReqWithdrawal <$> get
      0x06 -> SomeRequest <$> (ReqWithdrawals <$> get <*> get)
      _ -> fail "Invalid Request tag"

  put (SomeRequest req) = case req of
    ReqBoth a b -> do
      putWord8 0x00
      put $ SomeRequest a
      put $ SomeRequest b
    ReqContractHeaders cFilter range -> do
      putWord8 0x01
      put cFilter
      put range
    ReqContractState contractId -> do
      putWord8 0x02
      put contractId
    ReqTransaction txId -> do
      putWord8 0x03
      put txId
    ReqTransactions contractId -> do
      putWord8 0x04
      put contractId
    ReqWithdrawal txId -> do
      putWord8 0x05
      put txId
    ReqWithdrawals wFilter range -> do
      putWord8 0x06
      put wFilter
      put range

deriving instance Eq (Request a)
deriving instance Show (Request a)

instance Variations SomeRequest where
  variations = join $ NE.fromList
    [ fmap SomeRequest $ ReqBoth <$> (ReqTransactions <$> variations) `varyAp` (ReqTransactions <$> variations)
    , fmap SomeRequest $ ReqContractHeaders <$> variations `varyAp` variations
    , SomeRequest . ReqContractState <$> variations
    , SomeRequest . ReqTransaction <$> variations
    , SomeRequest . ReqTransactions <$> variations
    , SomeRequest . ReqWithdrawal <$> variations
    , fmap SomeRequest $ ReqWithdrawals <$> variations `varyAp` variations
    ]

instance ToJSON (Request a) where
  toJSON = \case
    ReqBoth a b -> object
      [ "req-both" .= (toJSON a, toJSON b)
      ]
    ReqContractHeaders cFilter range -> object
      [ "get-contract-headers" .= object
        [ "filter" .= cFilter
        , "range" .= range
        ]
      ]
    ReqContractState contractId -> object
      [ "get-contract-state" .= contractId
      ]
    ReqTransaction txId -> object
      [ "get-transaction" .= txId
      ]
    ReqTransactions contractId -> object
      [ "get-transactions" .= contractId
      ]
    ReqWithdrawal txId -> object
      [ "get-withdrawal" .= txId
      ]
    ReqWithdrawals wFilter range -> object
      [ "get-withdrawals" .= object
        [ "filter" .= wFilter
        , "range" .= range
        ]
      ]

data StRes a where
  TokContractHeaders :: StRes (Maybe (Page ContractId ContractHeader))
  TokContractState :: StRes (Maybe SomeContractState)
  TokTransaction :: StRes (Maybe SomeTransaction)
  TokTransactions :: StRes (Maybe SomeTransactions)
  TokWithdrawal :: StRes (Maybe Withdrawal)
  TokWithdrawals :: StRes (Maybe (Page TxId Withdrawal))
  TokBoth :: StRes a -> StRes b -> StRes (a, b)

data SomeStRes = forall a. SomeStRes (StRes a)

instance Variations SomeStRes where
  variations = NE.fromList
    [ SomeStRes $ TokBoth TokTransactions TokTransactions
    , SomeStRes TokContractHeaders
    , SomeStRes TokContractState
    , SomeStRes TokTransaction
    , SomeStRes TokTransactions
    , SomeStRes TokWithdrawal
    , SomeStRes TokWithdrawals
    ]

deriving instance Show (StRes a)
deriving instance Eq (StRes a)

instance Protocol MarloweQuery where
  data Message MarloweQuery st st' where
    MsgRequest :: Request a -> Message MarloweQuery
      'StReq
      ('StRes a)

    MsgRespond :: a -> Message MarloweQuery
      ('StRes a)
      'StReq

    MsgDone :: Message MarloweQuery
      'StReq
      'StDone

  data ClientHasAgency ps where
    TokReq :: ClientHasAgency 'StReq

  data ServerHasAgency ps where
    TokRes :: StRes a -> ServerHasAgency ('StRes a)

  data NobodyHasAgency ps where
    TokDone :: NobodyHasAgency 'StDone

  exclusionLemma_ClientAndServerHaveAgency TokReq = \case
  exclusionLemma_NobodyAndClientHaveAgency TokDone = \case
  exclusionLemma_NobodyAndServerHaveAgency TokDone = \case

data Range a = Range
  { rangeStart :: Maybe a
  , rangeOffset :: Int
  , rangeLimit :: Int
  , rangeDirection :: Order
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary, Variations)

data Page a b = Page
  { items :: [b]
  , nextRange :: Maybe (Range a)
  , totalCount :: Int
  }
  deriving stock (Eq, Show, Read, Ord, Functor, Generic, Foldable, Traversable)
  deriving anyclass (ToJSON, Binary, Variations)

instance Bifunctor Page where
  bimap f g Page{..} = Page{items = g <$> items, nextRange = fmap f <$> nextRange, ..}

data SomeContractState = forall v. SomeContractState (MarloweVersion v) (ContractState v)

instance BinaryMessage MarloweQuery where
  putMessage = \case
    ClientAgency TokReq -> \case
      MsgRequest req -> do
        putWord8 0x01
        put $ SomeRequest req

      MsgDone -> putWord8 0x03

    ServerAgency (TokRes req) -> \case
      MsgRespond a -> do
        putWord8 0x02
        putResult req a

  getMessage tok = do
    tag <- getWord8
    case tag of
      0x01 -> case tok of
        ClientAgency TokReq -> do
          SomeRequest req <- get
          pure $ SomeMessage $ MsgRequest req
        _ -> fail "Invalid protocol state for MsgRequest"

      0x02 -> case tok of
        ServerAgency (TokRes req) -> SomeMessage . MsgRespond <$> getResult req
        ClientAgency _  -> fail "Invalid protocol state for MsgRespond"

      0x03 -> case tok of
        ClientAgency TokReq -> pure $ SomeMessage MsgDone
        _ -> fail "Invalid protocol state for MsgDone"

      _ -> fail $ "Invalid message tag " <> show tag

getResult :: StRes a -> Get a
getResult = \case
  TokBoth a b -> (,) <$> getResult a <*> getResult b
  TokContractHeaders -> get
  TokContractState -> get
  TokTransaction -> get
  TokTransactions -> get
  TokWithdrawal -> get
  TokWithdrawals -> get

putResult :: StRes a -> a -> Put
putResult = \case
  TokBoth ta tb -> \(a, b) -> putResult ta a *> putResult tb b
  TokContractHeaders -> put
  TokContractState -> put
  TokTransaction -> put
  TokTransactions -> put
  TokWithdrawal -> put
  TokWithdrawals -> put

instance Show SomeContractState where
  showsPrec p (SomeContractState MarloweV1 state) = showParen (p >= 11)
    ( showString "SomeContractState"
    . showSpace
    . showsPrec 11 MarloweV1
    . showSpace
    . showsPrec 11 state
    )

instance Eq SomeContractState where
  SomeContractState v state == SomeContractState v' state' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> state == state'

instance Binary SomeContractState where
  put (SomeContractState MarloweV1 state) = do
    put $ SomeMarloweVersion MarloweV1
    put state
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomeContractState MarloweV1 <$> get

instance Variations SomeContractState where
  variations = SomeContractState MarloweV1 <$> variations

instance ToJSON SomeContractState where
  toJSON (SomeContractState MarloweV1 state) = object
    [ "version" .= MarloweV1
    , "state" .= state
    ]

data SomeTransaction = forall v. SomeTransaction
  { version :: MarloweVersion v
  , input :: TxOutRef
  , consumedBy :: Maybe TxId
  , transaction :: Transaction v
  }

instance Show SomeTransaction where
  showsPrec p (SomeTransaction MarloweV1 input consumedBy transaction) = showParen (p >= 11)
    ( showString "SomeTransaction"
    . showSpace
    . showsPrec 11 MarloweV1
    . showSpace
    . showsPrec 11 input
    . showSpace
    . showsPrec 11 consumedBy
    . showSpace
    . showsPrec 11 transaction
    )

instance Eq SomeTransaction where
  SomeTransaction v input consumedBy tx == SomeTransaction v' input' consumedBy' tx' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> input == input' && consumedBy == consumedBy' && tx == tx'

instance Binary SomeTransaction where
  put (SomeTransaction MarloweV1 input consumedBy tx) = do
    put $ SomeMarloweVersion MarloweV1
    put input
    put consumedBy
    put tx
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomeTransaction MarloweV1 <$> get <*> get <*> get

instance Variations SomeTransaction where
  variations = SomeTransaction MarloweV1 <$> variations `varyAp` variations `varyAp` variations

instance ToJSON SomeTransaction where
  toJSON (SomeTransaction MarloweV1 input consumedBy tx) = object
    [ "version" .= MarloweV1
    , "input" .= input
    , "consumedBy" .= consumedBy
    , "transaction" .= tx
    ]

data SomeTransactions = forall v. SomeTransactions (MarloweVersion v) [Transaction v]

instance Show SomeTransactions where
  showsPrec p (SomeTransactions MarloweV1 transactions) = showParen (p >= 11)
    ( showString "SomeTransactions"
    . showSpace
    . showsPrec 11 MarloweV1
    . showSpace
    . showsPrec 11 transactions
    )

instance Eq SomeTransactions where
  SomeTransactions v txs == SomeTransactions v' txs' = case testEquality v v' of
    Nothing -> False
    Just Refl -> case v of
      MarloweV1 -> txs == txs'

instance Binary SomeTransactions where
  put (SomeTransactions MarloweV1 txs) = do
    put $ SomeMarloweVersion MarloweV1
    put txs
  get = do
    SomeMarloweVersion MarloweV1 <- get
    SomeTransactions MarloweV1 <$> get

instance Variations SomeTransactions where
  variations = SomeTransactions MarloweV1 <$> variations

instance ToJSON SomeTransactions where
  toJSON (SomeTransactions MarloweV1 txs) = object
    [ "version" .= MarloweV1
    , "transactions" .= txs
    ]

data ContractState v = ContractState
  { contractId :: ContractId
  , roleTokenMintingPolicyId :: PolicyId
  , metadata :: MarloweTransactionMetadata
  , initialBlock :: BlockHeader
  , initialOutput :: TransactionScriptOutput v
  , latestBlock :: BlockHeader
  , latestOutput :: Maybe (TransactionScriptOutput v)
  , unclaimedPayouts :: Map TxOutRef (Payout v)
  } deriving (Generic)

deriving instance Show (ContractState 'V1)
deriving instance Eq (ContractState 'V1)
deriving instance ToJSON (ContractState 'V1)
deriving instance Variations (ContractState 'V1)
deriving instance Binary (ContractState 'V1)

data PayoutRef = PayoutRef
  { contractId :: ContractId
  , payout :: TxOutRef
  , rolesCurrency :: PolicyId
  , role :: TokenName
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, Binary, Variations)

data Withdrawal = Withdrawal
  { block :: BlockHeader
  , withdrawnPayouts :: Map TxOutRef PayoutRef
  , withdrawalTx :: TxId
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, Binary, Variations)

data Order = Ascending | Descending
  deriving stock (Eq, Show, Read, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, Binary, Variations)

instance MessageVariations MarloweQuery where
  agencyVariations = join $ NE.fromList
    [ pure $ SomePeerHasAgency $ ClientAgency TokReq
    , do
        SomeStRes tok <- variations
        pure $ SomePeerHasAgency $ ServerAgency $ TokRes tok
    ]
  messageVariations = \case
    ClientAgency TokReq -> join $ NE.fromList
      [ pure $ SomeMessage MsgDone
      , do
          SomeRequest req <- variations
          pure $ SomeMessage $ MsgRequest req
      ]
    ServerAgency (TokRes tok) -> SomeMessage . MsgRespond <$> resultVariations tok

resultVariations :: StRes a -> NE.NonEmpty a
resultVariations = \case
  TokBoth a b -> (,) <$> resultVariations a `varyAp` resultVariations b
  TokContractHeaders -> variations
  TokContractState -> variations
  TokTransaction -> variations
  TokTransactions -> variations
  TokWithdrawal -> variations
  TokWithdrawals -> variations

instance MessageToJSON MarloweQuery where
  messageToJSON = \case
    ClientAgency TokReq -> \case
      MsgRequest req -> object [ "request" .= req ]
      MsgDone -> String "done"
    ServerAgency (TokRes req) -> \case
      MsgRespond a -> object [ "respond" .= responseToJSON req a ]

    where
      responseToJSON :: StRes a -> a -> Value
      responseToJSON = \case
        TokContractHeaders -> toJSON
        TokContractState -> toJSON
        TokTransaction -> toJSON
        TokTransactions -> toJSON
        TokWithdrawal -> toJSON
        TokWithdrawals -> toJSON
        TokBoth a b -> toJSON . bimap (responseToJSON a) (responseToJSON b)

instance ShowProtocol MarloweQuery where
  showsPrecMessage p =  \case
    ClientAgency TokReq -> \case
      MsgRequest req -> showParen (p >= 11) (showString "MsgRequest" . showSpace . showsPrec 11 req)
      MsgDone -> showString "MsgDone"
    ServerAgency (TokRes req) -> \case
      MsgRespond a -> showParen (p >= 11) (showString "MsgRespond" . showSpace . showsPrecResult req 11 a)
    where
      showsPrecResult :: StRes a -> Int -> a -> String -> String
      showsPrecResult = \case
        TokContractHeaders -> showsPrec
        TokContractState -> showsPrec
        TokTransaction -> showsPrec
        TokTransactions -> showsPrec
        TokWithdrawal -> showsPrec
        TokWithdrawals -> showsPrec
        TokBoth ta tb -> \_ (a, b) -> showParen True (showsPrecResult ta 0 a . showCommaSpace . showsPrecResult tb 0 b)
  showsPrecServerHasAgency p (TokRes req) = showParen (p >= 11) (showString "TokRes" . showSpace . showsPrec 11 req)
  showsPrecClientHasAgency _ TokReq = showString "TokReq"

instance MessageEq MarloweQuery where
  messageEq = \case
    AnyMessageAndAgency (ClientAgency TokReq) (MsgRequest req) -> \case
      AnyMessageAndAgency (ClientAgency TokReq) (MsgRequest req') -> reqEq req req'
      _ -> False
    AnyMessageAndAgency (ClientAgency TokReq) MsgDone -> \case
      AnyMessageAndAgency (ClientAgency TokReq) MsgDone -> True
      _ -> False
    AnyMessageAndAgency (ServerAgency (TokRes req)) (MsgRespond a) -> \case
      AnyMessageAndAgency (ServerAgency (TokRes req')) (MsgRespond a') -> resultEq req req' a a'
      _ -> False
    where
      reqEq :: Request a -> Request a1 -> Bool
      reqEq (ReqBoth a b) (ReqBoth a' b') = reqEq a a' && reqEq b b'
      reqEq (ReqBoth _ _) _ = False
      reqEq (ReqContractHeaders cFilter range) (ReqContractHeaders cFilter' range') = range == range' && cFilter == cFilter'
      reqEq (ReqContractHeaders _ _) _ = False
      reqEq (ReqContractState contractId) (ReqContractState contractId') = contractId == contractId'
      reqEq (ReqContractState _) _ = False
      reqEq (ReqTransaction txId) (ReqTransaction txId') = txId == txId'
      reqEq (ReqTransaction _) _ = False
      reqEq (ReqTransactions contractId) (ReqTransactions contractId') = contractId == contractId'
      reqEq (ReqTransactions _) _ = False
      reqEq (ReqWithdrawal txId) (ReqWithdrawal txId') = txId == txId'
      reqEq (ReqWithdrawal _) _ = False
      reqEq (ReqWithdrawals wFilter range) (ReqWithdrawals filter' range') = wFilter == filter' && range == range'
      reqEq (ReqWithdrawals _ _) _ = False

      resultEq :: StRes a -> StRes b -> a -> b -> Bool
      resultEq (TokBoth ta tb) (TokBoth ta' tb') = \(a, b) (a', b') ->
        resultEq ta ta' a a' && resultEq tb tb' b b'
      resultEq (TokBoth _ _) _ = const $ const False
      resultEq TokContractHeaders TokContractHeaders = (==)
      resultEq TokContractHeaders _ = const $ const False
      resultEq TokContractState TokContractState = (==)
      resultEq TokContractState _ = const $ const False
      resultEq TokTransaction TokTransaction = (==)
      resultEq TokTransaction _ = const $ const False
      resultEq TokTransactions TokTransactions = (==)
      resultEq TokTransactions _ = const $ const False
      resultEq TokWithdrawal TokWithdrawal = (==)
      resultEq TokWithdrawal _ = const $ const False
      resultEq TokWithdrawals TokWithdrawals = (==)
      resultEq TokWithdrawals _ = const $ const False

requestToSt :: Request x -> StRes x
requestToSt = \case
  ReqContractHeaders _ _ -> TokContractHeaders
  ReqContractState _ -> TokContractState
  ReqTransaction _ -> TokTransaction
  ReqTransactions _ -> TokTransactions
  ReqWithdrawal _ -> TokWithdrawal
  ReqWithdrawals _ _ -> TokWithdrawals
  ReqBoth r1 r2 -> TokBoth (requestToSt r1) (requestToSt r2)
