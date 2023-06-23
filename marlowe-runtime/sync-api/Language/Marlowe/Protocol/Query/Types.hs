{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Marlowe.Protocol.Query.Types where

import Cardano.Api (NetworkId)
import Data.Aeson (FromJSON, ToJSON(..), Value(String), object, (.=))
import Data.Bifunctor (Bifunctor(..))
import Data.Binary (Binary(..), getWord8, putWord8)
import Data.Function (on)
import qualified Data.List.NonEmpty as NE
import Data.Map (Map)
import Data.Set (Set)
import Data.Time (UTCTime)
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import Data.Version (Version)
import GHC.Generics (Generic)
import GHC.Show (showSpace)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, ChainPoint, PolicyId, TokenName, TxId, TxOutRef)
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
import Language.Marlowe.Runtime.Core.ScriptRegistry ()
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Network.Protocol.Codec.Spec (Variations(..), varyAp)
import Network.Protocol.Handshake.Types (HasSignature(..))
import Network.Protocol.Query.Types

type MarloweQuery = Query MarloweSyncRequest

instance HasSignature MarloweSyncRequest where
  signature _ = "MarloweSyncRequest"

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

data RuntimeStatus = RuntimeStatus
  { nodeTip :: ChainPoint
  , nodeTipUTC :: UTCTime
  , runtimeChainTip :: ChainPoint
  , runtimeChainTipUTC :: UTCTime
  , runtimeTip :: ChainPoint
  , runtimeTipUTC :: UTCTime
  , networkId :: NetworkId
  , runtimeVersion :: Version
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (Binary, Variations)

data MarloweSyncRequest a where
  ReqStatus :: MarloweSyncRequest RuntimeStatus
  ReqContractHeaders :: ContractFilter -> Range ContractId -> MarloweSyncRequest (Maybe (Page ContractId ContractHeader))
  ReqContractState :: ContractId -> MarloweSyncRequest (Maybe SomeContractState)
  ReqTransaction :: TxId -> MarloweSyncRequest (Maybe SomeTransaction)
  ReqTransactions :: ContractId -> MarloweSyncRequest (Maybe SomeTransactions)
  ReqWithdrawal :: TxId -> MarloweSyncRequest (Maybe Withdrawal)
  ReqWithdrawals :: WithdrawalFilter -> Range TxId -> MarloweSyncRequest (Maybe (Page TxId Withdrawal))

deriving instance Show (MarloweSyncRequest a)
deriving instance Eq (MarloweSyncRequest a)

instance Request MarloweSyncRequest where
  data Tag MarloweSyncRequest a where
    TagStatus :: Tag MarloweSyncRequest RuntimeStatus
    TagContractHeaders :: Tag MarloweSyncRequest (Maybe (Page ContractId ContractHeader))
    TagContractState :: Tag MarloweSyncRequest (Maybe SomeContractState)
    TagTransaction :: Tag MarloweSyncRequest (Maybe SomeTransaction)
    TagTransactions :: Tag MarloweSyncRequest (Maybe SomeTransactions)
    TagWithdrawal :: Tag MarloweSyncRequest (Maybe Withdrawal)
    TagWithdrawals :: Tag MarloweSyncRequest (Maybe (Page TxId Withdrawal))
  tagFromReq = \case
    ReqStatus -> TagStatus
    ReqContractHeaders _ _ -> TagContractHeaders
    ReqContractState _ -> TagContractState
    ReqTransaction _ -> TagTransaction
    ReqTransactions _ -> TagTransactions
    ReqWithdrawal _ -> TagWithdrawal
    ReqWithdrawals _ _ -> TagWithdrawals
  tagEq = \case
    TagStatus -> \case
      TagStatus -> Just Refl
      _ -> Nothing
    TagContractHeaders -> \case
      TagContractHeaders -> Just Refl
      _ -> Nothing
    TagContractState -> \case
      TagContractState -> Just Refl
      _ -> Nothing
    TagTransaction -> \case
      TagTransaction -> Just Refl
      _ -> Nothing
    TagTransactions -> \case
      TagTransactions -> Just Refl
      _ -> Nothing
    TagWithdrawal -> \case
      TagWithdrawal -> Just Refl
      _ -> Nothing
    TagWithdrawals -> \case
      TagWithdrawals -> Just Refl
      _ -> Nothing

deriving instance Show (Tag MarloweSyncRequest a)
deriving instance Eq (Tag MarloweSyncRequest a)

instance BinaryRequest MarloweSyncRequest where
  getReq = do
    tag <- getWord8
    case tag of
      0x01 -> SomeRequest <$> (ReqContractHeaders <$> get <*> get)
      0x02 -> SomeRequest . ReqContractState <$> get
      0x03 -> SomeRequest . ReqTransaction <$> get
      0x04 -> SomeRequest . ReqTransactions <$> get
      0x05 -> SomeRequest . ReqWithdrawal <$> get
      0x06 -> SomeRequest <$> (ReqWithdrawals <$> get <*> get)
      0x07 -> pure $ SomeRequest ReqStatus
      _ -> fail "Invalid MarloweSyncRequest tag"

  putReq req = case req of
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
    ReqStatus -> putWord8 0x07

  getResult = \case
    TagContractHeaders -> get
    TagContractState -> get
    TagTransaction -> get
    TagTransactions -> get
    TagWithdrawal -> get
    TagWithdrawals -> get
    TagStatus -> get

  putResult = \case
    TagContractHeaders -> put
    TagContractState -> put
    TagTransaction -> put
    TagTransactions -> put
    TagWithdrawal -> put
    TagWithdrawals -> put
    TagStatus -> put

instance RequestVariations MarloweSyncRequest where
  tagVariations = NE.fromList
    [ SomeTag TagContractHeaders
    , SomeTag TagContractState
    , SomeTag TagTransaction
    , SomeTag TagTransactions
    , SomeTag TagWithdrawal
    , SomeTag TagWithdrawals
    , SomeTag TagStatus
    ]
  requestVariations = \case
    TagContractHeaders -> ReqContractHeaders <$> variations `varyAp` variations
    TagContractState -> ReqContractState <$> variations
    TagTransaction -> ReqTransaction <$> variations
    TagTransactions -> ReqTransactions <$> variations
    TagWithdrawal -> ReqWithdrawal <$> variations
    TagWithdrawals -> ReqWithdrawals <$> variations `varyAp` variations
    TagStatus -> pure ReqStatus
  resultVariations = \case
    TagContractHeaders -> variations
    TagContractState -> variations
    TagTransaction -> variations
    TagTransactions -> variations
    TagWithdrawal -> variations
    TagWithdrawals -> variations
    TagStatus -> variations

instance ToJSON (MarloweSyncRequest a) where
  toJSON = \case
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
    ReqStatus -> String "get-status"

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

instance OTelRequest MarloweSyncRequest where
  reqTypeName _ = "marlowe_sync_request"
  reqName = \case
    TagContractHeaders -> "contract_headers"
    TagContractState -> "contract_state"
    TagTransaction -> "transaction"
    TagTransactions -> "transactions"
    TagWithdrawal -> "withdrawal"
    TagWithdrawals -> "withdrawals"
    TagStatus -> "status"

instance ShowRequest MarloweSyncRequest where
  showsPrecResult p = \case
    TagContractHeaders -> showsPrec p
    TagContractState -> showsPrec p
    TagTransaction -> showsPrec p
    TagTransactions -> showsPrec p
    TagWithdrawal -> showsPrec p
    TagWithdrawals -> showsPrec p
    TagStatus -> showsPrec p

instance RequestEq MarloweSyncRequest where
  resultEq TagContractHeaders = (==)
  resultEq TagContractState = (==)
  resultEq TagTransaction = (==)
  resultEq TagTransactions = (==)
  resultEq TagWithdrawal = (==)
  resultEq TagWithdrawals = (==)
  resultEq TagStatus = (==)
