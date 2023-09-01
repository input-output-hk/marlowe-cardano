{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Client (
  Page (..),
  getContract,
  getContractNext,
  getContractSource,
  getContractSourceAdjacency,
  getContractSourceAdjacencyStatus,
  getContractSourceClosure,
  getContractSourceClosureStatus,
  getContractSourceStatus,
  getContractStatus,
  getContracts,
  getContractsStatus,
  getPayout,
  getPayoutStatus,
  getPayouts,
  getPayoutsStatus,
  getTransaction,
  getTransactionStatus,
  getTransactions,
  getTransactionsStatus,
  getWithdrawal,
  getWithdrawalStatus,
  getWithdrawals,
  getWithdrawalsStatus,
  healthcheck,
  postContract,
  postContractCreateTx,
  postContractCreateTxStatus,
  postContractSource,
  postContractSourceStatus,
  postContractStatus,
  postTransaction,
  postTransactionCreateTx,
  postTransactionCreateTxStatus,
  postTransactionStatus,
  postWithdrawal,
  postWithdrawalCreateTx,
  postWithdrawalCreateTxStatus,
  postWithdrawalStatus,
  putContract,
  putContractStatus,
  putTransaction,
  putTransactionStatus,
  putWithdrawal,
  putWithdrawalStatus,
) where

import Control.Monad.Error.Class (MonadError (catchError))
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Version (Version)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Core.V1.Next
import Language.Marlowe.Core.V1.Semantics.Types (Contract)
import Language.Marlowe.Object.Types (Label, ObjectBundle)
import Language.Marlowe.Runtime.Web (GetPayoutsResponse)
import Language.Marlowe.Runtime.Web.API (
  API,
  GetContractsResponse,
  GetTransactionsResponse,
  GetWithdrawalsResponse,
  ListObject (..),
  api,
  retractLink,
 )
import Language.Marlowe.Runtime.Web.Types
import Pipes (Producer)
import Servant (HasResponseHeader, ResponseHeader (..), getResponse, lookupResponseHeader, type (:<|>) ((:<|>)))
import Servant.API (Headers)
import Servant.Client (Client)
import Servant.Client.Streaming (ClientM)
import qualified Servant.Client.Streaming as ServantStreaming
import Servant.Pagination (ExtractRange (extractRange), HasPagination (..), PutRange (..), Range, Ranges)
import Servant.Pipes ()

client :: Client ClientM API
client = ServantStreaming.client api

data Page field resource = Page
  { totalCount :: Int
  , nextRange :: Maybe (Range field (RangeType resource field))
  , items :: [resource]
  }
  deriving (Eq, Show)

healthcheck :: ClientM Bool
healthcheck = do
  let _ :<|> _ :<|> _ :<|> healthcheck' = client
  (True <$ healthcheck') `catchError` const (pure False)

extractStatus
  :: ( HasResponseHeader "X-Node-Tip" ChainTip hs
     , HasResponseHeader "X-Runtime-Chain-Tip" ChainTip hs
     , HasResponseHeader "X-Runtime-Tip" ChainTip hs
     , HasResponseHeader "X-Network-Id" NetworkId hs
     , HasResponseHeader "X-Runtime-Version" Version hs
     )
  => Headers hs a
  -> ClientM RuntimeStatus
extractStatus response =
  RuntimeStatus
    <$> (reqHeaderValue $ lookupResponseHeader @"X-Node-Tip" response)
    <*> (reqHeaderValue $ lookupResponseHeader @"X-Runtime-Chain-Tip" response)
    <*> (reqHeaderValue $ lookupResponseHeader @"X-Runtime-Tip" response)
    <*> (reqHeaderValue $ lookupResponseHeader @"X-Network-Id" response)
    <*> (reqHeaderValue $ lookupResponseHeader @"X-Runtime-Version" response)

getContractsStatus
  :: Maybe (Set PolicyId)
  -> Maybe (Set Text)
  -> Maybe (Set Address)
  -> Maybe (Set AssetId)
  -> Maybe (Range "contractId" TxOutRef)
  -> ClientM (RuntimeStatus, Page "contractId" ContractHeader)
getContractsStatus roleCurrencies tags partyAddresses partyRoles range = do
  let contractsClient :<|> _ = client
  let getContracts' :<|> _ = contractsClient
  response <-
    getContracts'
      (foldMap Set.toList roleCurrencies)
      (foldMap Set.toList tags)
      (foldMap Set.toList partyAddresses)
      (foldMap Set.toList partyRoles)
      (putRange <$> range)
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  status <- extractStatus response
  pure
    ( status
    , Page
        { totalCount
        , nextRange = extractRangeSingleton @GetContractsResponse <$> nextRanges
        , items = retractLink @"contract" . retractLink @"transactions" <$> items
        }
    )

getContracts
  :: Maybe (Set PolicyId)
  -> Maybe (Set Text)
  -> Maybe (Set Address)
  -> Maybe (Set AssetId)
  -> Maybe (Range "contractId" TxOutRef)
  -> ClientM (Page "contractId" ContractHeader)
getContracts = (fmap . fmap . fmap . fmap . fmap) snd . getContractsStatus

postContractStatus
  :: Maybe StakeAddress
  -> Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostContractsRequest
  -> ClientM (RuntimeStatus, CreateTxEnvelope CardanoTxBody)
postContractStatus stakeAddress changeAddress otherAddresses collateralUtxos request = do
  let (_ :<|> (postContractCreateTxBody' :<|> _) :<|> _) :<|> _ = client
  response <-
    postContractCreateTxBody'
      request
      stakeAddress
      changeAddress
      (setToCommaList <$> otherAddresses)
      (setToCommaList <$> collateralUtxos)
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

postContract
  :: Maybe StakeAddress
  -> Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostContractsRequest
  -> ClientM (CreateTxEnvelope CardanoTxBody)
postContract = (fmap . fmap . fmap . fmap . fmap) snd . postContractStatus

postContractCreateTxStatus
  :: Maybe StakeAddress
  -> Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostContractsRequest
  -> ClientM (RuntimeStatus, CreateTxEnvelope CardanoTx)
postContractCreateTxStatus stakeAddress changeAddress otherAddresses collateralUtxos request = do
  let (_ :<|> (_ :<|> postContractCreateTx') :<|> _) :<|> _ = client
  response <-
    postContractCreateTx'
      request
      stakeAddress
      changeAddress
      (setToCommaList <$> otherAddresses)
      (setToCommaList <$> collateralUtxos)
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

postContractCreateTx
  :: Maybe StakeAddress
  -> Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostContractsRequest
  -> ClientM (CreateTxEnvelope CardanoTx)
postContractCreateTx = (fmap . fmap . fmap . fmap . fmap) snd . postContractCreateTxStatus

postContractSourceStatus
  :: Label
  -> Producer ObjectBundle IO ()
  -> ClientM (RuntimeStatus, PostContractSourceResponse)
postContractSourceStatus main bundles = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> _ :<|> contractSourcesClient = contractsClient
  let postContractSource' :<|> _ = contractSourcesClient
  response <- postContractSource' main bundles
  status <- extractStatus response
  pure (status, getResponse response)

postContractSource
  :: Label
  -> Producer ObjectBundle IO ()
  -> ClientM PostContractSourceResponse
postContractSource = (fmap . fmap) snd . postContractSourceStatus

getContractSourceStatus :: ContractSourceId -> Bool -> ClientM (RuntimeStatus, Contract)
getContractSourceStatus contractSourceId expand = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> _ :<|> contractSourcesClient = contractsClient
  let _ :<|> contractSourceClient = contractSourcesClient
  let getContractSource' :<|> _ = contractSourceClient contractSourceId
  response <- getContractSource' expand
  status <- extractStatus response
  pure (status, getResponse response)

getContractSource :: ContractSourceId -> Bool -> ClientM Contract
getContractSource = (fmap . fmap) snd . getContractSourceStatus

getContractSourceAdjacencyStatus :: ContractSourceId -> ClientM (RuntimeStatus, Set ContractSourceId)
getContractSourceAdjacencyStatus contractSourceId = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> _ :<|> contractSourcesClient = contractsClient
  let _ :<|> contractSourceClient = contractSourcesClient
  let _ :<|> getContractSourceAdjacency' :<|> _ = contractSourceClient contractSourceId
  response <- getContractSourceAdjacency'
  status <- extractStatus response
  pure (status, Set.fromList $ results $ getResponse response)

getContractSourceAdjacency :: ContractSourceId -> ClientM (Set ContractSourceId)
getContractSourceAdjacency = fmap snd . getContractSourceAdjacencyStatus

getContractSourceClosureStatus :: ContractSourceId -> ClientM (RuntimeStatus, Set ContractSourceId)
getContractSourceClosureStatus contractSourceId = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> _ :<|> contractSourcesClient = contractsClient
  let _ :<|> contractSourceClient = contractSourcesClient
  let _ :<|> _ :<|> getContractSourceClosure' = contractSourceClient contractSourceId
  response <- getContractSourceClosure'
  status <- extractStatus response
  pure (status, Set.fromList $ results $ getResponse response)

getContractSourceClosure :: ContractSourceId -> ClientM (Set ContractSourceId)
getContractSourceClosure = fmap snd . getContractSourceClosureStatus

getContractStatus :: TxOutRef -> ClientM (RuntimeStatus, ContractState)
getContractStatus contractId = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let getContract' :<|> _ = contractApi contractId
  response <- getContract'
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

getContract :: TxOutRef -> ClientM ContractState
getContract = fmap snd . getContractStatus

getContractNextStatus :: TxOutRef -> UTCTime -> UTCTime -> [Party] -> ClientM (RuntimeStatus, Next)
getContractNextStatus contractId validityStart validityEnd parties = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> _ :<|> next' :<|> _ = contractApi contractId
  response <- next' validityStart validityEnd parties
  status <- extractStatus response
  pure (status, getResponse response)

getContractNext :: TxOutRef -> UTCTime -> UTCTime -> [Party] -> ClientM Next
getContractNext = (fmap . fmap . fmap . fmap) snd . getContractNextStatus

putContractStatus :: TxOutRef -> TextEnvelope -> ClientM RuntimeStatus
putContractStatus contractId tx = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> putContract' :<|> _ = contractApi contractId
  response <- putContract' tx
  extractStatus response

putContract :: TxOutRef -> TextEnvelope -> ClientM ()
putContract = fmap void . putContractStatus

getWithdrawalsStatus
  :: Maybe (Set PolicyId)
  -> Maybe (Range "withdrawalId" TxId)
  -> ClientM (RuntimeStatus, Page "withdrawalId" WithdrawalHeader)
getWithdrawalsStatus roleCurrencies range = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let getWithdrawals' :<|> _ = withdrawalsClient
  response <- getWithdrawals' (foldMap Set.toList roleCurrencies) $ putRange <$> range
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  status <- extractStatus response
  pure
    ( status
    , Page
        { totalCount
        , nextRange = extractRangeSingleton @GetWithdrawalsResponse <$> nextRanges
        , items = retractLink @"withdrawal" <$> items
        }
    )

getWithdrawals
  :: Maybe (Set PolicyId)
  -> Maybe (Range "withdrawalId" TxId)
  -> ClientM (Page "withdrawalId" WithdrawalHeader)
getWithdrawals = (fmap . fmap) snd . getWithdrawalsStatus

postWithdrawalStatus
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostWithdrawalsRequest
  -> ClientM (RuntimeStatus, WithdrawTxEnvelope CardanoTxBody)
postWithdrawalStatus changeAddress otherAddresses collateralUtxos request = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> (postWithdrawal' :<|> _) :<|> _ = withdrawalsClient
  response <-
    postWithdrawal'
      request
      changeAddress
      (setToCommaList <$> otherAddresses)
      (setToCommaList <$> collateralUtxos)
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

postWithdrawal
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostWithdrawalsRequest
  -> ClientM (WithdrawTxEnvelope CardanoTxBody)
postWithdrawal = (fmap . fmap . fmap . fmap) snd . postWithdrawalStatus

postWithdrawalCreateTxStatus
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostWithdrawalsRequest
  -> ClientM (RuntimeStatus, WithdrawTxEnvelope CardanoTx)
postWithdrawalCreateTxStatus changeAddress otherAddresses collateralUtxos request = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> (_ :<|> postWithdrawalCreateTx') :<|> _ = withdrawalsClient
  response <-
    postWithdrawalCreateTx'
      request
      changeAddress
      (setToCommaList <$> otherAddresses)
      (setToCommaList <$> collateralUtxos)
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

postWithdrawalCreateTx
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostWithdrawalsRequest
  -> ClientM (WithdrawTxEnvelope CardanoTx)
postWithdrawalCreateTx = (fmap . fmap . fmap . fmap) snd . postWithdrawalCreateTxStatus

getWithdrawalStatus :: TxId -> ClientM (RuntimeStatus, Withdrawal)
getWithdrawalStatus withdrawalId = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = withdrawalsClient
  let getWithdrawal' :<|> _ = contractApi withdrawalId
  response <- getWithdrawal'
  status <- extractStatus response
  pure (status, getResponse response)

getWithdrawal :: TxId -> ClientM Withdrawal
getWithdrawal = fmap snd . getWithdrawalStatus

putWithdrawalStatus :: TxId -> TextEnvelope -> ClientM RuntimeStatus
putWithdrawalStatus withdrawalId tx = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = withdrawalsClient
  let _ :<|> putWithdrawal' = contractApi withdrawalId
  response <- putWithdrawal' tx
  extractStatus response

putWithdrawal :: TxId -> TextEnvelope -> ClientM ()
putWithdrawal = fmap void . putWithdrawalStatus

getPayoutsStatus
  :: Maybe (Set TxOutRef)
  -> Maybe (Set AssetId)
  -> Maybe PayoutStatus
  -> Maybe (Range "payoutId" TxOutRef)
  -> ClientM (RuntimeStatus, Page "payoutId" PayoutHeader)
getPayoutsStatus contractIds roleTokens unclaimed range = do
  let _ :<|> _ :<|> payoutsClient :<|> _ = client
  let getPayouts' :<|> _ = payoutsClient
  response <-
    getPayouts' (foldMap Set.toList contractIds) (foldMap Set.toList roleTokens) unclaimed $
      putRange <$> range
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  status <- extractStatus response
  pure
    ( status
    , Page
        { totalCount
        , nextRange = extractRangeSingleton @GetPayoutsResponse <$> nextRanges
        , items = retractLink @"payout" <$> items
        }
    )

getPayouts
  :: Maybe (Set TxOutRef)
  -> Maybe (Set AssetId)
  -> Maybe PayoutStatus
  -> Maybe (Range "payoutId" TxOutRef)
  -> ClientM (Page "payoutId" PayoutHeader)
getPayouts = (fmap . fmap . fmap . fmap) snd . getPayoutsStatus

getPayoutStatus
  :: TxOutRef
  -> ClientM (RuntimeStatus, PayoutState)
getPayoutStatus payoutId = do
  let _ :<|> _ :<|> payoutsClient :<|> _ = client
  let _ :<|> getPayout' = payoutsClient
  response <- getPayout' payoutId
  status <- extractStatus response
  pure
    ( status
    , retractLink $ retractLink $ retractLink $ getResponse response
    )

getPayout
  :: TxOutRef
  -> ClientM PayoutState
getPayout = fmap snd . getPayoutStatus

getTransactionsStatus
  :: TxOutRef
  -> Maybe (Range "transactionId" TxId)
  -> ClientM (RuntimeStatus, Page "transactionId" TxHeader)
getTransactionsStatus contractId range = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> _ :<|> _ :<|> getTransactions' :<|> _ = contractApi contractId
  response <- getTransactions' $ putRange <$> range
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  status <- extractStatus response
  pure
    ( status
    , Page
        { totalCount
        , nextRange = extractRangeSingleton @GetTransactionsResponse <$> nextRanges
        , items = retractLink <$> items
        }
    )

getTransactions
  :: TxOutRef
  -> Maybe (Range "transactionId" TxId)
  -> ClientM (Page "transactionId" TxHeader)
getTransactions = (fmap . fmap) snd . getTransactionsStatus

postTransactionStatus
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> TxOutRef
  -> PostTransactionsRequest
  -> ClientM (RuntimeStatus, ApplyInputsTxEnvelope CardanoTxBody)
postTransactionStatus changeAddress otherAddresses collateralUtxos contractId request = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> _ :<|> _ :<|> _ :<|> (postTransaction' :<|> _) :<|> _ = contractApi contractId
  response <-
    postTransaction'
      request
      changeAddress
      (setToCommaList <$> otherAddresses)
      (setToCommaList <$> collateralUtxos)
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

postTransaction
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> TxOutRef
  -> PostTransactionsRequest
  -> ClientM (ApplyInputsTxEnvelope CardanoTxBody)
postTransaction = (fmap . fmap . fmap . fmap . fmap) snd . postTransactionStatus

postTransactionCreateTxStatus
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> TxOutRef
  -> PostTransactionsRequest
  -> ClientM (RuntimeStatus, ApplyInputsTxEnvelope CardanoTx)
postTransactionCreateTxStatus changeAddress otherAddresses collateralUtxos contractId request = do
  let (_ :<|> _ :<|> contractApi :<|> _) :<|> _ = client
  let _ :<|> _ :<|> _ :<|> _ :<|> (_ :<|> postTransactionCreateTx') :<|> _ = contractApi contractId
  response <-
    postTransactionCreateTx'
      request
      changeAddress
      (setToCommaList <$> otherAddresses)
      (setToCommaList <$> collateralUtxos)
  status <- extractStatus response
  pure (status, retractLink $ getResponse response)

postTransactionCreateTx
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> TxOutRef
  -> PostTransactionsRequest
  -> ClientM (ApplyInputsTxEnvelope CardanoTx)
postTransactionCreateTx = (fmap . fmap . fmap . fmap . fmap) snd . postTransactionCreateTxStatus

getTransactionStatus :: TxOutRef -> TxId -> ClientM (RuntimeStatus, Tx)
getTransactionStatus contractId transactionId = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> transactionApi = contractApi contractId
  let getTransaction' :<|> _ = transactionApi transactionId
  response <- getTransaction'
  status <- extractStatus response
  pure (status, retractLink $ retractLink $ getResponse response)

getTransaction :: TxOutRef -> TxId -> ClientM Tx
getTransaction = (fmap . fmap) snd . getTransactionStatus

putTransactionStatus :: TxOutRef -> TxId -> TextEnvelope -> ClientM RuntimeStatus
putTransactionStatus contractId transactionId tx = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> _ :<|> _ :<|> _ :<|> _ :<|> transactionApi = contractApi contractId
  let _ :<|> putTransaction' = transactionApi transactionId
  response <- putTransaction' tx
  extractStatus response

putTransaction :: TxOutRef -> TxId -> TextEnvelope -> ClientM ()
putTransaction = (fmap . fmap) void . putTransactionStatus

setToCommaList :: Set a -> CommaList a
setToCommaList = CommaList . Set.toList

reqHeaderValue :: forall name a. (KnownSymbol name) => ResponseHeader name a -> ClientM a
reqHeaderValue = \case
  Header a -> pure a
  UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
  MissingHeader -> liftIO $ fail $ "Required header missing " <> symbolVal (Proxy @name)

headerValue :: forall name a. (KnownSymbol name) => ResponseHeader name a -> ClientM (Maybe a)
headerValue = \case
  Header a -> pure $ Just a
  UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
  MissingHeader -> pure Nothing

extractRangeSingleton
  :: (HasPagination resource field)
  => Ranges '[field] resource
  -> Range field (RangeType resource field)
extractRangeSingleton = fromJust . extractRange
