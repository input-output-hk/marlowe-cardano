{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Client
  ( Page(..)
  , getContract
  , getContracts
  , getTransaction
  , getTransactions
  , getWithdrawal
  , getWithdrawals
  , healthcheck
  , postContract
  , postContractCreateTx
  , postTransaction
  , postTransactionCreateTx
  , postWithdrawal
  , postWithdrawalCreateTx
  , putContract
  , putTransaction
  , putWithdrawal
  ) where

import Control.Monad.Error.Class (MonadError(catchError))
import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Runtime.Web.API
  (API, GetContractsResponse, GetTransactionsResponse, GetWithdrawalsResponse, ListObject(..), api, retractLink)
import Language.Marlowe.Runtime.Web.Types
import Servant (ResponseHeader(..), getResponse, lookupResponseHeader, type (:<|>)((:<|>)))
import Servant.Client (Client, ClientM)
import qualified Servant.Client as Servant
import Servant.Pagination (ExtractRange(extractRange), HasPagination(..), PutRange(..), Range, Ranges)

client :: Client ClientM API
client = Servant.client api

data Page field resource = Page
  { totalCount :: Int
  , nextRange :: Maybe (Range field (RangeType resource field))
  , items :: [resource]
  }
  deriving (Eq, Show)

healthcheck :: ClientM Bool
healthcheck = do
  let _ :<|> _ :<|> healthcheck' = client
  (True <$ healthcheck') `catchError` const (pure False)

getContracts
  :: Maybe (Set PolicyId)
  -> Maybe (Set Text)
  -> Maybe (Range "contractId" TxOutRef)
  -> ClientM (Page "contractId" ContractHeader)
getContracts roleCurrencies tags range = do
  let contractsClient :<|> _ = client
  let getContracts' :<|> _ = contractsClient
  response <- getContracts' (foldMap Set.toList roleCurrencies) (foldMap Set.toList tags)
    $ putRange <$> range
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  pure Page
    { totalCount
    , nextRange = extractRangeSingleton @GetContractsResponse <$> nextRanges
    , items = retractLink @"contract" . retractLink @"transactions" <$> items
    }

postContract
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostContractsRequest
  -> ClientM (CreateTxEnvelope CardanoTxBody)
postContract changeAddress otherAddresses collateralUtxos request = do
  let (_ :<|> (postContractCreateTxBody' :<|> _) :<|> _) :<|> _ = client
  response <- postContractCreateTxBody'
    request
    changeAddress
    (setToCommaList <$> otherAddresses)
    (setToCommaList <$> collateralUtxos)
  pure $ retractLink response

postContractCreateTx
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostContractsRequest
  -> ClientM (CreateTxEnvelope CardanoTx)
postContractCreateTx changeAddress otherAddresses collateralUtxos request = do
  let (_ :<|> (_ :<|> postContractCreateTx') :<|> _) :<|> _ = client
  response <- postContractCreateTx'
    request
    changeAddress
    (setToCommaList <$> otherAddresses)
    (setToCommaList <$> collateralUtxos)
  pure $ retractLink response

getContract :: TxOutRef -> ClientM ContractState
getContract contractId = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = contractsClient
  let getContract' :<|> _ = contractApi contractId
  retractLink <$> getContract'

putContract :: TxOutRef -> TextEnvelope -> ClientM ()
putContract contractId tx = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = contractsClient
  let _ :<|> putContract' :<|> _ = contractApi contractId
  void $ putContract' tx

getWithdrawals
  :: Maybe (Set PolicyId)
  -> Maybe (Range "withdrawalId" TxId)
  -> ClientM (Page "withdrawalId" WithdrawalHeader)
getWithdrawals roleCurrencies range = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let getWithdrawals' :<|> _ = withdrawalsClient
  response <- getWithdrawals' (foldMap Set.toList roleCurrencies) $ putRange <$> range
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  pure Page
    { totalCount
    , nextRange = extractRangeSingleton @GetWithdrawalsResponse <$> nextRanges
    , items = retractLink @"withdrawal" <$> items
    }

postWithdrawal
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostWithdrawalsRequest
  -> ClientM (WithdrawTxEnvelope CardanoTxBody)
postWithdrawal changeAddress otherAddresses collateralUtxos request = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> (postWithdrawal' :<|> _) :<|> _ = withdrawalsClient
  response <- postWithdrawal'
    request
    changeAddress
    (setToCommaList <$> otherAddresses)
    (setToCommaList <$> collateralUtxos)
  pure $ retractLink response

postWithdrawalCreateTx
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> PostWithdrawalsRequest
  -> ClientM (WithdrawTxEnvelope CardanoTx)
postWithdrawalCreateTx changeAddress otherAddresses collateralUtxos request = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> (_ :<|> postWithdrawalCreateTx') :<|> _ = withdrawalsClient
  response <- postWithdrawalCreateTx'
    request
    changeAddress
    (setToCommaList <$> otherAddresses)
    (setToCommaList <$> collateralUtxos)
  pure $ retractLink response

getWithdrawal :: TxId -> ClientM Withdrawal
getWithdrawal withdrawalId = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = withdrawalsClient
  let getWithdrawal' :<|> _ = contractApi withdrawalId
  getWithdrawal'

putWithdrawal :: TxId -> TextEnvelope -> ClientM ()
putWithdrawal withdrawalId tx = do
  let _ :<|> withdrawalsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = withdrawalsClient
  let _ :<|> putWithdrawal' = contractApi withdrawalId
  void $ putWithdrawal' tx

getTransactions
  :: TxOutRef
  -> Maybe (Range "transactionId" TxId)
  -> ClientM (Page "transactionId" TxHeader)
getTransactions contractId range = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = contractsClient
  let _ :<|> _ :<|> getTransactions' :<|> _ = contractApi contractId
  response <- getTransactions' $ putRange <$> range
  totalCount <- reqHeaderValue $ lookupResponseHeader @"Total-Count" response
  nextRanges <- headerValue $ lookupResponseHeader @"Next-Range" response
  let ListObject items = getResponse response
  pure Page
    { totalCount
    , nextRange = extractRangeSingleton @GetTransactionsResponse <$> nextRanges
    , items = retractLink <$> items
    }

postTransaction
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> TxOutRef
  -> PostTransactionsRequest
  -> ClientM (ApplyInputsTxEnvelope CardanoTxBody)
postTransaction changeAddress otherAddresses collateralUtxos contractId request = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = contractsClient
  let _ :<|> _ :<|> _ :<|> (postTransaction' :<|> _) :<|> _ = contractApi contractId
  response <- postTransaction'
    request
    changeAddress
    (setToCommaList <$> otherAddresses)
    (setToCommaList <$> collateralUtxos)
  pure $ retractLink response

postTransactionCreateTx
  :: Address
  -> Maybe (Set Address)
  -> Maybe (Set TxOutRef)
  -> TxOutRef
  -> PostTransactionsRequest
  -> ClientM (ApplyInputsTxEnvelope CardanoTx)
postTransactionCreateTx changeAddress otherAddresses collateralUtxos contractId request = do
  let (_ :<|> _ :<|> contractApi) :<|> _ = client
  let _ :<|> _ :<|> _ :<|> (_ :<|> postTransactionCreateTx') :<|> _ = contractApi contractId
  response <- postTransactionCreateTx'
    request
    changeAddress
    (setToCommaList <$> otherAddresses)
    (setToCommaList <$> collateralUtxos)
  pure $ retractLink response


getTransaction :: TxOutRef -> TxId -> ClientM Tx
getTransaction contractId transactionId = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = contractsClient
  let _ :<|> _ :<|> _ :<|> _ :<|> transactionApi = contractApi contractId
  let getTransaction' :<|> _ = transactionApi transactionId
  retractLink . retractLink <$> getTransaction'

putTransaction :: TxOutRef -> TxId -> TextEnvelope -> ClientM ()
putTransaction contractId transactionId tx = do
  let contractsClient :<|> _ = client
  let _ :<|> _ :<|> contractApi = contractsClient
  let _ :<|> _ :<|> _ :<|> _ :<|> transactionApi = contractApi contractId
  let _ :<|> putTransaction' = transactionApi transactionId
  void $ putTransaction' tx

setToCommaList :: Set a -> CommaList a
setToCommaList = CommaList . Set.toList

reqHeaderValue :: forall name a. KnownSymbol name => ResponseHeader name a -> ClientM a
reqHeaderValue = \case
  Header a -> pure a
  UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
  MissingHeader -> liftIO $ fail $ "Required header missing " <> symbolVal (Proxy @name)

headerValue :: forall name a. KnownSymbol name => ResponseHeader name a -> ClientM (Maybe a)
headerValue = \case
  Header a -> pure $ Just a
  UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
  MissingHeader -> pure Nothing

extractRangeSingleton
  :: HasPagination resource field
  => Ranges '[field] resource
  -> Range field (RangeType resource field)
extractRangeSingleton = fromJust . extractRange
