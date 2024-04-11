{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Contract.Transaction.Client (
  getTransaction,
  getTransactionStatus,
  getTransactions,
  getTransactionsStatus,
  postTransaction,
  postTransactionCreateTx,
  postTransactionCreateTxStatus,
  postTransactionStatus,
  putTransaction,
  putTransactionStatus,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Version (Version)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Language.Marlowe.Runtime.Web.API (RuntimeAPI, runtimeApi)
import Language.Marlowe.Runtime.Web.Adapter.CommaList (
  CommaList (CommaList),
 )
import Language.Marlowe.Runtime.Web.Adapter.Links (retractLink)
import Language.Marlowe.Runtime.Web.Adapter.Servant (ListObject (..))
import Language.Marlowe.Runtime.Web.Contract.Transaction.API (
  GetTransactionsResponse,
 )
import Language.Marlowe.Runtime.Web.Core.Address (
  Address,
 )

import Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId)
import Language.Marlowe.Runtime.Web.Core.Tip (ChainTip)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TextEnvelope,
  TxId,
  TxOutRef,
 )
import Language.Marlowe.Runtime.Web.Status (
  RuntimeStatus (RuntimeStatus),
 )
import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTxEnvelope,
  CardanoTx,
  CardanoTxBody,
  Tx,
  TxHeader,
 )
import Language.Marlowe.Runtime.Web.Withdrawal.API (
  PostTransactionsRequest,
 )
import Servant (HasResponseHeader, ResponseHeader (..), getResponse, lookupResponseHeader, type (:<|>) ((:<|>)))
import Servant.API (Headers)
import Servant.Client (Client)
import Servant.Client.Streaming (ClientM)
import qualified Servant.Client.Streaming as ServantStreaming
import Servant.Pagination (ExtractRange (extractRange), HasPagination (..), PutRange (..), Range, Ranges)
import Servant.Pipes ()

import Language.Marlowe.Runtime.Web.Client (Page (..))
import Language.Marlowe.Runtime.Web.Core.Object.Schema ()

runtimeClient :: Client ClientM RuntimeAPI
runtimeClient = ServantStreaming.client runtimeApi

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

getTransactionsStatus
  :: TxOutRef
  -> Maybe (Range "transactionId" TxId)
  -> ClientM (RuntimeStatus, Page "transactionId" TxHeader)
getTransactionsStatus contractId range = do
  let contractsClient :<|> _ = runtimeClient
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
  let contractsClient :<|> _ = runtimeClient
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
  let (_ :<|> _ :<|> contractApi :<|> _) :<|> _ = runtimeClient
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
  let contractsClient :<|> _ = runtimeClient
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
  let contractsClient :<|> _ = runtimeClient
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
