{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Role.Client (
  WalletHeader (..),
  toWalletHeader,
  buildBurnTokenTxBody,
  buildBurnTokenTxBodyWithStatus,
  buildBurnTokenTx,
  buildBurnTokenTxWithStatus,
  submitBurnTokenTx,
  submitBurnTokenTxWithRuntimeStatus,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Functor (void)
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Version (Version)
import GHC.TypeLits (KnownSymbol, symbolVal)

import Language.Marlowe.Runtime.Web.API (RuntimeAPI, runtimeApi)
import Language.Marlowe.Runtime.Web.Adapter.CommaList (
  CommaList (CommaList),
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
  CardanoTx,
  CardanoTxBody,
 )
import Servant (HasResponseHeader, ResponseHeader (..), getResponse, lookupResponseHeader, type (:<|>) ((:<|>)))
import Servant.API (Headers)
import Servant.Client (Client)
import Servant.Client.Streaming (ClientM)
import qualified Servant.Client.Streaming as ServantStreaming
import Servant.Pipes ()

import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))
import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (
  ToDTO (toDTO),
 )
import Language.Marlowe.Runtime.Web.Core.Object.Schema ()
import Language.Marlowe.Runtime.Web.Role.API (
  BurnRoleTokensTxEnvelope,
 )
import Language.Marlowe.Runtime.Web.Role.TokenFilter (
  RoleTokenFilter,
 )

runtimeClient :: Client ClientM RuntimeAPI
runtimeClient = ServantStreaming.client runtimeApi

data WalletHeader = Wallet
  { changeAddress :: Address
  , usedAddresses :: Maybe (Set Address)
  , collaterals :: Maybe (Set TxOutRef)
  }
  deriving (Eq, Show)

toWalletHeader :: WalletAddresses -> WalletHeader
toWalletHeader WalletAddresses{..} =
  Wallet
    { changeAddress = toDTO changeAddress
    , usedAddresses = Just $ Set.map toDTO extraAddresses
    , collaterals = Just $ Set.map toDTO collateralUtxos
    }

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

buildBurnTokenTxBodyWithStatus
  :: WalletHeader
  -> RoleTokenFilter
  -> ClientM (RuntimeStatus, BurnRoleTokensTxEnvelope CardanoTxBody)
buildBurnTokenTxBodyWithStatus Wallet{..} roleFilter = do
  let _ :<|> _ :<|> _ :<|> roleClient :<|> _ = runtimeClient
  let (buildBurnTokenTxBody' :<|> _) :<|> _ = roleClient
  response <-
    buildBurnTokenTxBody'
      roleFilter
      changeAddress
      (setToCommaList <$> usedAddresses)
      (setToCommaList <$> collaterals)
  status <- extractStatus response
  pure (status, getResponse response)

buildBurnTokenTxBody
  :: WalletHeader
  -> RoleTokenFilter
  -> ClientM (BurnRoleTokensTxEnvelope CardanoTxBody)
buildBurnTokenTxBody = (fmap . fmap) snd . buildBurnTokenTxBodyWithStatus

buildBurnTokenTxWithStatus
  :: WalletHeader
  -> RoleTokenFilter
  -> ClientM (RuntimeStatus, BurnRoleTokensTxEnvelope CardanoTx)
buildBurnTokenTxWithStatus Wallet{..} roleFilter = do
  let _ :<|> _ :<|> _ :<|> roleClient :<|> _ = runtimeClient
  let (_ :<|> buildBurnTokenTx') :<|> _ = roleClient
  response <-
    buildBurnTokenTx'
      roleFilter
      changeAddress
      (setToCommaList <$> usedAddresses)
      (setToCommaList <$> collaterals)
  status <- extractStatus response
  pure (status, getResponse response)

buildBurnTokenTx
  :: WalletHeader
  -> RoleTokenFilter
  -> ClientM (BurnRoleTokensTxEnvelope CardanoTx)
buildBurnTokenTx = (fmap . fmap) snd . buildBurnTokenTxWithStatus

submitBurnTokenTxWithRuntimeStatus :: TxId -> TextEnvelope -> ClientM RuntimeStatus
submitBurnTokenTxWithRuntimeStatus txId textEnvelope = do
  let _ :<|> _ :<|> _ :<|> roleClient :<|> _ = runtimeClient
  let _ :<|> submitBurnTokenTx' = roleClient
  response <- submitBurnTokenTx' txId textEnvelope
  extractStatus response

submitBurnTokenTx :: TxId -> TextEnvelope -> ClientM ()
submitBurnTokenTx txId = void . submitBurnTokenTxWithRuntimeStatus txId

setToCommaList :: Set a -> CommaList a
setToCommaList = CommaList . Set.toList

reqHeaderValue :: forall name a. (KnownSymbol name) => ResponseHeader name a -> ClientM a
reqHeaderValue = \case
  Header a -> pure a
  UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
  MissingHeader -> liftIO $ fail $ "Required header missing " <> symbolVal (Proxy @name)
