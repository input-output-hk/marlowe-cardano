{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.Contract.Next.Client (
  getContractNext,
) where

import Control.Monad.IO.Class (liftIO)
import Data.Proxy (Proxy (..))
import Data.Time (UTCTime)
import Data.Version (Version)
import GHC.TypeLits (KnownSymbol, symbolVal)
import Language.Marlowe.Core.V1.Next (Next)

import Language.Marlowe.Runtime.Web.API (RuntimeAPI, runtimeApi)
import Language.Marlowe.Runtime.Web.Core.Party (Party)

import Language.Marlowe.Runtime.Web.Core.NetworkId (NetworkId)
import Language.Marlowe.Runtime.Web.Core.Tip (ChainTip)
import Language.Marlowe.Runtime.Web.Core.Tx (
  TxOutRef,
 )
import Language.Marlowe.Runtime.Web.Status (
  RuntimeStatus (RuntimeStatus),
 )
import Servant (HasResponseHeader, ResponseHeader (..), getResponse, lookupResponseHeader, type (:<|>) ((:<|>)))
import Servant.API (Headers)
import Servant.Client (Client)
import Servant.Client.Streaming (ClientM)
import qualified Servant.Client.Streaming as ServantStreaming
import Servant.Pipes ()

import Language.Marlowe.Runtime.Web.Core.Object.Schema ()

runtimeClient :: Client ClientM RuntimeAPI
runtimeClient = ServantStreaming.client runtimeApi

getContractNextStatus :: TxOutRef -> UTCTime -> UTCTime -> [Party] -> ClientM (RuntimeStatus, Next)
getContractNextStatus contractId validityStart validityEnd parties = do
  let contractsClient :<|> _ = runtimeClient
  let _ :<|> _ :<|> contractApi :<|> _ = contractsClient
  let _ :<|> _ :<|> next' :<|> _ = contractApi contractId
  response <- next' validityStart validityEnd parties
  status <- extractStatus response
  pure (status, getResponse response)

getContractNext :: TxOutRef -> UTCTime -> UTCTime -> [Party] -> ClientM Next
getContractNext = (fmap . fmap . fmap . fmap) snd . getContractNextStatus

reqHeaderValue :: forall name a. (KnownSymbol name) => ResponseHeader name a -> ClientM a
reqHeaderValue = \case
  Header a -> pure a
  UndecodableHeader _ -> liftIO $ fail $ "Unable to decode header " <> symbolVal (Proxy @name)
  MissingHeader -> liftIO $ fail $ "Required header missing " <> symbolVal (Proxy @name)

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
