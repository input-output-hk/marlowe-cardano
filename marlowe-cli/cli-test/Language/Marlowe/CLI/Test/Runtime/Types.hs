{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Runtime.Types (
  AnyRuntimeInterpreterMarloweThread,
  AnyRuntimeMonitorMarloweThread,
  ContractInfo (..),
  DoMerkleize (..),
  HasInterpretState,
  HasInterpretEnv,
  InterpretMonad,
  MarloweTags,
  RuntimeContractInfo (..),
  RuntimeError (..),
  RuntimeInterpreterMarloweThread,
  RuntimeMonitor (..),
  RuntimeMonitorInput (..),
  RuntimeMonitorMarloweThread,
  RuntimeMonitorState (..),
  RuntimeMonitorTxInfo,
  RuntimeOperation (..),
  RuntimeTxInfo (..),
  anyRuntimeInterpreterMarloweThreadInputsApplied,
  anyRuntimeMonitorMarloweThreadInputsApplied,
  ciContinuations,
  ciContract,
  ciContractId,
  ciMarloweThread,
  ciRoleCurrency,
  connectionT,
  currenciesL,
  currentMarloweData,
  defaultOperationTimeout,
  eraL,
  txBuildupContextL,
  knownContractsL,
  rcMarloweThread,
  runtimeClientConnectorT,
  runtimeMonitorInputT,
  runtimeMonitorStateT,
  slotConfigL,
  toMarloweTags,
  unMarloweTags,
  walletsL,
)
where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo, ScriptDataSupportedInEra)
import Cardano.Api qualified as C
import Contrib.Data.Time.Units.Aeson qualified as A
import Control.Concurrent.STM (TChan, TVar)
import Control.Error (lastMay)
import Control.Lens (Lens', Traversal', makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON)
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (ToJSON)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Time.Units
import Data.Traversable (for)
import Data.Word (Word64)
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.Contract qualified as Contract
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError)
import Language.Marlowe.CLI.Test.Log qualified as Log
import Language.Marlowe.CLI.Test.Operation.Aeson (
  ConstructorName (ConstructorName),
  NewPropName (NewPropName),
  OldPropName (OldPropName),
  rewriteProp,
  rewritePropWith,
  toConstructorBasedJSON,
 )
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies, CurrencyNickname, WalletNickname, Wallets)
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Types (SomeTimeout, TxBuildupContext)
import Language.Marlowe.Cardano.Thread (
  AnyMarloweThread,
  MarloweThread,
  anyMarloweThreadInputsApplied,
  marloweThreadTxInfos,
  overAnyMarloweThread,
 )
import Language.Marlowe.Core.V1.Merkle (Continuations)
import Language.Marlowe.Core.V1.Semantics qualified as M
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Protocol.Client qualified as Marlowe.Protocol
import Language.Marlowe.Runtime.ChainSync.Api qualified as ChainSync
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweMetadataTag (MarloweMetadataTag))
import Network.Protocol.Connection qualified as Network.Protocol
import Plutus.V1.Ledger.SlotConfig (SlotConfig)

-- | We use TxId in the thread to perform awaits for a particular marlowe transaction.
type RuntimeMonitorTxInfo = C.TxId

type RuntimeMonitorMarloweThread = MarloweThread RuntimeMonitorTxInfo

type AnyRuntimeMonitorMarloweThread = AnyMarloweThread RuntimeMonitorTxInfo

anyRuntimeMonitorMarloweThreadInputsApplied
  :: C.TxId
  -> Maybe C.TxIx
  -> [M.Input]
  -> AnyRuntimeMonitorMarloweThread
  -> Maybe AnyRuntimeMonitorMarloweThread
anyRuntimeMonitorMarloweThreadInputsApplied txId possibleTxIx = do
  let possibleTxIn = C.TxIn txId <$> possibleTxIx
  anyMarloweThreadInputsApplied txId possibleTxIn

-- | To avoid confusion we have a separate type for submitted transaction
-- | in the main interpreter thread. We use these to await for runtime
-- | confirmations.
data RuntimeTxInfo = RuntimeTxInfo
  { _rtiTxId :: !C.TxId
  , _rtiMaroweParams :: !(Maybe M.MarloweData)
  }
  deriving stock (Eq, Generic, Show)

type RuntimeInterpreterMarloweThread = MarloweThread RuntimeTxInfo

type AnyRuntimeInterpreterMarloweThread = AnyMarloweThread RuntimeTxInfo

currentMarloweData :: AnyRuntimeInterpreterMarloweThread -> Maybe M.MarloweData
currentMarloweData th = do
  RuntimeTxInfo _ md <- lastMay $ overAnyMarloweThread marloweThreadTxInfos th
  md

anyRuntimeInterpreterMarloweThreadInputsApplied
  :: C.TxId
  -> Maybe (C.TxIx, M.MarloweData)
  -> [M.Input]
  -> AnyRuntimeInterpreterMarloweThread
  -> Maybe AnyRuntimeInterpreterMarloweThread
anyRuntimeInterpreterMarloweThreadInputsApplied txId possibleOutput = do
  let possibleTxIx = fst <$> possibleOutput
      possibleTxIn = C.TxIn txId <$> possibleTxIx
      txInfo = RuntimeTxInfo txId (snd <$> possibleOutput)
  anyMarloweThreadInputsApplied txInfo possibleTxIn

data RuntimeError
  = RuntimeConnectionError
  | RuntimeExecutionFailure String
  | RuntimeContractNotFound ContractId
  | RuntimeRollbackError ContractNickname
  deriving stock (Eq, Generic, Show)
  deriving anyclass (A.ToJSON, A.FromJSON)

newtype RuntimeContractInfo = RuntimeContractInfo
  { _rcMarloweThread :: AnyRuntimeMonitorMarloweThread
  }

newtype RuntimeMonitorInput = RuntimeMonitorInput (TChan (ContractNickname, ContractId))

newtype RuntimeMonitorState = RuntimeMonitorState (TVar (Map ContractNickname RuntimeContractInfo))

newtype RuntimeMonitor = RuntimeMonitor {runMonitor :: IO RuntimeError}

defaultOperationTimeout :: Second
defaultOperationTimeout = 30

data DoMerkleize = ClientSide | RuntimeSide
  deriving stock (Eq, Generic, Show)

instance A.FromJSON DoMerkleize where
  parseJSON = A.genericParseJSON A.defaultOptions

-- parseConstructorBasedJSON' ""

instance A.ToJSON DoMerkleize where
  toJSON = toConstructorBasedJSON ""

data RuntimeOperation
  = RuntimeAwaitTxsConfirmed
      { roContractNickname :: Maybe ContractNickname
      , roTimeout :: Maybe A.Second
      }
  | RuntimeAwaitClosed
      { roContractNickname :: Maybe ContractNickname
      , roTimeout :: Maybe A.Second
      }
  | RuntimeCreateContract
      { roContractNickname :: Maybe ContractNickname
      , roSubmitter :: Maybe WalletNickname
      -- ^ A wallet which gonna submit the initial transaction.
      , roMinLovelace :: Word64
      , roRoleCurrency :: Maybe CurrencyNickname
      -- ^ If contract uses roles then currency is required.
      , roContractSource :: Contract.Source
      -- ^ The Marlowe contract to be created.
      , roAwaitConfirmed :: Maybe A.Second
      -- ^ How long to wait for the transaction to be confirmed in the Runtime. By default we don't wait.
      , roMerkleize :: Maybe DoMerkleize
      -- ^ Whether to merkleize the contract by using Marlowe Runtime store.
      -- By default we don't merkleize.
      , roTags :: Maybe MarloweTags
      }
  | RuntimeApplyInputs
      { roContractNickname :: Maybe ContractNickname
      , roInputs :: [ParametrizedMarloweJSON]
      -- ^ Inputs to the contract.
      , roSubmitter :: Maybe WalletNickname
      -- ^ A wallet which gonna submit the initial transaction.
      , roAwaitConfirmed :: Maybe A.Second
      -- ^ How long to wait for the transaction to be confirmed in the Runtime. By default we don't wait.
      , roInvalidBefore :: SomeTimeout
      , roInvalidHereafter :: SomeTimeout
      }
  | RuntimeWithdraw
      { roContractNickname :: Maybe ContractNickname
      , roWallets :: Maybe [WalletNickname]
      -- ^ Wallets with role tokens. By default we find all the role tokens and use them.
      , roAwaitConfirmed :: Maybe A.Second
      -- ^ How long to wait for the transactions to be confirmed in the Runtime. By default we don't wait.
      }
  deriving stock (Eq, Generic, Show)

-- | `MarloweTags` invariant is that metadata can be serialized to JSON using fromJSONEncodedMetadata.
-- We keep a witness of it as a part of the value.
newtype MarloweTags = MarloweTags {unMarloweTags :: Map MarloweMetadataTag (Maybe (ChainSync.Metadata, A.Value))}
  deriving stock (Eq, Generic, Show)

toMarloweTags :: Map MarloweMetadataTag (Maybe ChainSync.Metadata) -> Maybe MarloweTags
toMarloweTags m = do
  m' <- for m \case
    Nothing -> pure Nothing
    Just metadata -> do
      value' <- ChainSync.toJSONEncodedMetadata metadata
      pure $ Just (metadata, value')
  pure $ MarloweTags m'

-- We use `ChainSync.fromJSONEncodedMetadata` to turn json (A.Value) into the Chain.Metadata.
instance FromJSON MarloweTags where
  parseJSON json = do
    let parseTag tag A.Null = pure (MarloweMetadataTag tag, Nothing)
        parseTag tag value = case ChainSync.fromJSONEncodedMetadata value of
          Just metadata -> pure (MarloweMetadataTag tag, Just (metadata, value))
          Nothing -> fail $ "Failed to parse metadata for tag: " <> show tag
    A.Object (KeyMap.toList -> pairs) <- A.parseJSON json
    tags <- for pairs \(key, value) -> parseTag (Key.toText key) value
    pure $ MarloweTags $ Map.fromList tags

instance ToJSON MarloweTags where
  toJSON (MarloweTags tags) = A.object $ do
    (MarloweMetadataTag tag, possibleMetadata) <- Map.toList tags
    pure (Key.fromText tag, maybe A.Null snd possibleMetadata)

instance FromJSON RuntimeOperation where
  parseJSON = do
    let preprocess = do
          let rewriteCreate = do
                let constructorName = ConstructorName "RuntimeCreateContract"
                    rewriteTemplate =
                      rewritePropWith
                        constructorName
                        (OldPropName "template")
                        (NewPropName "contractSource")
                        (A.object . List.singleton . ("template",))
                    rewriteContract =
                      rewritePropWith
                        constructorName
                        (OldPropName "source")
                        (NewPropName "contractSource")
                        (A.object . List.singleton . ("inline",))
                    rewriteContractNickname =
                      rewriteProp
                        constructorName
                        (OldPropName "nickname")
                        (NewPropName "contractNickname")
                rewriteTemplate <> rewriteContract <> rewriteContractNickname
              rewriteWithdraw = do
                let constructorName = ConstructorName "RuntimeWithdraw"
                    rewriteWallet =
                      rewritePropWith
                        constructorName
                        (OldPropName "wallet")
                        (NewPropName "wallets")
                        (A.toJSON . List.singleton)
                rewriteWallet
          rewriteCreate <> rewriteWithdraw
    Operation.parseConstructorBasedJSON "ro" preprocess

instance ToJSON RuntimeOperation where
  toJSON = Operation.toConstructorBasedJSON "ro"

data ContractInfo = ContractInfo
  { _ciContractId :: !ContractId
  , _ciRoleCurrency :: !(Maybe CurrencyNickname)
  , _ciContract :: !M.Contract
  , _ciContinuations :: Maybe Continuations
  -- ^ If the contract uses roles then currency is required.
  , _ciMarloweThread :: !AnyRuntimeInterpreterMarloweThread
  }

makeLenses 'ContractInfo

class HasInterpretState st era | st -> era where
  knownContractsL :: Lens' st (Map ContractNickname ContractInfo)
  walletsL :: Lens' st (Wallets era)
  currenciesL :: Lens' st Currencies

class HasInterpretEnv env era | env -> era where
  runtimeMonitorStateT :: Traversal' env RuntimeMonitorState
  runtimeMonitorInputT :: Traversal' env RuntimeMonitorInput
  runtimeClientConnectorT :: Traversal' env (Network.Protocol.Connector Marlowe.Protocol.MarloweRuntimeClient IO)

  connectionT :: Traversal' env (LocalNodeConnectInfo CardanoMode)
  eraL :: Lens' env (ScriptDataSupportedInEra era)
  slotConfigL :: Lens' env SlotConfig

  txBuildupContextL :: Lens' env (TxBuildupContext era)

type InterpretMonad env st m era =
  ( MonadState st m
  , HasInterpretState st era
  , MonadReader env m
  , HasInterpretEnv env era
  , Wallet.InterpretMonad env st m era
  , MonadError InterpreterError m
  , MonadIO m
  , Log.InterpretMonad env st m era
  )

makeLenses 'RuntimeContractInfo
