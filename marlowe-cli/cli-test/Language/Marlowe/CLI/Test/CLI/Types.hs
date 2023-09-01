{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.CLI.Types where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo, Lovelace, ScriptDataSupportedInEra)
import Cardano.Api qualified as C
import Contrib.Data.List qualified as List
import Control.Lens (Lens', makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List.NonEmpty qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Base (Alternative ((<|>)))
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import Language.Marlowe.CLI.Test.Contract qualified as Contract
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import Language.Marlowe.CLI.Test.ExecutionMode
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError)
import Language.Marlowe.CLI.Test.Operation.Aeson (
  ConstructorName (ConstructorName),
  NewPropName (NewPropName),
  OldPropName (OldPropName),
  PropName (PropName),
  rewriteProp,
  rewritePropWith,
  rewriteToEmptyObject,
  rewriteToSingletonObject,
 )
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies, CurrencyNickname, WalletNickname, Wallets)
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Types (
  MarlowePlutusVersion,
  MarloweScriptsRefs,
  MarloweTransaction (MarloweTransaction, mtInputs),
  PrintStats,
  SomeTimeout,
 )
import Language.Marlowe.Cardano.Thread (
  AnyMarloweThread,
  MarloweThread (Closed, Created, InputsApplied, Redemption),
  anyMarloweThreadInputsApplied,
  marloweThreadInitialTxIn,
  overAnyMarloweThread,
 )
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Runtime.Cardano.Api qualified as Runtime.Api
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Language.Marlowe.Runtime.Core.Api qualified as Runtime.Api
import Plutus.V1.Ledger.Api (CostModelParams, ProtocolVersion, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)

type CLITxInfo lang era = (MarloweTransaction lang era, C.TxBody era)

type CLIMarloweThread lang era status = MarloweThread (CLITxInfo lang era) status

getCLIMarloweThreadTransaction :: CLIMarloweThread lang era status -> MarloweTransaction lang era
getCLIMarloweThreadTransaction (Created (mt, _) _) = mt
getCLIMarloweThreadTransaction (InputsApplied (mt, _) _ _ _) = mt
getCLIMarloweThreadTransaction (Redemption (mt, _) _ _) = mt
getCLIMarloweThreadTransaction (Closed (mt, _) _ _) = mt

getCLIMarloweThreadTxBody :: CLIMarloweThread lang era status -> C.TxBody era
getCLIMarloweThreadTxBody (Created (_, txBody) _) = txBody
getCLIMarloweThreadTxBody (InputsApplied (_, txBody) _ _ _) = txBody
getCLIMarloweThreadTxBody (Redemption (_, txBody) _ _) = txBody
getCLIMarloweThreadTxBody (Closed (_, txBody) _ _) = txBody

type AnyCLIMarloweThread lang era = AnyMarloweThread (CLITxInfo lang era)

anyCLIMarloweThreadInputsApplied
  :: CLITxInfo lang era
  -> Maybe C.TxIn
  -> AnyCLIMarloweThread lang era
  -> Maybe (AnyCLIMarloweThread lang era)
anyCLIMarloweThreadInputsApplied txInfo@(MarloweTransaction{..}, _) mTxIn = anyMarloweThreadInputsApplied txInfo mTxIn mtInputs

data MarloweValidators
  = -- | Embed Marlowe validator in the applying transaction.
    InTxCurrentValidators
  | -- | Use already published validator or publish a new one.
    ReferenceCurrentValidators
      { umPublishPermanently :: Maybe Bool --
      , umPublisher :: Maybe WalletNickname
      -- ^ Use this wallet if publishing is required.
      }
  | -- | Pick a version of the validator from the runtime registry.
    ReferenceRuntimeValidators
  deriving stock (Eq, Generic, Show)

instance FromJSON MarloweValidators where
  parseJSON json = do
    let inTx =
          parseJSON json >>= \case
            Aeson.String "inTxCurrent" -> pure InTxCurrentValidators
            _ -> fail "Expected string `inTxCurrent`"
        fromRuntimeRegistry =
          parseJSON json >>= \case
            Aeson.String "referenceRuntime" -> pure ReferenceRuntimeValidators
            Aeson.String "referenceCurrent" -> pure $ ReferenceCurrentValidators Nothing Nothing
            _ -> fail "Expected string `referenceRuntime`"
        fromPublished =
          parseJSON json >>= \case
            Aeson.Object (KeyMap.toList -> [("publishCurrent", objJson)]) -> do
              obj <- parseJSON objJson
              publisher <- obj .:? "publisher"
              permanently <- obj .:? "permanently"
              pure $ ReferenceCurrentValidators permanently publisher
            _ -> fail "Expected object with a single field `referenceCurrent`"
    inTx <|> fromRuntimeRegistry <|> fromPublished

instance ToJSON MarloweValidators where
  toJSON InTxCurrentValidators = Aeson.String "inTxCurrent"
  toJSON ReferenceRuntimeValidators = Aeson.String "referenceRuntime"
  toJSON (ReferenceCurrentValidators permanent publisher) =
    Aeson.object
      [ "referenceCurrent"
          .= Aeson.object
            [ "permanent" .= permanent
            , "publisher" .= publisher
            ]
      ]

-- | On-chain test operations for the Marlowe contract and payout validators.
data CLIOperation
  = -- | We use "private" currency minting policy which
    -- | checks for a signature of a particular issuer.
    Initialize
      { coMinLovelace :: Lovelace
      -- ^ Minimum lovelace to be sent to the contract.
      , coContractNickname :: Maybe ContractNickname
      -- ^ The name of the wallet's owner.
      , coRoleCurrency :: Maybe CurrencyNickname
      -- ^ If contract uses roles then currency is required.
      , coContractSource :: Contract.Source
      -- ^ The Marlowe contract to be created.
      , coSubmitter :: Maybe WalletNickname
      -- ^ A wallet which gonna submit the initial transaction.
      , coMarloweValidators :: Maybe MarloweValidators
      , coMerkleize :: Maybe Bool
      }
  | Prepare
      { coContractNickname :: Maybe ContractNickname
      -- ^ The name of the contract.
      , coInputs :: [ParametrizedMarloweJSON]
      -- ^ Inputs to the contract.
      , coMinimumTime :: SomeTimeout
      , coMaximumTime :: SomeTimeout
      , coOverrideMarloweState :: Maybe M.State
      -- ^ Useful for testing scenarios with non standard initial state.
      }
  | -- | Publishing can be a part of `Initialize` operation but we can also test it separately.
    Publish
      { coPublisher :: Maybe WalletNickname
      -- ^ Wallet used to cover fees. Falls back to faucet wallet.
      , coPublishPermanently :: Maybe Bool
      -- ^ Whether to publish script permanently.
      }
  | AutoRun
      { coContractNickname :: Maybe ContractNickname
      , coInvalid :: Maybe Bool
      }
  | Withdraw
      { coContractNickname :: Maybe ContractNickname
      , coWalletNickname :: WalletNickname
      }
  deriving stock (Eq, Generic, Show)

instance FromJSON CLIOperation where
  parseJSON = do
    let preprocess = do
          let rewriteInitialize = do
                let constructorName = ConstructorName "Initialize"
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

              rewriteAutoRun = do
                let constructorName = ConstructorName "AutoRun"
                    rewriteToContractNickname = rewriteToSingletonObject constructorName (PropName "contractNickname")
                rewriteToContractNickname <> rewriteToEmptyObject constructorName
              rewriteWithdraw = do
                let constructorName = ConstructorName "Withdraw"
                    rewriteToWalletNickname = rewriteToSingletonObject constructorName (PropName "walletNickname")
                    rewriteWallet =
                      rewriteProp
                        constructorName
                        (OldPropName "wallet")
                        (NewPropName "walletNickname")
                rewriteToWalletNickname <> rewriteWallet
          rewriteAutoRun <> rewriteWithdraw <> rewriteInitialize
    Operation.parseConstructorBasedJSON "co" preprocess

instance ToJSON CLIOperation where
  toJSON = Operation.toConstructorBasedJSON "co"

data CLIContractInfo lang era = CLIContractInfo
  { _ciContract :: M.Contract
  , _ciCurrency :: Maybe CurrencyNickname
  , _ciPlan :: List.NonEmpty (MarloweTransaction lang era)
  , _ciThread :: Maybe (AnyCLIMarloweThread lang era)
  , _ciWithdrawalsCheckPoints :: Map TokenName C.TxId
  -- ^ TODO: Currently we track a point of the last withdrawal on the chain.
  -- We should use new marlowe thread data type support for withdrawals tracking instead.
  , _ciSubmitter :: WalletNickname
  }
makeLenses 'CLIContractInfo

data MarloweReferenceScripts = MarloweReferenceScripts
  { mrsMarloweValidator :: C.TxIn
  , mrsPayoutValidator :: C.TxIn
  }

newtype CLIContracts lang era = CLIContracts {_unCLIContracts :: Map ContractNickname (CLIContractInfo lang era)}

makeLenses 'CLIContracts

cliMarloweThreadContractId :: AnyCLIMarloweThread lang era -> ContractId
cliMarloweThreadContractId =
  Runtime.Api.ContractId . Runtime.Api.fromCardanoTxIn . overAnyMarloweThread marloweThreadInitialTxIn

cliContractsIds :: CLIContracts lang era -> Map ContractNickname ContractId
cliContractsIds (CLIContracts contracts) =
  Map.fromList $ mapMaybe (\(k, v) -> (k,) . cliMarloweThreadContractId <$> _ciThread v) $ Map.toList contracts

class HasInterpretState st lang era | st -> lang era where
  walletsL :: Lens' st (Wallets era)
  currenciesL :: Lens' st Currencies
  contractsL :: Lens' st (CLIContracts lang era)
  publishedScriptsL :: Lens' st (Maybe (MarloweScriptsRefs MarlowePlutusVersion era))

class HasInterpretEnv env lang era | env -> lang era where
  connectionL :: Lens' env (LocalNodeConnectInfo CardanoMode)
  eraL :: Lens' env (ScriptDataSupportedInEra era)
  printStatsL :: Lens' env PrintStats
  executionModeL :: Lens' env ExecutionMode
  slotConfigL :: Lens' env SlotConfig
  costModelParamsL :: Lens' env CostModelParams
  protocolVersionL :: Lens' env ProtocolVersion

type InterpretMonad env st m lang era =
  ( MonadState st m
  , HasInterpretState st lang era
  , MonadReader env m
  , HasInterpretEnv env lang era
  , Wallet.InterpretMonad env st m era
  , MonadError InterpreterError m
  , MonadIO m
  )
