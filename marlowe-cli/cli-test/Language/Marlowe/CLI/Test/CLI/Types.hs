{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

module Language.Marlowe.CLI.Test.CLI.Types where

import Cardano.Api (
  BabbageEraOnwards,
  LocalNodeConnectInfo,
 )
import Cardano.Api qualified as C
import Cardano.Api.Ledger qualified as Ledger
import Control.Category ((<<<))
import Control.Error.Util (hush)
import Control.Lens (Lens', makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON (..), ToJSON (..), (.:?), (.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List qualified as List
import Data.List.NonEmpty qualified as NE
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import GHC.Base (Alternative ((<|>)))
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname, Source (..))
import Language.Marlowe.CLI.Test.Contract qualified as Contract
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (
  ParametrizedMarloweJSON (..),
 )
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError)
import Language.Marlowe.CLI.Test.Operation.Aeson (
  ConstructorName (ConstructorName),
  NewPropName (NewPropName),
  OldPropName (OldPropName),
  PropName (PropName),
  genericJSONOptions,
  rewriteProp,
  rewritePropWith,
  rewriteToEmptyObject,
  rewriteToSingleConstructorJSON,
  rewriteToSingletonObject,
 )
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Test.Wallet.Types (
  Currencies,
  CurrencyNickname,
  WalletNickname,
  Wallets,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Types (
  MarloweScriptsRefs,
  MarloweTransaction (MarloweTransaction, mtInputs),
  PrintStats,
  SomeTimeout,
  TxBuildupContext,
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
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import PlutusLedgerApi.V1 (MajorProtocolVersion, TokenName)

type CLITxInfo lang era = (MarloweTransaction lang era, C.TxBody era)

type CLIMarloweThread lang era status =
  MarloweThread (CLITxInfo lang era) status

getCLIMarloweThreadTransaction
  :: CLIMarloweThread lang era status -> MarloweTransaction lang era
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
anyCLIMarloweThreadInputsApplied txInfo@(MarloweTransaction{..}, _) mTxIn =
  anyMarloweThreadInputsApplied txInfo mTxIn mtInputs

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
  deriving (Eq, Generic, Show)

instance FromJSON MarloweValidators where
  parseJSON json = do
    let inTx =
          parseJSON json >>= \case
            A.String "inTxCurrent" -> pure InTxCurrentValidators
            _ -> fail "Expected string `inTxCurrent`"
        fromRuntimeRegistry =
          parseJSON json >>= \case
            A.String "referenceRuntime" -> pure ReferenceRuntimeValidators
            A.String "referenceCurrent" ->
              pure $ ReferenceCurrentValidators Nothing Nothing
            _ -> fail "Expected string `referenceRuntime`"
        fromPublished =
          parseJSON json >>= \case
            A.Object (KeyMap.toList -> [("publishCurrent", objJson)]) -> do
              obj <- parseJSON objJson
              publisher <- obj .:? "publisher"
              permanently <- obj .:? "permanently"
              pure $ ReferenceCurrentValidators permanently publisher
            _ -> fail "Expected object with a single field `referenceCurrent`"
    inTx <|> fromRuntimeRegistry <|> fromPublished

instance ToJSON MarloweValidators where
  toJSON InTxCurrentValidators = A.String "inTxCurrent"
  toJSON ReferenceRuntimeValidators = A.String "referenceRuntime"
  toJSON (ReferenceCurrentValidators permanent publisher) =
    A.object
      [ "referenceCurrent"
          .= A.object ["permanent" .= permanent, "publisher" .= publisher]
      ]

data AnalysisStrategy = AnalysisStrategy
  { asMinimumUtxo :: Maybe Bool
  , asExecutionCost :: Maybe Bool
  , asTransactionSize :: Maybe Bool
  , asVerbose :: Maybe Bool
  }
  deriving (Eq, Generic, Show)

instance FromJSON AnalysisStrategy where
  parseJSON json = do
    let json' = rewriteToSingleConstructorJSON (ConstructorName "AnalysisStrategy") json
    A.genericParseJSON (genericJSONOptions "as") json'

instance ToJSON AnalysisStrategy where
  toJSON a = do
    let json = A.genericToJSON (genericJSONOptions "as") a
    case json of
      A.Object (KeyMap.toAscList -> [("content", objJson), ("tag", "AnalysisStrategy")]) -> objJson
      _ -> json

-- | On-chain test operations for the Marlowe contract and payout validators.
data CLIOperation
  = -- | We use "private" currency minting policy which
    -- | checks for a signature of a particular issuer.
    Initialize
      { coMinLovelace :: Maybe Ledger.Coin
      -- ^ Minimum lovelace to be sent to the contract.
      , coContractNickname :: Maybe ContractNickname
      -- ^ The name of the wallet's owner.
      , coRoleCurrency :: Maybe CurrencyNickname
      -- ^ If contract uses roles then currency is required.
      , coContractSource :: Contract.Source
      -- ^ The Marlowe contract to be created.
      , coInitialState :: Maybe ParametrizedMarloweJSON
      -- ^ The initial Marlowe state.
      , coSubmitter :: Maybe WalletNickname
      -- ^ A wallet which gonna submit the initial transaction.
      , coMarloweValidators :: Maybe MarloweValidators
      , coMerkleize :: Maybe Bool
      , coFullyAnalyze :: Maybe AnalysisStrategy
      }
  | Prepare
      { coContractNickname :: Maybe ContractNickname
      -- ^ The name of the contract.
      , coInputs :: [ParametrizedMarloweJSON]
      -- ^ Inputs to the contract.
      , coMinimumTime :: SomeTimeout
      , coMaximumTime :: SomeTimeout
      , coOverrideMarloweState :: Maybe M.State
      -- ^ Useful for testing failing scenarios where we want to override the output state.
      -- TODO: It should be parametrized value like contract or inputs.
      , coAnalyze :: Maybe AnalysisStrategy
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
      , coSubmitter :: Maybe WalletNickname
      -- ^ In the case of open roles we are not able to pick a wallet.
      -- We fallback to the initial contract submitter as a default.
      }
  | Withdraw
      { coContractNickname :: Maybe ContractNickname
      , coWalletNickname :: WalletNickname
      }
  deriving (Eq, Generic, Show)

initialize :: Contract.Source -> Maybe CurrencyNickname -> Either Ledger.Coin M.State -> CLIOperation
initialize source roleCurrency minLovelaceOrInitialState = do
  let coMinLovelace = case minLovelaceOrInitialState of
        Left lovelace -> Just lovelace
        Right _ -> Nothing
      coInitialState = ParametrizedMarloweJSON . toJSON <$> hush minLovelaceOrInitialState
  Initialize
    { coMinLovelace
    , coContractNickname = Nothing
    , coRoleCurrency = roleCurrency
    , coContractSource = source
    , coInitialState
    , coSubmitter = Nothing
    , coMarloweValidators = Nothing
    , coMerkleize = Nothing
    , coFullyAnalyze = Nothing
    }

initialize' :: M.Contract -> Maybe CurrencyNickname -> Either Ledger.Coin M.State -> CLIOperation
initialize' contract roleCurrency minLovelaceOrInitialState = do
  let contractSource = InlineContract $ ParametrizedMarloweJSON (toJSON contract)
  initialize contractSource roleCurrency minLovelaceOrInitialState

newtype ParametrizedInputs = ParametrizedInputs [ParametrizedMarloweJSON]

prepare :: ParametrizedInputs -> SomeTimeout -> SomeTimeout -> CLIOperation
prepare (ParametrizedInputs inputs) minTime maxTime =
  Prepare
    { coContractNickname = Nothing
    , coInputs = inputs
    , coMinimumTime = minTime
    , coMaximumTime = maxTime
    , coOverrideMarloweState = Nothing
    , coAnalyze = Nothing
    }

prepare' :: [M.Input] -> SomeTimeout -> SomeTimeout -> CLIOperation
prepare' =
  prepare
    <<< ParametrizedInputs
    <<< map (ParametrizedMarloweJSON <<< toJSON)

autoRun :: CLIOperation
autoRun =
  AutoRun
    { coContractNickname = Nothing
    , coInvalid = Nothing
    , coSubmitter = Nothing
    }

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
                    rewriteToContractNickname =
                      rewriteToSingletonObject
                        constructorName
                        (PropName "contractNickname")
                rewriteToContractNickname
                  <> rewriteToEmptyObject constructorName
              rewriteWithdraw = do
                let constructorName = ConstructorName "Withdraw"
                    rewriteToWalletNickname =
                      rewriteToSingletonObject
                        constructorName
                        (PropName "walletNickname")
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
  , _ciPlan :: NE.NonEmpty (MarloweTransaction lang era)
  -- ^ Whole Marlowe execution plan for the contract.
  , _ciThread :: Maybe (AnyCLIMarloweThread lang era)
  -- ^ Parts of the plan which are already on the chain.
  , _ciWithdrawalsCheckPoints :: Map TokenName C.TxId
  -- ^ TODO: Currently we track a point of the last withdrawal on the chain
  -- but marlowe thread has type constructor which actually tracks this now so
  -- we should use it instead and drop this check point.
  , _ciSubmitter :: WalletNickname
  }

makeLenses 'CLIContractInfo

newtype CLIContracts lang era = CLIContracts
  { _unCLIContracts :: Map ContractNickname (CLIContractInfo lang era)
  }

makeLenses 'CLIContracts

cliMarloweThreadContractId :: AnyCLIMarloweThread lang era -> ContractId
cliMarloweThreadContractId =
  Runtime.Api.ContractId
    . Runtime.Api.fromCardanoTxIn
    . overAnyMarloweThread marloweThreadInitialTxIn

cliContractsIds :: CLIContracts lang era -> Map ContractNickname ContractId
cliContractsIds (CLIContracts contracts) =
  Map.fromList $
    mapMaybe (\(k, v) -> (k,) . cliMarloweThreadContractId <$> _ciThread v) $
      Map.toList contracts

class HasInterpretState st lang era | st -> lang era where
  walletsL :: Lens' st (Wallets era)
  currenciesL :: Lens' st Currencies
  contractsL :: Lens' st (CLIContracts lang era)
  publishedScriptsL :: Lens' st (Maybe (MarloweScriptsRefs lang era))

class HasInterpretEnv env lang era | env -> lang era where
  connectionL :: Lens' env LocalNodeConnectInfo
  eraL :: Lens' env (BabbageEraOnwards era)
  printStatsL :: Lens' env PrintStats
  slotConfigL :: Lens' env SlotConfig
  costModelParamsL :: Lens' env [Integer]
  protocolVersionL :: Lens' env MajorProtocolVersion
  txBuildupContextL :: Lens' env (TxBuildupContext era)

type InterpretMonad env st m lang era =
  ( MonadState st m
  , HasInterpretState st lang era
  , MonadReader env m
  , HasInterpretEnv env lang era
  , Wallet.InterpretMonad env st m era
  , MonadError InterpreterError m
  , MonadIO m
  )
