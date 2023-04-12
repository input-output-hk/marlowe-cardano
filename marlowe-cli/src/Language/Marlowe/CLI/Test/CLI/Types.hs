{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.CLI.Types
  where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo, Lovelace, PolicyId, ScriptDataSupportedInEra)
import qualified Cardano.Api as C
import Control.Lens (Lens')
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.List.NonEmpty as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import GHC.Base (Alternative((<|>)))
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Test.Contract (ContractNickname)
import qualified Language.Marlowe.CLI.Test.Contract as Contract
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import Language.Marlowe.CLI.Test.ExecutionMode
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Test.Wallet.Types (Currencies, CurrencyNickname, WalletNickname, Wallets)
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Types
  ( CliError
  , MarlowePlutusVersion
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
  , PrintStats
  , SomeTimeout
  )
import Language.Marlowe.Cardano.Thread
  ( AnyMarloweThread
  , MarloweThread(Closed, Created, InputsApplied)
  , anyMarloweThread
  , marloweThreadInitialTxIn
  , overAnyMarloweThread
  )
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Api
import Language.Marlowe.Runtime.Core.Api (ContractId)
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Api
import Ledger.Orphans ()
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, ProtocolVersion, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)


type CLITxInfo lang era = (MarloweTransaction lang era, C.TxBody era)

type CLIMarloweThread lang era status = MarloweThread (CLITxInfo lang era) lang era status

getCLIMarloweThreadTransaction :: CLIMarloweThread lang era status -> MarloweTransaction lang era
getCLIMarloweThreadTransaction (Created (mt, _) _)           = mt
getCLIMarloweThreadTransaction (InputsApplied (mt, _) _ _ _) = mt
getCLIMarloweThreadTransaction (Closed (mt, _) _ _)          = mt

getCLIMarloweThreadTxBody :: CLIMarloweThread lang era status -> C.TxBody era
getCLIMarloweThreadTxBody (Created (_, txBody) _)           = txBody
getCLIMarloweThreadTxBody (InputsApplied (_, txBody) _ _ _) = txBody
getCLIMarloweThreadTxBody (Closed (_, txBody) _ _)          = txBody

type AnyCLIMarloweThread lang era = AnyMarloweThread (CLITxInfo lang era) lang era

anyCLIMarloweThread :: CLITxInfo lang era
                    -> Maybe C.TxIn
                    -> AnyCLIMarloweThread lang era
                    -> Maybe (AnyCLIMarloweThread lang era)
anyCLIMarloweThread txInfo@(MarloweTransaction{..}, _) mTxIn = anyMarloweThread txInfo mTxIn mtInputs

data MarloweValidators
  = InTxCurrentValidators                           -- ^ Embed Marlowe validator in the applying transaction.
  | ReferenceCurrentValidators                      -- ^ Use already published validator or publish a new one.
    { umPublishPermanently :: Maybe Bool            --
    , umPublisher          :: Maybe WalletNickname  -- ^ Use this wallet if publishing is required.
    }
  | ReferenceRuntimeValidators                      -- ^ Pick a version of the validator from the runtime registry.
  deriving stock (Eq, Generic, Show)

instance FromJSON MarloweValidators where
  parseJSON json = do
    let
      inTx = parseJSON json >>= \case
        Aeson.String "inTxCurrent" -> pure InTxCurrentValidators
        _                          -> fail "Expected string `inTxCurrent`"
      fromRuntimeRegistry = parseJSON json >>= \case
        Aeson.String "referenceRuntime" -> pure ReferenceRuntimeValidators
        Aeson.String "referenceCurrent" -> pure $ ReferenceCurrentValidators Nothing Nothing
        _ -> fail "Expected string `referenceRuntime`"
      fromPublished = parseJSON json >>= \case
        Aeson.Object (KeyMap.toList -> [("referenceCurrent", objJson)]) -> do
          obj <- parseJSON objJson
          publisher <- obj .:? "publisher"
          permanent <- obj .:? "permanent"
          pure $ ReferenceCurrentValidators permanent publisher
        _ -> fail "Expected object with a single field `referenceCurrent`"
    inTx <|> fromRuntimeRegistry <|> fromPublished

instance ToJSON MarloweValidators where
  toJSON InTxCurrentValidators = Aeson.String "inTxCurrent"
  toJSON ReferenceRuntimeValidators = Aeson.String "referenceRuntime"
  toJSON (ReferenceCurrentValidators permanent publisher) =
    Aeson.object
      [ "referenceCurrent" .= Aeson.object
        [ "permanent" .= permanent
        , "publisher" .= publisher
        ]
      ]

-- | On-chain test operations for the Marlowe contract and payout validators.
data CLIOperation =
    -- | We use "private" currency minting policy which
    -- | checks for a signature of a particular issuer.
    Initialize
    {
      coMinLovelace         :: Lovelace                -- ^ Minimum lovelace to be sent to the contract.
    , coContractNickname    :: ContractNickname        -- ^ The name of the wallet's owner.
    , coRoleCurrency        :: Maybe CurrencyNickname  -- ^ If contract uses roles then currency is required.
    , coContractSource      :: Contract.Source         -- ^ The Marlowe contract to be created.
    , coSubmitter           :: Maybe WalletNickname    -- ^ A wallet which gonna submit the initial transaction.
    , coMarloweValidators   :: MarloweValidators
    }
  | Prepare
    {
      coContractNickname     :: ContractNickname  -- ^ The name of the contract.
    , coInputs               :: [ParametrizedMarloweJSON]         -- ^ Inputs to the contract.
    , coMinimumTime          :: SomeTimeout
    , coMaximumTime          :: SomeTimeout
    , coOverrideMarloweState :: Maybe M.State -- ^ Useful for testing scenarios with non standard initial state.
    }
  | Publish                                          -- ^ Publishing can be a part of `Initialize` operation but we can also test it separately.
    { coPublisher          :: Maybe WalletNickname   -- ^ Wallet used to cover fees. Falls back to faucet wallet.
    , coPublishPermanently :: Maybe Bool             -- ^ Whether to publish script permanently.
    }
  | AutoRun
    {
      coContractNickname :: ContractNickname
    , coInvalid          :: Maybe Bool
    }
  | Withdraw
   {
     coContractNickname :: ContractNickname
   , coWalletNickname   :: WalletNickname
   }
  deriving stock (Eq, Generic, Show)

instance FromJSON CLIOperation where
  parseJSON = do
    A.genericParseJSON $ Operation.genericJSONOptions "co"

instance ToJSON CLIOperation where
  toJSON = do
    A.genericToJSON $ Operation.genericJSONOptions "co"

-- | We encode `PartyRef` as `Party` so we can use role based contracts
-- | without any change in the JSON structure.
-- | In the case of the `Address` you should use standard encoding but
-- | reference a wallet instead of providing hash value:
-- | ```
-- |  "address": "Wallet-1"
-- | ```
data PartyRef =
    WalletRef WalletNickname
  | RoleRef TokenName
  deriving stock (Eq, Generic, Show)

data CLIContractInfo lang era = CLIContractInfo
  {
    ciContract                :: M.Contract
  , ciCurrency                :: Maybe CurrencyNickname
  , ciPlan                    :: List.NonEmpty (MarloweTransaction lang era)
  , ciThread                  :: Maybe (AnyCLIMarloweThread lang era)
  , ciWithdrawalsCheckPoints  :: Map TokenName C.TxId -- ^ Track a point of the last withdrawal on the chain.
  , ciSubmitter               :: WalletNickname
  }

data Currency = Currency
  {
    ccCurrencySymbol :: CurrencySymbol
  , ccIssuer         :: WalletNickname
  , ccPolicyId       :: PolicyId
  }

data MarloweReferenceScripts = MarloweReferenceScripts
  { mrsMarloweValidator :: C.TxIn
  , mrsPayoutValidator  :: C.TxIn
  }

newtype CLIContracts lang era = CLIContracts (Map ContractNickname (CLIContractInfo lang era))

cliMarloweThreadContractId :: AnyCLIMarloweThread lang era -> ContractId
cliMarloweThreadContractId =
  Runtime.Api.ContractId . Runtime.Api.fromCardanoTxIn . overAnyMarloweThread marloweThreadInitialTxIn

cliContractsIds :: CLIContracts lang era -> Map ContractNickname ContractId
cliContractsIds (CLIContracts contracts) =
  Map.fromList $ mapMaybe (\(k, v) -> (k,) . cliMarloweThreadContractId  <$> ciThread v) $ Map.toList contracts

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
  , MonadError CliError m
  , MonadIO m
  )

