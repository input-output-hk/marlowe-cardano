{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Type safe list of transactions representing on chain Marlowe execution.
--
-----------------------------------------------------------------------------

module Language.Marlowe.CLI.Test.CLI.Types
  where
  -- ( AnyCLIMarloweThread
  -- , ContractNickname(..)
  -- , ContractSource(..)
  -- , CLIOperation(..)
  -- , CLIContracts(..)
  -- , Currency(..)
  -- , CLIContractInfo(..)
  -- , MarloweReferenceScripts(..)
  -- , MarloweThread(..)
  -- , PartyRef(..)
  -- , TokenName(..)
  -- , UseTemplate(..)
  -- , WalletNickname(..)
  -- , anyCLIMarloweThread
  -- , getCLIMarloweThreadTransaction
  -- , getCLIMarloweThreadTxBody
  -- ) where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo, Lovelace, PolicyId, ScriptDataSupportedInEra)
import qualified Cardano.Api as C
import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import qualified Data.Fixed as F
import qualified Data.Fixed as Fixed
import Data.Foldable (fold)
import qualified Data.List.NonEmpty as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Language.Marlowe.CLI.Test.Contract (ContractNickname(ContractNickname))
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import Language.Marlowe.CLI.Test.ExecutionMode
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currencies(Currencies)
  , CurrencyNickname
  , WalletNickname(WalletNickname)
  , Wallets(Wallets)
  , parseTokenNameJSON
  , tokenNameToJSON
  )
import Language.Marlowe.CLI.Types
  ( CliError(CliError)
  , MarlowePlutusVersion
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
  , PrintStats(PrintStats)
  , SomeTimeout
  )
import Language.Marlowe.Cardano.Thread
  (AnyMarloweThread, MarloweThread(Closed, Created, InputsApplied), anyMarloweThread)
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Ledger.Orphans ()
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, ProtocolVersion, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)


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

data ContractSource =
    InlineContract ParametrizedMarloweJSON
  | UseTemplate UseTemplate
    deriving stock (Eq, Generic, Show)


instance ToJSON ContractSource where
    toJSON (InlineContract c)            = Aeson.object [("inline", toJSON c)]
    toJSON (UseTemplate templateCommand) = Aeson.object [("template", toJSON templateCommand)]

instance FromJSON ContractSource where
    parseJSON json = case json of
      Aeson.Object (KeyMap.toList -> [("inline", contractJson)]) -> do
        parsedContract <- parseJSON contractJson
        pure $ InlineContract parsedContract
      Aeson.Object (KeyMap.toList -> [("template", templateCommandJson)]) -> do
        parsedTemplateCommand <- parseJSON templateCommandJson
        pure $ UseTemplate parsedTemplateCommand
      _ -> fail "Expected object with a single field of either `inline` or `template`"

-- | On-chain test operations for the Marlowe contract and payout validators.
data CLIOperation =
    -- | We use "private" currency minting policy which
    -- | checks for a signature of a particular issuer.
    Initialize
    {
      coMinLovelace       :: Lovelace                -- ^ Minimum lovelace to be sent to the contract.
    , coContractNickname  :: ContractNickname        -- ^ The name of the wallet's owner.
    , coRoleCurrency      :: Maybe CurrencyNickname  -- ^ If contract uses roles then currency is required.
    , coContractSource    :: ContractSource          -- ^ The Marlowe contract to be created.
    , coSubmitter         :: Maybe WalletNickname    -- ^ A wallet which gonna submit the initial transaction.
    }
  -- | RuntimeInitialize
  --   { coMinLovelace       :: Lovelace                -- ^ Minimum lovelace to be sent to the contract.
  --   , coContractNickname  :: ContractNickname        -- ^ The name of the wallet's owner.
  --   -- Runtime initialization differs from the usual one by the fact that
  --   -- we have possibly more flexible role tokens configuration.
  --   -- , coRoleCurrency      :: Maybe CurrencyNickname  -- ^ If contract uses roles then currency is required.
  --   -- Additionally we have a possibility to specify metadata.
  --   -- , coMetadata          :: Maybe Aeson.Object
  --   , coContractSource    :: ContractSource          -- ^ The Marlowe contract to be created.
  --   , coSubmitter         :: Maybe WalletNickname    -- ^ A wallet which gonna submit the initial transaction.
  --   }
  | Prepare
    {
      coContractNickname     :: ContractNickname  -- ^ The name of the contract.
    , coInputs               :: [ParametrizedMarloweJSON]         -- ^ Inputs to the contract.
    , coMinimumTime          :: SomeTimeout
    , coMaximumTime          :: SomeTimeout
    , coOverrideMarloweState :: Maybe M.State
    }
  | Publish
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
  deriving anyclass (FromJSON, ToJSON)



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


-- FIXME: We don't parse currency symbol yet.
instance FromJSON PartyRef where
  parseJSON = \case
    Aeson.Object (KeyMap.toList -> [("address", A.String walletNickname)]) ->
      pure . WalletRef . WalletNickname . T.unpack $ walletNickname
    Aeson.Object (KeyMap.toList -> [("role_token", roleTokenJSON)]) -> do
      roleToken <- parseTokenNameJSON roleTokenJSON
      pure $ RoleRef roleToken
    _ -> fail "Expecting a Party object."

instance ToJSON PartyRef where
    toJSON (WalletRef (WalletNickname walletNickname)) = A.object
        [ "address" .= A.String (T.pack walletNickname) ]
    toJSON (RoleRef tokenName) = A.object
        [ "role_token" .= tokenNameToJSON tokenName ]

data UseTemplate =
    UseTrivial
    {
      -- utBystander          :: Maybe WalletNickname        -- ^ The party providing the min-ADA. Falls back to the faucet wallet.
      utParty              :: Maybe PartyRef              -- ^ The party. Falls back to the faucet wallet pkh.
    , utDepositLovelace    :: Integer                     -- ^ Lovelace in the deposit.
    , utWithdrawalLovelace :: Integer                     -- ^ Lovelace in the withdrawal.
    , utTimeout            :: SomeTimeout                  -- ^ The timeout.
    }
    -- | Use for escrow contract.
  | UseEscrow
    {
      utPrice             :: Integer          -- ^ Price of the item for sale, in lovelace.
    , utSeller            :: Maybe PartyRef   -- ^ Defaults to a wallet with the "Buyer" nickname.
    , utBuyer             :: Maybe PartyRef   -- ^ Defaults to a wallet with the "Seller" ncikname.
    , utMediator          :: Maybe PartyRef   -- ^ The mediator.
    , utPaymentDeadline   :: SomeTimeout      -- ^ The deadline for the buyer to pay.
    , utComplaintDeadline :: SomeTimeout      -- ^ The deadline for the buyer to complain.
    , utDisputeDeadline   :: SomeTimeout      -- ^ The deadline for the seller to dispute a complaint.
    , utMediationDeadline :: SomeTimeout      -- ^ The deadline for the mediator to decide.
    }
    -- | Use for swap contract.
  | UseSwap
    {
      utAParty            :: Maybe PartyRef     -- ^ First party. Falls back to wallet with "A" nickname.
    , utACurrencyNickname :: CurrencyNickname
    , utATokenName        :: TokenName
    , utAAmount           :: Integer            -- ^ Amount of first party's token.
    , utATimeout          :: SomeTimeout        -- ^ Timeout for first party's deposit.
    , utBParty            :: Maybe PartyRef     -- ^ Second party. Falls back to wallet with "B" nickname.
    , utBCurrencyNickname :: CurrencyNickname
    , utBTokenName        :: TokenName
    , utBAmount           :: Integer            -- ^ Amount of second party's token.
    , utBTimeout          :: SomeTimeout        -- ^ Timeout for second party's deposit.
    }
    -- | Use for zero-coupon bond.
  | UseZeroCouponBond
    {
      utLender          :: PartyRef    -- ^ The lender.
    , utBorrower        :: PartyRef    -- ^ The borrower.
    , utPrincipal       :: Integer    -- ^ The principal.
    , utInterest        :: Integer    -- ^ The interest.
    , utLendingDeadline :: SomeTimeout -- ^ The lending deadline.
    , utPaybackDeadline :: SomeTimeout -- ^ The payback deadline.
    }
    -- | Use for covered call.
  | UseCoveredCall
    {
      utIssuer         :: PartyRef  -- ^ The issuer.
    , utCounterparty   :: PartyRef  -- ^ The counter-party.
    , utCurrency       :: E.Token    -- ^ The currency token.
    , utUnderlying     :: E.Token    -- ^ The underlying token.
    , utStrike         :: Integer    -- ^ The strike in currency.
    , utAmount         :: Integer    -- ^ The amount of underlying.
    , utIssueDate      :: SomeTimeout -- ^ The issue date.
    , utMaturityDate   :: SomeTimeout -- ^ The maturity date.
    , utSettlementDate :: SomeTimeout -- ^ The settlement date.
    }
    -- | Use for actus contracts.
  | UseActus
    {
      utParty          :: Maybe PartyRef   -- ^ The party. Fallsback to the faucet wallet.
    , utCounterparty   :: PartyRef         -- ^ The counter-party.
    , utActusTermsFile :: FilePath         -- ^ The Actus contract terms.
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

data CLIContractInfo lang era = CLIContractInfo
  {
    ciContract                :: M.Contract
  , ciCurrency                :: Maybe CurrencyNickname
  , ciPlan                    :: List.NonEmpty (MarloweTransaction lang era)
  , ciThread                  :: Maybe (AnyCLIMarloweThread lang era)
  , ciWithdrawalsCheckPoints  :: Map TokenName C.TxId                           -- ^ Track of last withrawal on chain point.
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

data InterpretState lang era = InterpretState
  { _isWallets :: Wallets era
  , _isCurrencies :: Currencies
  , _isContracts :: CLIContracts lang era
  , _isReferenceScripts :: Maybe (MarloweScriptsRefs MarlowePlutusVersion era)
  , _isKnownContracts :: Map ContractNickname ContractId
  }

data InterpretEnv lang era = InterpretEnv
  { _ieConnection :: LocalNodeConnectInfo CardanoMode
  , _ieEra :: ScriptDataSupportedInEra era
  , _iePrintStats :: PrintStats
  , _ieExecutionMode :: ExecutionMode
  , _ieSlotConfig :: SlotConfig
  , _ieCostModelParams :: CostModelParams
  , _ieProtocolVersion :: ProtocolVersion
  }

type InterpretMonad m lang era =
  ( MonadState (InterpretState lang era) m
  , MonadReader (InterpretEnv lang era) m
  , MonadError CliError m
  , MonadIO m
  )

makeLenses 'InterpretState
makeLenses 'InterpretEnv

