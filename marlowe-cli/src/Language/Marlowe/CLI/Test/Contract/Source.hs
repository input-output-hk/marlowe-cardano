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
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Contract.Source
  where

import Cardano.Api (CardanoMode, LocalNodeConnectInfo, Lovelace, PolicyId, ScriptDataSupportedInEra)
import qualified Cardano.Api as C
import Control.Lens (makeLenses)
import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.=))
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
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Base (Alternative((<|>)))
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Language.Marlowe.CLI.Command.Template (makeContract)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress)
import Language.Marlowe.CLI.Test.Contract.ContractNickname (ContractNickname(ContractNickname))
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import Language.Marlowe.CLI.Test.ExecutionMode
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Test.Wallet.Interpret
  (assetIdToToken, findWallet, findWalletByUniqueToken, getSingletonCurrency)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Asset(Asset)
  , AssetId(AssetId)
  , Currencies(Currencies)
  , CurrencyNickname
  , Wallet(waAddress)
  , WalletNickname(WalletNickname)
  , Wallets(Wallets)
  , adaToken
  , faucetNickname
  , parseTokenNameJSON
  , tokenNameToJSON
  )
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Types
  ( CliError(CliError)
  , MarlowePlutusVersion
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
  , PrintStats(PrintStats)
  , SomeTimeout
  , toMarloweTimeout
  )
import Language.Marlowe.Cardano.Thread
  ( AnyMarloweThread
  , MarloweThread(Closed, Created, InputsApplied)
  , anyMarloweThread
  , marloweThreadInitialTxIn
  , overAnyMarloweThread
  )
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import qualified Language.Marlowe.Runtime.Cardano.Api as Runtime.Api
import Language.Marlowe.Runtime.Core.Api (ContractId)
import qualified Language.Marlowe.Runtime.Core.Api as Runtime.Api
import Ledger.Orphans ()
import Marlowe.Contracts (coveredCall, escrow, swap, trivial, zeroCouponBond)
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, ProtocolVersion, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)

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
      utParty              :: Maybe PartyRef              -- ^ The party. Falls back to the faucet wallet pkh.
    , utDepositLovelace    :: Integer                     -- ^ Lovelace in the deposit.
    , utWithdrawalLovelace :: Integer                     -- ^ Lovelace in the withdrawal.
    , utTimeout            :: SomeTimeout                 -- ^ The timeout.
    }
    -- | Use for escrow contract.
  | UseEscrow
    {
      utPrice             :: Integer          -- ^ Price of the item for sale, in lovelace.
    , utSeller            :: PartyRef         -- ^ Defaults to a wallet with the "Buyer" nickname.
    , utBuyer             :: PartyRef         -- ^ Defaults to a wallet with the "Seller" ncikname.
    , utMediator          :: PartyRef         -- ^ The mediator.
    , utPaymentDeadline   :: SomeTimeout      -- ^ The deadline for the buyer to pay.
    , utComplaintDeadline :: SomeTimeout      -- ^ The deadline for the buyer to complain.
    , utDisputeDeadline   :: SomeTimeout      -- ^ The deadline for the seller to dispute a complaint.
    , utMediationDeadline :: SomeTimeout      -- ^ The deadline for the mediator to decide.
    }
    -- | Use for swap contract.
  | UseSwap
    {
      utAParty            :: PartyRef           -- ^ First party
    , utAAsset            :: Asset
    , utATimeout          :: SomeTimeout        -- ^ Timeout for first party's deposit.
    , utBParty            :: PartyRef           -- ^ Second party. Falls back to wallet with "B" nickname.
    , utBAsset            :: Asset
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
      utIssuer         :: PartyRef    -- ^ The issuer.
    , utCounterParty   :: PartyRef    -- ^ The counter-party.
    , utCurrency       :: AssetId     -- ^ The currency token.
    , utStrike         :: Integer     -- ^ The strike in currency.
    , utUnderlying     :: AssetId     -- ^ The underlying token.
    , utAmount         :: Integer     -- ^ The amount of underlying.
    , utIssueDate      :: SomeTimeout -- ^ The issue date.
    , utMaturityDate   :: SomeTimeout -- ^ The maturity date.
    , utSettlementDate :: SomeTimeout -- ^ The settlement date.
    }
    -- | Use for actus contracts.
  | UseActus
    {
      utParty          :: Maybe PartyRef   -- ^ The party. Fallsback to the faucet wallet.
    , utCounterParty   :: PartyRef         -- ^ The counter-party.
    , utActusTermsFile :: FilePath         -- ^ The Actus contract terms.
    }
    deriving stock (Eq, Generic, Show)

instance FromJSON UseTemplate where
  parseJSON = do
    A.genericParseJSON $ Operation.genericJSONOptions "ut"

instance ToJSON UseTemplate where
  toJSON = A.genericToJSON $ Operation.genericJSONOptions "ut"

data Source =
    InlineContract ParametrizedMarloweJSON
  | UseTemplate UseTemplate
    deriving stock (Eq, Generic, Show)


instance ToJSON Source where
    toJSON (InlineContract c)            = Aeson.object [("inline", toJSON c)]
    toJSON (UseTemplate templateCommand) = Aeson.object [("template", toJSON templateCommand)]

instance FromJSON Source where
    parseJSON json = case json of
      Aeson.Object (KeyMap.toList -> [("inline", contractJson)]) -> do
        parsedContract <- parseJSON contractJson
        pure $ InlineContract parsedContract
      Aeson.Object (KeyMap.toList -> [("template", templateCommandJson)]) -> do
        parsedTemplateCommand <- parseJSON templateCommandJson
        pure $ UseTemplate parsedTemplateCommand
      _ -> fail "Expected object with a single field of either `inline` or `template`"

buildParty
  :: Wallet.InterpretMonad env st m era
  => Maybe CurrencyNickname
  -> PartyRef
  -> m M.Party
buildParty mRoleCurrency = \case
  WalletRef nickname -> do
      wallet <- findWallet nickname
      uncurry M.Address <$> marloweAddressFromCardanoAddress (waAddress wallet)
  RoleRef token -> do
    -- Cosistency check
    currency <- case mRoleCurrency of
      Nothing -> fst <$> getSingletonCurrency
      Just cn -> pure cn
    void $ findWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token

useTemplate
  :: Wallet.InterpretMonad env st m era
  => Maybe CurrencyNickname
  -> UseTemplate
  -> m M.Contract
useTemplate currency =
  \case
  UseTrivial{..} -> do
    timeout' <- toMarloweTimeout utTimeout
    let
      partyRef = fromMaybe (WalletRef faucetNickname) utParty
    party <- buildParty currency partyRef
    makeContract $ trivial
      party
      utDepositLovelace
      utWithdrawalLovelace
      timeout'
  UseSwap{..} -> do
    aTimeout' <- toMarloweTimeout utATimeout
    bTimeout' <- toMarloweTimeout utBTimeout

    let
      Asset aAssetId aAmount = utAAsset
      Asset bAssetId bAmount = utBAsset

    aToken <- assetIdToToken aAssetId
    bToken <- assetIdToToken bAssetId

    aParty <- buildParty currency utAParty
    bParty <- buildParty currency utBParty

    makeContract $ swap
        aParty
        aToken
        (E.Constant aAmount)
        aTimeout'
        bParty
        bToken
        (E.Constant bAmount)
        bTimeout'
        E.Close
  UseEscrow{..} -> do
    paymentDeadline' <- toMarloweTimeout utPaymentDeadline
    complaintDeadline' <- toMarloweTimeout utComplaintDeadline
    disputeDeadline' <- toMarloweTimeout utDisputeDeadline
    mediationDeadline' <- toMarloweTimeout utMediationDeadline

    seller <- buildParty currency utSeller
    buyer <- buildParty currency utBuyer
    mediator <- buildParty currency utMediator

    makeContract $ escrow
      (E.Constant utPrice)
      seller
      buyer
      mediator
      paymentDeadline'
      complaintDeadline'
      disputeDeadline'
      mediationDeadline'
  UseCoveredCall{..} -> do
    issueDate <- toMarloweTimeout utIssueDate
    maturityDate <- toMarloweTimeout utMaturityDate
    settlementDate <- toMarloweTimeout utSettlementDate
    issuer <- buildParty currency utIssuer
    counterParty <- buildParty currency utCounterParty

    currency <- assetIdToToken utCurrency
    underlying <- assetIdToToken utUnderlying

    makeContract $ coveredCall
        issuer
        counterParty
        Nothing
        currency
        underlying
        (E.Constant utStrike)
        (E.Constant utAmount)
        issueDate
        maturityDate
        settlementDate
  UseZeroCouponBond{..} -> do
    lendingDeadline <- toMarloweTimeout utLendingDeadline
    paybackDeadline <- toMarloweTimeout utPaybackDeadline

    lender <- buildParty currency utLender
    borrower <- buildParty currency utBorrower

    makeContract $ zeroCouponBond
      lender
      borrower
      lendingDeadline
      paybackDeadline
      (E.Constant utPrincipal)
      (E.Constant utPrincipal `E.AddValue` E.Constant utInterest)
      adaToken
      E.Close
  template -> throwError $ CliError $ "Template not implemented: " <> show template

