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

import Control.Monad (void)
import Control.Monad.Except (throwError)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Language.Marlowe.CLI.Command.Template (makeContract)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import qualified Language.Marlowe.CLI.Test.Operation.Aeson as Operation
import Language.Marlowe.CLI.Test.Wallet.Interpret
  (assetIdToToken, findWallet, findWalletByUniqueToken, getSingletonCurrency)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Asset(Asset)
  , AssetId
  , CurrencyNickname
  , Wallet(waAddress)
  , WalletNickname(WalletNickname)
  , adaToken
  , faucetNickname
  , parseTokenNameJSON
  , tokenNameToJSON
  )
import qualified Language.Marlowe.CLI.Test.Wallet.Types as Wallet
import Language.Marlowe.CLI.Types (CliError(CliError), SomeTimeout, toMarloweTimeout)
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import Ledger.Orphans ()
import Marlowe.Contracts (coveredCall, escrow, swap, trivial, zeroCouponBond)
import Plutus.V1.Ledger.Api (TokenName)

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

instance FromJSON PartyRef where
  parseJSON = \case
    A.Object (KeyMap.toList -> [("address", A.String walletNickname)]) ->
      pure . WalletRef . WalletNickname . T.unpack $ walletNickname
    A.Object (KeyMap.toList -> [("role_token", roleTokenJSON)]) -> do
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
    toJSON (InlineContract c)            = A.object [("inline", toJSON c)]
    toJSON (UseTemplate templateCommand) = A.object [("template", toJSON templateCommand)]

instance FromJSON Source where
    parseJSON json = case json of
      A.Object (KeyMap.toList -> [("inline", contractJson)]) -> do
        parsedContract <- parseJSON contractJson
        pure $ InlineContract parsedContract
      A.Object (KeyMap.toList -> [("template", templateCommandJson)]) -> do
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

    ccCurrency <- assetIdToToken utCurrency
    underlying <- assetIdToToken utUnderlying

    makeContract $ coveredCall
        issuer
        counterParty
        Nothing
        ccCurrency
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

