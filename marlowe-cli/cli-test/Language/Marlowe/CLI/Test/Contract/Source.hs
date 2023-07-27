{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.CLI.Test.Contract.Source where

import Control.Monad (void)
import Control.Monad.Except (MonadError, throwError)
import Data.Aeson (FromJSON (..), ToJSON (..), (.=))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Marlowe.CLI.IO (liftCliMaybe)
import Language.Marlowe.CLI.Run (marloweAddressFromCardanoAddress)
import Language.Marlowe.CLI.Test.Contract.ParametrizedMarloweJSON (ParametrizedMarloweJSON)
import Language.Marlowe.CLI.Test.InterpreterError (InterpreterError, rethrowCliError, testExecutionFailed')
import Language.Marlowe.CLI.Test.Operation.Aeson qualified as Operation
import Language.Marlowe.CLI.Test.Wallet.Interpret (
  assetIdToToken,
  findWallet,
  findWalletByUniqueToken,
  getSingletonCurrency,
 )
import Language.Marlowe.CLI.Test.Wallet.Types (
  Asset (Asset),
  AssetId,
  CurrencyNickname,
  Wallet (_waAddress),
  WalletNickname (WalletNickname),
  adaToken,
  faucetNickname,
  parseTokenNameJSON,
  tokenNameToJSON,
 )
import Language.Marlowe.CLI.Test.Wallet.Types qualified as Wallet
import Language.Marlowe.CLI.Types (CliError, SomeTimeout, toPlutusPOSIXTime)
import Language.Marlowe.Core.V1.Semantics.Types qualified as C
import Language.Marlowe.Core.V1.Semantics.Types qualified as M
import Language.Marlowe.Extended.V1 (toCore)
import Language.Marlowe.Extended.V1 qualified as E
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
data PartyRef
  = WalletRef WalletNickname
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
  toJSON (WalletRef (WalletNickname walletNickname)) =
    A.object
      ["address" .= A.String (T.pack walletNickname)]
  toJSON (RoleRef tokenName) =
    A.object
      ["role_token" .= tokenNameToJSON tokenName]

data UseTemplate
  = UseTrivial
      { utParty :: Maybe PartyRef
      -- ^ The party. Falls back to the faucet wallet pkh.
      , utDepositLovelace :: Integer
      -- ^ Lovelace in the deposit.
      , utWithdrawalLovelace :: Integer
      -- ^ Lovelace in the withdrawal.
      , utTimeout :: SomeTimeout
      -- ^ The timeout.
      }
  | -- | Use for escrow contract.
    UseEscrow
      { utPrice :: Integer
      -- ^ Price of the item for sale, in lovelace.
      , utSeller :: PartyRef
      -- ^ Defaults to a wallet with the "Buyer" nickname.
      , utBuyer :: PartyRef
      -- ^ Defaults to a wallet with the "Seller" nickname.
      , utMediator :: PartyRef
      -- ^ The mediator.
      , utPaymentDeadline :: SomeTimeout
      -- ^ The deadline for the buyer to pay.
      , utComplaintDeadline :: SomeTimeout
      -- ^ The deadline for the buyer to complain.
      , utDisputeDeadline :: SomeTimeout
      -- ^ The deadline for the seller to dispute a complaint.
      , utMediationDeadline :: SomeTimeout
      -- ^ The deadline for the mediator to decide.
      }
  | -- | Use for swap contract.
    UseSwap
      { utAParty :: PartyRef
      -- ^ First party
      , utAAsset :: Asset
      , utATimeout :: SomeTimeout
      -- ^ Timeout for first party's deposit.
      , utBParty :: PartyRef
      -- ^ Second party. Falls back to wallet with "B" nickname.
      , utBAsset :: Asset
      , utBTimeout :: SomeTimeout
      -- ^ Timeout for second party's deposit.
      }
  | -- | Use for zero-coupon bond.
    UseZeroCouponBond
      { utLender :: PartyRef
      -- ^ The lender.
      , utBorrower :: PartyRef
      -- ^ The borrower.
      , utPrincipal :: Integer
      -- ^ The principal.
      , utInterest :: Integer
      -- ^ The interest.
      , utLendingDeadline :: SomeTimeout
      -- ^ The lending deadline.
      , utPaybackDeadline :: SomeTimeout
      -- ^ The payback deadline.
      }
  | -- | Use for covered call.
    UseCoveredCall
      { utIssuer :: PartyRef
      -- ^ The issuer.
      , utCounterParty :: PartyRef
      -- ^ The counter-party.
      , utCurrency :: AssetId
      -- ^ The currency token.
      , utStrike :: Integer
      -- ^ The strike in currency.
      , utUnderlying :: AssetId
      -- ^ The underlying token.
      , utAmount :: Integer
      -- ^ The amount of underlying.
      , utIssueDate :: SomeTimeout
      -- ^ The issue date.
      , utMaturityDate :: SomeTimeout
      -- ^ The maturity date.
      , utSettlementDate :: SomeTimeout
      -- ^ The settlement date.
      }
  | -- | Use for actus contracts.
    UseActus
      { utParty :: Maybe PartyRef
      -- ^ The party. Falls back to the faucet wallet.
      , utCounterParty :: PartyRef
      -- ^ The counter-party.
      , utActusTermsFile :: FilePath
      -- ^ The Actus contract terms.
      }
  deriving stock (Eq, Generic, Show)

instance FromJSON UseTemplate where
  parseJSON = Operation.parseConstructorBasedJSON' "ut"

instance ToJSON UseTemplate where
  toJSON = Operation.toConstructorBasedJSON "ut"

data Source
  = InlineContract ParametrizedMarloweJSON
  | UseTemplate UseTemplate
  deriving stock (Eq, Generic, Show)

instance ToJSON Source where
  toJSON (InlineContract c) = A.object [("inline", toJSON c)]
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

makeContract' :: (MonadError InterpreterError m) => E.Contract -> m C.Contract
makeContract' = rethrowCliError . makeContract

-- | Conversion from Extended to Core Marlowe.
makeContract :: (MonadError CliError m) => E.Contract -> m C.Contract
makeContract = liftCliMaybe "Conversion from Extended to Core Marlowe failed!" . toCore

buildParty
  :: (Wallet.InterpretMonad env st m era)
  => Maybe CurrencyNickname
  -> PartyRef
  -> m M.Party
buildParty mRoleCurrency = \case
  WalletRef nickname -> do
    wallet <- findWallet nickname
    uncurry M.Address <$> rethrowCliError (marloweAddressFromCardanoAddress (_waAddress wallet))
  RoleRef token -> do
    -- Consistency check
    currency <- case mRoleCurrency of
      Nothing -> fst <$> getSingletonCurrency
      Just cn -> pure cn
    void $ findWalletByUniqueToken currency token
    -- We are allowed to use this M.Role
    pure $ M.Role token

useTemplate
  :: (Wallet.InterpretMonad env st m era)
  => Maybe CurrencyNickname
  -> UseTemplate
  -> m M.Contract
useTemplate currency = \case
  UseTrivial{..} -> do
    timeout' <- toPlutusPOSIXTime utTimeout
    let partyRef = fromMaybe (WalletRef faucetNickname) utParty
    party <- buildParty currency partyRef
    return $
      trivial
        party
        utDepositLovelace
        utWithdrawalLovelace
        timeout'
  UseSwap{..} -> do
    aTimeout' <- toPlutusPOSIXTime utATimeout
    bTimeout' <- toPlutusPOSIXTime utBTimeout

    let Asset aAssetId aAmount = utAAsset
        Asset bAssetId bAmount = utBAsset

    aToken <- assetIdToToken aAssetId
    bToken <- assetIdToToken bAssetId

    aParty <- buildParty currency utAParty
    bParty <- buildParty currency utBParty

    return $
      swap
        aParty
        aToken
        (M.Constant aAmount)
        aTimeout'
        bParty
        bToken
        (M.Constant bAmount)
        bTimeout'
        M.Close
  UseEscrow{..} -> do
    paymentDeadline' <- toPlutusPOSIXTime utPaymentDeadline
    complaintDeadline' <- toPlutusPOSIXTime utComplaintDeadline
    disputeDeadline' <- toPlutusPOSIXTime utDisputeDeadline
    mediationDeadline' <- toPlutusPOSIXTime utMediationDeadline

    seller <- buildParty currency utSeller
    buyer <- buildParty currency utBuyer
    mediator <- buildParty currency utMediator

    return $
      escrow
        (M.Constant utPrice)
        seller
        buyer
        mediator
        paymentDeadline'
        complaintDeadline'
        disputeDeadline'
        mediationDeadline'
  UseCoveredCall{..} -> do
    issueDate <- toPlutusPOSIXTime utIssueDate
    maturityDate <- toPlutusPOSIXTime utMaturityDate
    settlementDate <- toPlutusPOSIXTime utSettlementDate
    issuer <- buildParty currency utIssuer
    counterParty <- buildParty currency utCounterParty

    ccCurrency <- assetIdToToken utCurrency
    underlying <- assetIdToToken utUnderlying

    return $
      coveredCall
        issuer
        counterParty
        Nothing
        ccCurrency
        underlying
        (M.Constant utStrike)
        (M.Constant utAmount)
        issueDate
        maturityDate
        settlementDate
  UseZeroCouponBond{..} -> do
    lendingDeadline <- toPlutusPOSIXTime utLendingDeadline
    paybackDeadline <- toPlutusPOSIXTime utPaybackDeadline

    lender <- buildParty currency utLender
    borrower <- buildParty currency utBorrower

    return $
      zeroCouponBond
        lender
        borrower
        lendingDeadline
        paybackDeadline
        (M.Constant utPrincipal)
        (M.Constant utPrincipal `M.AddValue` M.Constant utInterest)
        adaToken
        M.Close
  template -> throwError $ testExecutionFailed' $ "Template not implemented: " <> show template
