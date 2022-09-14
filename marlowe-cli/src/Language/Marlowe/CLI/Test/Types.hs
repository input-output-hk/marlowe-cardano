-----------------------------------------------------------------------------
--
-- Module      :  $Headers
-- License     :  Apache 2.0
--
-- Stability   :  Experimental
-- Portability :  Portable
--
-- | Types for testing Marlowe contracts.
--
-----------------------------------------------------------------------------


{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}


module Language.Marlowe.CLI.Test.Types
  ( AnyMarloweThread
  , AssetId(..)
  , Assets(..)
  , ContractNickname(..)
  , ContractSource(..)
  , Currency(..)
  , CurrencyNickname(..)
  , ExecutionMode(..)
  , Finished
  , MarloweContract(..)
  , MarloweReferenceScripts(..)
  , MarloweTests(..)
  , MarloweThread(..)
  , PartyRef(..)
  , Running
  , ScriptEnv(..)
  , ScriptOperation(..)
  , ScriptState
  , ScriptTest(..)
  , Seconds(..)
  , TokenAssignment(..)
  , TokenName(..)
  , UseTemplate(..)
  , Wallet(..)
  , WalletNickname(..)
  , anyMarloweThread
  , buildWallet
  , emptyWallet
  , faucetNickname
  , foldlMarloweThread
  , foldrMarloweThread
  , getMarloweThreadTransaction
  , getMarloweThreadTxBody
  , getMarloweThreadTxIn
  , overAnyMarloweThread
  , scriptState
  , seConnection
  , seCostModelParams
  , seEra
  , seExecutionMode
  , sePrintStats
  , seProtocolVersion
  , seSlotConfig
  , ssContracts
  , ssCurrencies
  , ssReferenceScripts
  , ssWallets
  ) where

import Cardano.Api
  (AddressInEra, CardanoMode, LocalNodeConnectInfo, Lovelace, NetworkId, PolicyId, ScriptDataSupportedInEra, TxBody)
import qualified Cardano.Api as C
import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON(..), ToJSON(..), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Types as A
import qualified Data.Fixed as F
import qualified Data.Fixed as Fixed
import Data.Foldable (fold)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Generics (Generic)
import GHC.Num (Natural)
import Language.Marlowe.CLI.Cardano.Api.Value (toPlutusValue, txOutValueValue)
import Language.Marlowe.CLI.Transaction (queryUtxos)
import Language.Marlowe.CLI.Types
  ( CliEnv
  , CliError
  , MarloweScriptsRefs
  , MarloweTransaction(MarloweTransaction, mtInputs)
  , PrintStats
  , SomePaymentSigningKey
  , SomeTimeout
  )
import qualified Language.Marlowe.Core.V1.Semantics.Types as M
import qualified Language.Marlowe.Extended.V1 as E
import Ledger.Orphans ()
import Plutus.ApiCommon (ProtocolVersion)
import Plutus.V1.Ledger.Api (CostModelParams, CurrencySymbol, TokenName)
import Plutus.V1.Ledger.SlotConfig (SlotConfig)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)


-- | Configuration for a set of Marlowe tests.
data MarloweTests era a =
    -- | Test contracts on-chain.
    ScriptTests
    {
      stNetwork              :: NetworkId   -- ^ The network ID, if any.
    , stSocketPath           :: FilePath    -- ^ The path to the node socket.
    , stFaucetSigningKeyFile :: FilePath    -- ^ The file containing the faucet's signing key.
    , stFaucetAddress        :: AddressInEra era  -- ^ The faucet address.
    , stBurnAddress          :: AddressInEra era -- ^ The address to which to send unneeded native tokens.
    , stExecutionMode        :: ExecutionMode
    , stTests                :: [a]         -- ^ Input for the tests.
    }
    deriving stock (Eq, Generic, Show)

-- | Configuration for executing Marlowe CLI DSL commands on the blockchain
-- | The idea behind simulation mode is to use it as a "first line of defense" to detect
-- | errors in code or in the test case itself before spending the time/resources to run the same
-- | scenarios on chain.
-- | Tests that fail in simulation mode should also fail on chain
-- | Tests that pass on chain should also pass in simulation mode
data ExecutionMode = SimulationMode | OnChainMode { transactionSubmissionTimeout :: Seconds }
    deriving stock (Eq, Show)

-- | An on-chain test of the Marlowe contract and payout validators.
data ScriptTest =
  ScriptTest
  {
    stTestName         :: String             -- ^ The name of the test.
  , stScriptOperations :: [ScriptOperation]  -- ^ The sequence of test operations.
  }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


data ContractSource = InlineContract A.Value | UseTemplate UseTemplate
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


-- In the current setup we use really a unique currency
-- per issuer. Should we drop this and identify currencies
-- using `WalletNickname`?
newtype CurrencyNickname = CurrencyNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
instance IsString CurrencyNickname where fromString = CurrencyNickname


data TokenAssignment = TokenAssignment
  { taWalletNickname :: WalletNickname  -- ^ Default to the same wallet nickname as a token name.
  , taTokenName      :: TokenName
  , taAmount         :: Natural
  }
  deriving stock (Eq, Generic, Show)


parseTokenNameJSON :: Aeson.Value -> A.Parser P.TokenName
parseTokenNameJSON json = do
  parseJSON (A.object [("unTokenName", json)])


tokenNameToJSON :: TokenName -> Aeson.Value
tokenNameToJSON (P.TokenName tokenName) = toJSON tokenName


instance FromJSON TokenAssignment where
  parseJSON = \case
    A.Array (V.toList -> [walletNicknameJSON, tokenNameJSON, amountJSON]) -> do
      walletNickname <- parseJSON walletNicknameJSON
      tokenName <- parseTokenNameJSON tokenNameJSON
      amount <- parseJSON amountJSON
      pure $ TokenAssignment walletNickname tokenName amount
    _ -> fail "Expecting a `TokenAssignment` tuple: `[WalletNickname, TokenName, Integer]`."


instance ToJSON TokenAssignment where
    toJSON (TokenAssignment (WalletNickname walletNickname) tokenName amount) = do
      toJSON [toJSON walletNickname, tokenNameToJSON tokenName, toJSON amount]


data AssetId = AdaAsset | AssetId CurrencyNickname TokenName
  deriving (Eq, Ord, Show)


instance FromJSON AssetId where
  parseJSON = \case
    A.String (T.toLower -> "ada") -> pure AdaAsset
    A.Array (V.toList -> [currencyNicknameJSON, tokenNameJSON]) -> do
      currencyNickname <- parseJSON currencyNicknameJSON
      tokenName <- parseTokenNameJSON tokenNameJSON
      pure $ AssetId currencyNickname tokenName
    _ -> fail "Expecting an AssetId \"tuple\" or \"ada\"."


instance ToJSON AssetId where
    toJSON AdaAsset = A.String "ADA"
    toJSON (AssetId currencyNickname tokenName) =
      toJSON [ toJSON currencyNickname, tokenNameToJSON tokenName ]


-- | Yaml friendly representation of assets which we use in balance checking.
newtype Assets = Assets (Map AssetId Integer)
  deriving (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)


assetsSingleton :: AssetId -> Integer -> Assets
assetsSingleton assetId = Assets . Map.singleton assetId


lovelaceAssets :: Integer -> Assets
lovelaceAssets = assetsSingleton AdaAsset


instance FromJSON Assets where
  parseJSON json = do
    (assetsEntries :: [A.Value]) <- parseJSON json
    assets <- for assetsEntries parseAssetEntry
    pure $ fold assets
    where
      parseAssetEntry = \case
        lovelaceJSON@(Aeson.Number _) -> do
          lovelaceAssets <$> parseJSON lovelaceJSON
        Aeson.String txt -> do
          let
            txt' = T.replace "_" "" txt
          lovelaceAssets <$> parseJSON (A.String txt')
        A.Array (V.toList -> [assetJSON, amountJSON]) -> do
          -- Matches ADA asset
          parseJSON assetJSON >>= \case
            AdaAsset -> pure ()
            _ -> fail "Expecting ADA asset"
          (_ :: AssetId) <- parseJSON assetJSON
          (amount :: Fixed.Micro) <- case amountJSON of
            -- Handle decimal as String
            A.String str -> case readMaybe (T.unpack str) of
              Just a -> pure a
              Nothing -> fail "Unable to parse ADA amount"
            -- Handle plain Integer
            _ -> parseJSON amountJSON
          pure $ lovelaceAssets (floor $ 1_000_000 * amount)
        A.Array (V.toList -> [assetNameJSON, tokenNameJSON, amountJSON]) -> do
          assetId <- parseJSON $ A.Array $ V.fromList [assetNameJSON, tokenNameJSON]
          amount <- parseJSON amountJSON
          pure $ assetsSingleton assetId amount
        _ -> fail "Expecting an assets map encoding."


instance ToJSON Assets where
    toJSON (Assets (Map.toList -> assetsList)) = toJSON $ map assetToJSON assetsList
      where
        assetToJSON (AdaAsset, amount) = toJSON
          [ toJSON ("ADA" :: String)
          , toJSON $ show (fromInteger amount :: F.Micro)
          ]
        assetToJSON (AssetId currencyNickname tokenName, amount) = toJSON
          [ toJSON currencyNickname, toJSON tokenName, toJSON amount ]


-- | On-chain test operations for the Marlowe contract and payout validators.
data ScriptOperation =
    -- | We use "private" currency minting policy which
    -- | checks for a signature of a particular issuer.
    Mint
    {
      soCurrencyNickname  :: CurrencyNickname
    , soIssuer            :: Maybe WalletNickname   -- ^ Fallbacks to faucet
    , soMetadata          :: Maybe Aeson.Object
    , soTokenDistribution :: [TokenAssignment]
    }
  | BurnAll
    {
      soCurrencyNickname    :: CurrencyNickname
    , soMetadata            :: Maybe Aeson.Object
    }
  | CheckBalance
    { soWalletNickname  :: WalletNickname
    , soBalance         :: Assets           -- ^ Expected delta of funds in the case of `Faucet` wallet we
                                            -- subtract the baseline funds so they are not included in this balance.
                                            -- We also exclude fees from this calculation.
    }
  | Initialize
    {
      soMinAda           :: Integer
    , soContractNickname :: ContractNickname        -- ^ The name of the wallet's owner.
    , soRoleCurrency     :: Maybe CurrencyNickname  -- ^ If contract uses roles then currency is required.
    , soContractSource   :: ContractSource          -- ^ The Marlowe contract to be created.
    , soSubmitter        :: Maybe WalletNickname    -- ^ A wallet which gonna submit the initial transaction.
    }
  | Prepare
    {
      soContractNickname     :: ContractNickname  -- ^ The name of the contract.
    , soInputs               :: [A.Value]         -- ^ Inputs to the contract.
    , soMinimumTime          :: SomeTimeout
    , soMaximumTime          :: SomeTimeout
    , soOverrideMarloweState :: Maybe M.State
    }
  | Publish
    { soPublisher          :: Maybe WalletNickname   -- ^ Wallet used to cover fees. Falls back to faucet wallet.
    , soPublishPermanently :: Maybe Bool             -- ^ Whether to publish script permanently.
    }
  | AutoRun
    {
      soContractNickname :: ContractNickname
    , soInvalid          :: Maybe Bool
    }
  | CreateWallet
    {
      soWalletNickname  :: WalletNickname
    }
  | Withdraw
   {
     soContractNickname :: ContractNickname
   , soWalletNickname   :: WalletNickname
   }
  | FundWallets
    {
      soWalletNicknames  :: [WalletNickname]
    , soValues           :: [Lovelace]
    , soCreateCollateral :: Maybe Bool
    }
  | SplitWallet
    {
      soWalletNickname :: WalletNickname
    , soValues         :: [Lovelace]
    }
  | Fail
    {
      soFailureMessage :: String
    }
    deriving stock (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)


data Wallet era =
  Wallet
  {
    waAddress                   :: AddressInEra era
  , waBalanceCheckBaseline      :: P.Value                  -- ^ This value should reflect all the assets from the wallet which
                                                            -- were on the chain when we started a particular scenario. Currently
                                                            -- it is only used in the context of `Faucet` wallet.
  , waMintedTokens              :: P.Value                  -- ^ Tracks all the minted tokens to simplify auto run flow.
  , waSigningKey                :: SomePaymentSigningKey
  , waSubmittedTransactions     :: [TxBody era]             -- ^ We keep track of all the transaction so we can
                                                            -- discard fees from the balance check calculation.
  , waMintingDistributionCosts  :: P.Value                  -- ^ Total min ADA costs of all mintings to other parties.
  }
  deriving stock (Generic, Show)


emptyWallet :: AddressInEra era -> SomePaymentSigningKey -> Wallet era
emptyWallet address signignKey = Wallet address mempty mempty signignKey mempty mempty


buildWallet :: MonadError CliError m
            => MonadIO m
            => MonadReader (CliEnv era) m
            => C.LocalNodeConnectInfo CardanoMode
            -> AddressInEra era
            -> SomePaymentSigningKey
            -> m (Wallet era)
buildWallet connection address signignKey = do
  C.UTxO (Map.elems -> txOuts) <- queryUtxos connection address
  let
    total = foldMap (toPlutusValue . txOutValueValue) txOuts
  pure $ Wallet address total mempty signignKey mempty mempty


-- | In many contexts this defaults to the `RoleName` but at some
-- | point we want to also support multiple marlowe contracts scenarios
-- | in a one transaction.
newtype WalletNickname = WalletNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance IsString WalletNickname where fromString = WalletNickname


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
      utPrice             :: Lovelace    -- ^ Price of the item for sale, in lovelace.
    , utSeller            :: PartyRef   -- ^ Defaults to a wallet with the "Buyer" nickname.
    , utBuyer             :: PartyRef    -- ^ Defaults to a wallet with the "Seller" ncikname.
    , utMediator          :: PartyRef   -- ^ The mediator.
    , utPaymentDeadline   :: SomeTimeout -- ^ The deadline for the buyer to pay.
    , utComplaintDeadline :: SomeTimeout -- ^ The deadline for the buyer to complain.
    , utDisputeDeadline   :: SomeTimeout -- ^ The deadline for the seller to dispute a complaint.
    , utMediationDeadline :: SomeTimeout -- ^ The deadline for the mediator to decide.
    }
    -- | Use for swap contract.
  | UseSwap
    {
      utAParty            :: PartyRef    -- ^ First party.
    , utACurrencyNickname :: CurrencyNickname
    , utATokenName        :: TokenName
    , utAAmount           :: Integer    -- ^ Amount of first party's token.
    , utATimeout          :: SomeTimeout -- ^ Timeout for first party's deposit.
    , utBParty            :: PartyRef    -- ^ Second party.
    , utBCurrencyNickname :: CurrencyNickname
    , utBTokenName        :: TokenName
    , utBAmount           :: Integer    -- ^ Amount of second party's token.
    , utBTimeout          :: SomeTimeout -- ^ Timeout for second party's deposit.
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


newtype ContractNickname = ContractNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
instance IsString ContractNickname where fromString = ContractNickname


data Running

data Finished

data MarloweThread lang era status where
  Created :: MarloweTransaction lang era
          -> C.TxBody era
          -> C.TxIn
          -> MarloweThread lang era Running
  InputsApplied :: MarloweTransaction lang era
                -> C.TxBody era
                -> C.TxIn
                -> List.NonEmpty M.Input
                -> MarloweThread lang era Running
                -> MarloweThread lang era Running
  Closed :: MarloweTransaction lang era
         -> C.TxBody era
         -> [M.Input]
         -> MarloweThread lang era Running
         -> MarloweThread lang era Finished


foldlMarloweThread :: (forall status'. b -> MarloweThread lang era status' -> b) -> b -> MarloweThread lang era status -> b
foldlMarloweThread step acc m@(Closed _ _ _ th)          = foldlMarloweThread step (step acc m) th
foldlMarloweThread step acc m@(InputsApplied _ _ _ _ th) = foldlMarloweThread step (step acc m) th
foldlMarloweThread step acc m                            = step acc m


foldrMarloweThread :: (forall status'. MarloweThread lang era status' -> b -> b) -> b -> MarloweThread lang era status -> b
foldrMarloweThread step acc m@(Closed _ _ _ th)          = step m (foldrMarloweThread step acc th)
foldrMarloweThread step acc m@(InputsApplied _ _ _ _ th) = step m (foldrMarloweThread step acc th)
foldrMarloweThread step acc m                            = step m acc


getMarloweThreadTransaction :: MarloweThread lang era status -> MarloweTransaction lang era
getMarloweThreadTransaction (Created mt _ _)           = mt
getMarloweThreadTransaction (InputsApplied mt _ _ _ _) = mt
getMarloweThreadTransaction (Closed mt _ _ _)          = mt


getMarloweThreadTxIn :: MarloweThread lang era status -> Maybe C.TxIn
getMarloweThreadTxIn (Created _ _ txIn)           = Just txIn
getMarloweThreadTxIn (InputsApplied _ _ txIn _ _) = Just txIn
getMarloweThreadTxIn Closed {}                    = Nothing


getMarloweThreadTxBody :: MarloweThread lang era status -> C.TxBody era
getMarloweThreadTxBody (Created _ txBody _)           = txBody
getMarloweThreadTxBody (InputsApplied _ txBody _ _ _) = txBody
getMarloweThreadTxBody (Closed _ txBody _ _)          = txBody


data AnyMarloweThread lang era where
  AnyMarloweThread :: MarloweThread lang era status -> AnyMarloweThread lang era


anyMarloweThread :: MarloweTransaction lang era
                 -> C.TxBody era
                 -> Maybe C.TxIn
                 -> Maybe (AnyMarloweThread lang era)
                 -> Maybe (AnyMarloweThread lang era)
anyMarloweThread mt@MarloweTransaction{..} txBody mTxIn mth = case (mTxIn, mtInputs, mth) of
  (Just txIn, [], Nothing) -> Just (AnyMarloweThread (Created mt txBody txIn))

  (Just txIn, input:inputs, Just (AnyMarloweThread th)) -> case th of
    m@Created {}       -> Just $ AnyMarloweThread $ InputsApplied mt txBody txIn (input :| inputs) m
    m@InputsApplied {} -> Just $ AnyMarloweThread $ InputsApplied mt txBody txIn (input :| inputs) m
    _                  -> Nothing

  (Nothing, inputs, Just (AnyMarloweThread th)) -> case th of
    m@Created {}       -> Just $ AnyMarloweThread $ Closed mt txBody inputs m
    m@InputsApplied {} -> Just $ AnyMarloweThread $ Closed mt txBody inputs m
    _                  -> Nothing

  -- Inputs are not allowed in the initial transaction.
  (Just _, _:_, Nothing) -> Nothing
  -- We disallow empty intermediate input application in here.
  -- Is there a non closing contract reduction which can be
  -- triggered like that?
  (Just _, [], Just _) -> Nothing
  -- It is impossible to deploy anything without marlowe output.
  (Nothing, _, Nothing) -> Nothing


overAnyMarloweThread :: forall a era lang
                      . (forall status'. MarloweThread lang era status' -> a)
                      -> AnyMarloweThread lang era
                      -> a
overAnyMarloweThread f (AnyMarloweThread th) = f th


data MarloweContract lang era = MarloweContract
  {
    mcContract                :: M.Contract
  , mcCurrency                :: Maybe CurrencyNickname
  , mcPlan                    :: List.NonEmpty (MarloweTransaction lang era)
  , mcThread                  :: Maybe (AnyMarloweThread lang era)
  , mcWithdrawalsCheckPoints  :: Map TokenName C.TxId                           -- ^ Track of last withrawal on chain point.
                                                                                -- We need this to do a check if there are
                                                                                -- waiting withdrawals for a party on chain.
  , mcSubmitter               :: WalletNickname
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


data ScriptState lang era = ScriptState
  {
    _ssContracts        :: Map ContractNickname (MarloweContract lang era)
  , _ssCurrencies       :: Map CurrencyNickname Currency
  , _ssReferenceScripts :: Maybe (MarloweScriptsRefs lang era)
  , _ssWallets          :: Map WalletNickname (Wallet era)                    -- ^ Faucet wallet should be included here.
  }


faucetNickname :: WalletNickname
faucetNickname = "Faucet"


scriptState :: Wallet era -> ScriptState lang era
scriptState faucet = do
  let
    wallets = Map.singleton faucetNickname faucet
  ScriptState mempty mempty Nothing wallets


newtype Seconds = Seconds Int
    deriving stock (Eq, Show)


data ScriptEnv era = ScriptEnv
  { _seConnection         :: LocalNodeConnectInfo CardanoMode
  , _seCostModelParams    :: CostModelParams
  , _seEra                :: ScriptDataSupportedInEra era
  , _seProtocolVersion    :: ProtocolVersion
  , _seSlotConfig         :: SlotConfig
  , _seExecutionMode      :: ExecutionMode
  , _sePrintStats         :: PrintStats
  }


makeLenses 'ScriptEnv
makeLenses 'ScriptState

