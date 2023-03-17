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
{-# LANGUAGE TypeApplications #-}
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

module Language.Marlowe.CLI.Test.Wallet.Types
  where

import Cardano.Api
  (AddressInEra, CardanoMode, LocalNodeConnectInfo, Lovelace, PolicyId, ScriptDataSupportedInEra, TxBody)
import Contrib.Data.Aeson.Generic (getConName)
import Control.Lens (makeLenses)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State.Strict (StateT)
import Data.Aeson (FromJSON(..), ToJSON(..))
import qualified Data.Aeson as A
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as A
import qualified Data.Fixed as F
import qualified Data.Fixed as Fixed
import Data.Foldable (fold)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.String (IsString(fromString))
import qualified Data.Text as T
import Data.Traversable (for)
import qualified Data.Vector as V
import GHC.Generics (Generic(from))
import GHC.Natural (Natural)
import Language.Marlowe.CLI.Test.ExecutionMode (ExecutionMode)
import Language.Marlowe.CLI.Types (CliError(CliError), PrintStats(PrintStats), SomePaymentSigningKey)
import Ledger.Orphans ()
import Plutus.V1.Ledger.Api (CurrencySymbol, TokenName)
import qualified Plutus.V1.Ledger.Value as P
import Text.Read (readMaybe)

data Wallet era =
  Wallet
  { waAddress                   :: AddressInEra era
  , waBalanceCheckBaseline      :: P.Value                  -- ^ This value should reflect all the assets from the wallet which
                                                            -- were on the chain when we started a particular scenario. Currently
                                                            -- it is only used in the context of `Faucet` wallet.
  , waMintedTokens              :: P.Value                  -- ^ Tracks all the minted tokens to simplify auto run flow.
  , waSigningKey                :: SomePaymentSigningKey
  , waSubmittedTransactions     :: [TxBody era]             -- ^ We keep track of all the transaction so we can
                                                            -- discard fees from the balance check calculation.
  }
  deriving stock (Generic, Show)

emptyWallet :: AddressInEra era -> SomePaymentSigningKey -> Wallet era
emptyWallet address signignKey = Wallet address mempty mempty signignKey mempty -- mempty

-- | In many contexts this defaults to the `RoleName` but at some
-- | point we want to also support multiple marlowe contracts scenarios
-- | in a one transaction.
newtype WalletNickname = WalletNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance IsString WalletNickname where fromString = WalletNickname

faucetNickname :: WalletNickname
faucetNickname = "Faucet"

-- In the current setup we use really a unique currency
-- per issuer. Should we drop this and identify currencies
-- using `WalletNickname`?
newtype CurrencyNickname = CurrencyNickname String
    deriving stock (Eq, Ord, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)
instance IsString CurrencyNickname where fromString = CurrencyNickname

data Currency = Currency
  {
    ccCurrencySymbol :: CurrencySymbol
  , ccIssuer         :: WalletNickname
  , ccPolicyId       :: PolicyId
  }

newtype Currencies = Currencies (Map CurrencyNickname Currency)

data AssetId = AdaAsset | AssetId CurrencyNickname TokenName
  deriving (Eq, Ord, Show)

parseTokenNameJSON :: Aeson.Value -> A.Parser P.TokenName
parseTokenNameJSON json = do
  parseJSON (A.object [("unTokenName", json)])

tokenNameToJSON :: TokenName -> Aeson.Value
tokenNameToJSON (P.TokenName tokenName) = toJSON tokenName

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

data TokenAssignment = TokenAssignment
  { taWalletNickname :: WalletNickname  -- ^ Default to the same wallet nickname as a token name.
  , taTokenName      :: TokenName
  , taAmount         :: Natural
  }
  deriving stock (Eq, Generic, Show)

instance FromJSON TokenAssignment where
  parseJSON = \case
    A.Array (V.toList -> [walletNicknameJSON, tokenNameJSON, amountJSON]) -> do
      walletNickname <- parseJSON walletNicknameJSON
      tokenName <- parseTokenNameJSON tokenNameJSON
      amount <- parseJSON amountJSON
      pure $ TokenAssignment walletNickname tokenName amount
    _ -> fail "Expecting a `TokenAssignment` tuple: `[WalletNickname, TokenName, Integer]`."

instance ToJSON TokenAssignment where
    toJSON (TokenAssignment (WalletNickname walletNickname) tokenName amount) =
      toJSON [toJSON walletNickname, tokenNameToJSON tokenName, toJSON amount]

-- Parts of this operation set is implemented in `marlowe-cli`. Should we extract
-- this to a separate package/tool like `cardano-testing-wallet`?
data WalletOperation =
    BurnAll
    {
      soCurrencyNickname    :: CurrencyNickname
    , soMetadata            :: Maybe Aeson.Object
    }
  | CreateWallet
    {
      soWalletNickname  :: WalletNickname
    }
  | CheckBalance
    { soWalletNickname  :: WalletNickname
    , soBalance         :: Assets           -- ^ Expected delta of funds:
                                            -- * We exclude tx fees from this calculation.
                                            -- * We DON'T subtract the minimum lovelace amount (attached when tokens are
                                            --  sent or attached to the contract during the execution).
                                            -- * In the case of `Faucet` wallet we subtract the baseline
                                            --  funds so they are not included in this balance.
    }
  | FundWallets
    {
      soWalletNicknames  :: [WalletNickname]
    , soValues           :: [Lovelace]
    , soCreateCollateral :: Maybe Bool
    }
  | Mint
    {
      soCurrencyNickname  :: CurrencyNickname
    , soIssuer            :: Maybe WalletNickname   -- ^ Fallbacks to faucet
    , soMetadata          :: Maybe Aeson.Object
    , soTokenDistribution :: [TokenAssignment]
    , soMinLovelace       :: Lovelace
    }

  | SplitWallet
    {
      soWalletNickname :: WalletNickname
    , soValues         :: [Lovelace]
    }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

newtype Wallets era = Wallets (Map WalletNickname (Wallet era))

-- conNames = getConNames @(Rep WalletOperation)

data InterpretState era = InterpretState
  { _isWallets :: Wallets era
  , _isCurrencies :: Currencies
  }

data InterpretEnv era = InterpretEnv
  { _ieConnection :: LocalNodeConnectInfo CardanoMode
  , _ieEra :: ScriptDataSupportedInEra era
  , _iePrintStats :: PrintStats
  , _ieExecutionMode :: ExecutionMode
  }


type InterpretMonad m era =
  ( MonadState (InterpretState era) m
  , MonadReader (InterpretEnv era) m
  , MonadError CliError m
  , MonadIO m
  )

makeLenses 'InterpretState
makeLenses 'InterpretEnv
