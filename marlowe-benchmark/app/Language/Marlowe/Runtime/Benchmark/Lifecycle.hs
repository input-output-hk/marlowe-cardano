{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Benchmark the basic contract lifecycle.
module Language.Marlowe.Runtime.Benchmark.Lifecycle (
  -- * Benchmarking
  runScenario,
  measure,
) where

import Control.Monad (forM_, replicateM, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Marlowe (MarloweT)
import Control.Monad.Trans.Marlowe.Class (MonadMarlowe, applyInputs, createContract, submitAndWait)
import Data.Aeson (ToJSON)
import Data.Bifunctor (second)
import Data.Maybe (fromJust, mapMaybe)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe.Core.V1.Semantics.Types (Contract, Input (NormalInput), Party (Address, Role))
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Runtime.Benchmark.Lifecycle.Scenario (Scenario (..), randomScenario)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId, toCardanoAddressAny, toCardanoAddressInEra)
import Language.Marlowe.Runtime.ChainSync.Api (Address (unAddress), TokenName (TokenName), TxId)
import Language.Marlowe.Runtime.Core.Api (ContractId, MarloweVersion (..), emptyMarloweTransactionMetadata)
import Language.Marlowe.Runtime.Transaction.Api (
  ContractCreated (..),
  ContractCreatedInEra (..),
  Destination (ToAddress),
  InputsApplied (..),
  InputsAppliedInEra (..),
  RoleTokensConfig (..),
  WalletAddresses (WalletAddresses),
  mkMint,
 )
import PlutusLedgerApi.V2 (fromBuiltin, unTokenName)
import System.IO (hPutStrLn, stderr)
import Test.QuickCheck (generate, infiniteListOf)
import Test.QuickCheck.Hedgehog (hedgehog)
import UnliftIO (forConcurrently)

import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as C
import qualified Data.ByteString as BS
import qualified Data.List.NonEmpty as NE (fromList)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain (Address (Address))
import qualified Test.Gen.Cardano.Api.Typed as C (genShelleyWitnessSigningKey)

data Benchmark = Benchmark
  { metric :: String
  , contractsPerSecond :: Double
  , creationsPerSecond :: Double
  , appliesPerSecond :: Double
  , withdrawsPerSecond :: Double
  , seconds :: Double
  }
  deriving (Eq, Generic, Ord, Show, ToJSON)

measure
  :: (C.IsShelleyBasedEra era)
  => C.SocketPath
  -> C.CardanoEra era
  -> C.NetworkId
  -> Int
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Int
  -> MarloweT IO [Benchmark]
measure node era network parallelism faucetAddress faucetKey count =
  do
    let lovelacePerClient = C.lovelaceToValue . fromIntegral $ 10_000_000 + 1_000_000 * count
    keys <- liftIO . replicateM parallelism $ genKey faucetAddress
    let local = C.LocalNodeConnectInfo (C.CardanoModeParams $ C.EpochSlots 432_000) network node
    Right systemStart <- liftIO $ C.queryNodeLocalState local Nothing C.QuerySystemStart
    Right ledgerEpochInfo <-
      liftIO $
        fmap (fmap C.toLedgerEpochInfo) $
          C.queryNodeLocalState local Nothing $
            C.QueryEraHistory C.CardanoModeIsMultiEra
    Right protocol <-
      liftIO $
        C.executeQueryCardanoMode node era network $
          C.QueryInEra (fromJust $ C.toEraInMode era C.CardanoMode) $
            C.QueryInShelleyBasedEra C.shelleyBasedEra C.QueryProtocolParameters
    Right utxos <-
      liftIO $
        C.executeQueryCardanoMode node era network $
          C.QueryInEra (fromJust $ C.toEraInMode era C.CardanoMode) $
            C.QueryInShelleyBasedEra C.shelleyBasedEra $
              C.QueryUTxO $
                C.QueryUTxOByAddress . S.singleton . fromJust $
                  toCardanoAddressAny faucetAddress
    let txBodyContent =
          C.TxBodyContent
            { txIns = second (const . C.BuildTxWith $ C.KeyWitness C.KeyWitnessForSpending) <$> M.toList (C.unUTxO utxos)
            , txInsCollateral = C.TxInsCollateralNone
            , txInsReference = C.TxInsReferenceNone
            , txOuts =
                [ C.TxOut
                  (fromJust $ toCardanoAddressInEra era address)
                  (C.TxOutValue (either (error "fromRight") id $ C.multiAssetSupportedInEra era) lovelacePerClient)
                  C.TxOutDatumNone
                  C.ReferenceScriptNone
                | (address, _) <- keys
                ]
            , txTotalCollateral = C.TxTotalCollateralNone
            , txReturnCollateral = C.TxReturnCollateralNone
            , txFee = C.TxFeeExplicit (either (error "fromRight") id $ C.txFeesExplicitInEra era) 0
            , txValidityRange =
                (C.TxValidityNoLowerBound, C.TxValidityNoUpperBound . fromJust $ C.validityNoUpperBoundSupportedInEra era)
            , txMetadata = C.TxMetadataNone
            , txAuxScripts = C.TxAuxScriptsNone
            , txExtraKeyWits = C.TxExtraKeyWitnessesNone
            , txProtocolParams = C.BuildTxWith Nothing
            , txWithdrawals = C.TxWithdrawalsNone
            , txCertificates = C.TxCertificatesNone
            , txUpdateProposal = C.TxUpdateProposalNone
            , txMintValue = C.TxMintNone
            , txScriptValidity = C.TxScriptValidityNone
            }
    Right (C.BalancedTxBody _ txBody _ _) <-
      pure $
        C.makeTransactionBodyAutoBalance
          systemStart
          ledgerEpochInfo
          protocol
          mempty
          mempty
          utxos
          txBodyContent
          (fromJust $ toCardanoAddressInEra era faucetAddress)
          Nothing
    void $ signSubmit (fromJust $ C.refInsScriptsAndInlineDatsSupportedInEra era) faucetKey txBody
    -- FIXME: Catch exceptions and finalize by returning funds to faucet.
    forConcurrently keys $
      uncurry (run "Lifecycle" count)

genKey
  :: Address
  -> IO (Address, C.SigningKey C.PaymentExtendedKey)
genKey faucetAddress =
  do
    let isPaymentExtendedKey (C.WitnessPaymentExtendedKey key) = Just key
        isPaymentExtendedKey _ = Nothing
    skey <-
      fmap (head . mapMaybe isPaymentExtendedKey)
        . generate
        . infiniteListOf
        $ hedgehog C.genShelleyWitnessSigningKey
    let address =
          Chain.Address $
            (BS.singleton . BS.head . unAddress) faucetAddress
              <> (C.serialiseToRawBytes . C.verificationKeyHash . C.getVerificationKey) skey
    liftIO
      . hPutStrLn stderr
      $ "Generated address " <> show address <> " and signing key " <> show skey <> "."
    pure (address, skey)

run
  :: String
  -> Int
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> MarloweT IO Benchmark
run metric count address key =
  do
    let address' = uncurry Address . fromJust . deserialiseAddress $ unAddress address
    start <- liftIO getPOSIXTime
    scenarios <- liftIO . replicateM count $ randomScenario 1200 [address']
    mapM_ (runScenario $ M.singleton address' (address, key)) scenarios
    finish <- liftIO getPOSIXTime
    let seconds = realToFrac $ finish - start
        contractsPerSecond = realToFrac (length scenarios) / seconds
        creationsPerSecond = realToFrac (length scenarios) / seconds
        appliesPerSecond = realToFrac (sum $ length . actions <$> scenarios) / seconds
        withdrawsPerSecond = 0
    pure Benchmark{..}

runScenario
  :: (MonadIO m)
  => (MonadMarlowe m)
  => M.Map Party (Address, C.SigningKey C.PaymentExtendedKey)
  -> Scenario
  -> m ()
runScenario partyCredentials Scenario{..} =
  let act contractId party action =
        let (address, key) = partyCredentials M.! party
         in apply address key contractId [NormalInput action]
   in forM_ actions . uncurry . act
        =<< create partyCredentials contract

create
  :: (MonadIO m)
  => (MonadMarlowe m)
  => M.Map Party (Address, C.SigningKey C.PaymentExtendedKey)
  -> Contract
  -> m ContractId
create partyCredentials contract =
  do
    let (_, (change, key)) = M.findMin partyCredentials
        distributeRole (Role token) (address, _) = Just (TokenName . fromBuiltin $ unTokenName token, Nothing, ToAddress address, 1)
        distributeRole _ _ = Nothing
        distributeRoles = mapMaybe (uncurry distributeRole) $ M.toList partyCredentials
        roles =
          if null distributeRoles
            then RoleTokensNone
            else RoleTokensMint . mkMint $ NE.fromList distributeRoles
    ContractCreated support ContractCreatedInEra{contractId, txBody} <-
      createContract
        Nothing
        MarloweV1
        (WalletAddresses change mempty mempty)
        Nothing
        roles
        emptyMarloweTransactionMetadata
        Nothing
        (Left contract)
        >>= either (error . show) pure
    void $ signSubmit support key txBody
    pure contractId

apply
  :: (MonadIO m)
  => (MonadMarlowe m)
  => Address
  -> C.SigningKey C.PaymentExtendedKey
  -> ContractId
  -> [Input]
  -> m TxId
apply change key contractId input =
  do
    InputsApplied support InputsAppliedInEra{txBody} <-
      applyInputs
        MarloweV1
        (WalletAddresses change mempty mempty)
        contractId
        emptyMarloweTransactionMetadata
        input
        >>= either (error . show) pure
    signSubmit support key txBody

-- TODO: Here we should determine any payouts and withdraw them.

signSubmit
  :: (MonadIO m)
  => (MonadMarlowe m)
  => C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -> C.SigningKey C.PaymentExtendedKey
  -> C.TxBody era
  -> m TxId
signSubmit support key txBody =
  case support of
    C.ReferenceTxInsScriptsInlineDatumsInBabbageEra -> signSubmit' support key txBody
    C.ReferenceTxInsScriptsInlineDatumsInConwayEra -> signSubmit' support key txBody

signSubmit'
  :: (C.IsShelleyBasedEra era)
  => (MonadIO m)
  => (MonadMarlowe m)
  => C.ReferenceTxInsScriptsInlineDatumsSupportedInEra era
  -> C.SigningKey C.PaymentExtendedKey
  -> C.TxBody era
  -> m TxId
signSubmit' support key txBody =
  do
    let tx = C.signShelleyTransaction txBody [C.WitnessPaymentExtendedKey key]
    submitAndWait support tx
      >>= \case
        Right _ -> pure . fromCardanoTxId $ C.getTxId txBody
        Left e -> error $ show e
