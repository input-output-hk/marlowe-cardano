{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.CliSpec
  where

import Cardano.Api (AsType(AsTxBody), BabbageEra, CardanoEra(BabbageEra), TxBody, readFileTextEnvelope)
import Control.Concurrent.Async.Lifted (concurrently)
import qualified Control.Monad.Base as Trans
import qualified Control.Monad.Reader as Reader
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), Lovelace(Lovelace), TransactionMetadata, toBech32)
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Core.Api
  (MarloweTransactionMetadata(..), MarloweVersion(MarloweV1), MarloweVersionTag(V1))
import Language.Marlowe.Runtime.Integration.Common
  (Integration, Wallet(..), execMarlowe_, getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), CreateError, MarloweTxCommand(..), RoleTokensConfig(RoleTokensNone), WalletAddresses(..))
import qualified Network.Protocol.Job.Client as JobClient
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe
  (LocalTestnet(..), resolveWorkspacePath, testnet, withLocalMarloweRuntime, writeWorkspaceFileJSON)

serializeAddress :: Address -> String
serializeAddress = Text.unpack . Maybe.fromJust . toBech32

toCliArgs :: MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) -> [String]
toCliArgs (Create _ _ WalletAddresses {changeAddress, extraAddresses} _ _ (Lovelace minUTXO) _) =
  ["create", "--change-address", serializeAddress changeAddress]
    <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
    <> ["--min-utxo", show minUTXO]

marloweRuntimeJobClient :: MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) -> Integration (TxBody BabbageEra)
marloweRuntimeJobClient cmd = do
  (Either.fromRight (error "Some JobClient creation error!") -> ContractCreated {txBody}) <- runMarloweTxClient $ JobClient.liftCommand cmd
  pure txBody

spec :: Spec
spec = describe "Marlowe runtime CLI" do
  describe "create" do
    it "creates a tx body envelope" $
      withLocalMarloweRuntime $ runIntegrationTest do
        Wallet {addresses} <- getGenesisWallet 0

        workspace <- Reader.asks $ workspace . testnet

        let transactionMetadata :: TransactionMetadata
            transactionMetadata = mempty

            contract :: V1.Contract
            contract = V1.Close

            txBodyEnvelopeFilePath :: FilePath
            txBodyEnvelopeFilePath = resolveWorkspacePath workspace "tx-body.json"

        contractFilePath <- writeWorkspaceFileJSON workspace "close-contract.json" contract
        transactionMetadataFilePath <- writeWorkspaceFileJSON workspace "transaction-metadata.json" transactionMetadata

        let creationCommand :: MarloweTxCommand Void (CreateError 'V1) (ContractCreated BabbageEra 'V1)
            creationCommand =
              Create
                Nothing
                MarloweV1
                addresses
                RoleTokensNone
                (MarloweTransactionMetadata Nothing transactionMetadata)
                (Lovelace 2_000_000)
                contract

            cliEffect :: Integration ()
            cliEffect =
              execMarlowe_ $
                toCliArgs creationCommand
                  <> ["--core-file", contractFilePath]
                  <> ["--metadata-file", transactionMetadataFilePath]
                  <> ["--manual-sign", txBodyEnvelopeFilePath]

            jobClientEffect :: Integration (TxBody BabbageEra)
            jobClientEffect = marloweRuntimeJobClient creationCommand

        (_, expected) <- concurrently cliEffect jobClientEffect

        (either (error . show) id -> actual) <-
          Trans.liftBase $ readFileTextEnvelope (AsTxBody (cardanoEraToAsType BabbageEra)) txBodyEnvelopeFilePath

        Trans.liftBase $ shouldBe actual expected
