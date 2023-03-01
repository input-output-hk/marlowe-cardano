{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Marlowe.Runtime.CliSpec
  where

import Cardano.Api (AsType(AsTxBody), BabbageEra, CardanoEra(BabbageEra), TxBody, readFileTextEnvelope)
import Control.Concurrent.Async.Lifted (concurrently)
import qualified Control.Monad.Base as Trans
import qualified Control.Monad.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.Either as Either
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Void (Void)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import Language.Marlowe.Runtime.Cardano.Api (cardanoEraToAsType)
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), Lovelace(Lovelace), TransactionMetadata, toBech32)
import Language.Marlowe.Runtime.Client (runMarloweTxClient)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion(MarloweV1), MarloweVersionTag(V1), contractToJSON)
import Language.Marlowe.Runtime.Integration.Common (Integration, Wallet(..), getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Transaction.Api
  (ContractCreated(..), CreateError, MarloweTxCommand(..), RoleTokensConfig(RoleTokensNone), WalletAddresses(..))
import qualified Network.Protocol.Job.Client as JobClient
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local
  ( LocalTestnet(..)
  , Workspace(Workspace, workspaceDir)
  , execMarlowe_
  , marloweSyncPort
  , testnet
  , withLocalMarloweRuntime
  )

serializeAddress :: Address -> String
serializeAddress = Text.unpack . Maybe.fromJust . toBech32

toCliArgs :: MarloweTxCommand Void (CreateError v) (ContractCreated BabbageEra v) -> [String]
toCliArgs (Create _ _ WalletAddresses {changeAddress, extraAddresses} _ _ (Lovelace minUTXO) _) =
  ["create", "--change-address", serializeAddress changeAddress]
    <> do address <- Set.toList extraAddresses; ["--address", serializeAddress address]
    <> ["--min-utxo", show minUTXO]

marloweRuntimeCli :: [String] -> Integration ()
marloweRuntimeCli cliArgs = do
  marlowe_sync_port :: Int <- Reader.asks marloweSyncPort
  execMarlowe_ $
    ["--marlowe-runtime-port", show marlowe_sync_port]
      <> cliArgs

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

        LocalTestnet {workspace = Workspace {workspaceDir}} <- Reader.asks testnet

        let roleTokensConfig :: RoleTokensConfig
            roleTokensConfig = RoleTokensNone

            transactionMetadata :: TransactionMetadata
            transactionMetadata = mempty

            contract :: V1.Contract
            contract = V1.Close

            transactionMetadataFilePath :: FilePath
            transactionMetadataFilePath = workspaceDir <> "/transaction-metadata"

            contractFilePath :: FilePath
            contractFilePath = workspaceDir <> "/close-contract"

            txBodyEnvelopeFilePath :: FilePath
            txBodyEnvelopeFilePath = workspaceDir <> "/tx-body.envelope"

        Trans.liftBase do
          Aeson.encodeFile contractFilePath $ contractToJSON MarloweV1 contract
          Aeson.encodeFile transactionMetadataFilePath transactionMetadata

        let creationCommand :: MarloweTxCommand Void (CreateError 'V1) (ContractCreated BabbageEra 'V1)
            creationCommand =
              Create
                Nothing
                MarloweV1
                addresses
                roleTokensConfig
                transactionMetadata
                (Lovelace 2_000_000)
                contract

            cliEffect :: Integration ()
            cliEffect =
              marloweRuntimeCli $
                toCliArgs creationCommand
                  <> ["--core-file", contractFilePath]
                  <> ["--metadata-file", transactionMetadataFilePath]
                  <> ["--manual-sign", txBodyEnvelopeFilePath]

            jobClientEffect :: Integration (TxBody BabbageEra)
            jobClientEffect = marloweRuntimeJobClient creationCommand

        (_, expected) <- concurrently cliEffect jobClientEffect

        (Either.fromRight (error "Some Runtime CLI creation error!") -> actual) <-
          Trans.liftBase $ readFileTextEnvelope (AsTxBody (cardanoEraToAsType BabbageEra)) txBodyEnvelopeFilePath

        Trans.liftBase $ shouldBe actual expected
