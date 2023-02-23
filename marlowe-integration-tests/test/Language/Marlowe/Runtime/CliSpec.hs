module Language.Marlowe.Runtime.CliSpec
  where

import Control.Monad (void)
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), toBech32)
import Language.Marlowe.Runtime.Integration.Common (Wallet(..), getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import Test.Hspec (Spec, describe, fdescribe, it)
import Test.Integration.Marlowe.Local
  ( LocalTestnet(..)
  , Workspace(Workspace, workspaceDir)
  , execMarlowe_
  , marloweSyncPort
  , testnet
  , txJobPort
  , withLocalMarloweRuntime
  )

serializeAddress :: Address -> String
serializeAddress = Text.unpack . Maybe.fromJust . toBech32

{-
bash ./marlowe-integration-tests/marlowe --help
cabal run marlowe-integration-tests
-}

spec :: Spec
spec = fdescribe "Marlowe runtime CLI" do
  describe "create" do
    it "creates a tx body envelope" $
      withLocalMarloweRuntime $ runIntegrationTest do
        Wallet {addresses = WalletAddresses {changeAddress, extraAddresses}} <- getGenesisWallet 0

        LocalTestnet {workspace = Workspace {workspaceDir}} <- Reader.asks testnet

        let contractFilePath = workspaceDir <> "/close-contract"
            txBodyEnvelopeFilePath = workspaceDir <> "/tx-body.envelope"

        Trans.lift $ writeFile contractFilePath "\"close\""

        marlowe_sync_port :: Int <- Reader.asks marloweSyncPort
        tx_command_port :: Int <- Reader.asks txJobPort

        execMarlowe_ $
          concat
            [ ["create", "--change-address", serializeAddress changeAddress],
              do address <- Set.toList extraAddresses; ["--address", serializeAddress address],
              ["--manual-sign", txBodyEnvelopeFilePath],
              ["--core-file", contractFilePath],
              ["--min-utxo", show @Int 2_000_000],
              ["--marlowe-sync-port", show marlowe_sync_port],
              ["--tx-command-port", show tx_command_port]
            ]

        void $ Trans.lift $ readFile txBodyEnvelopeFilePath
