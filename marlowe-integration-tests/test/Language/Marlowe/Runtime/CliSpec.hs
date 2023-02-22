{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.Marlowe.Runtime.CliSpec
  where

import qualified Control.Monad as Control
import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans as Trans
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), toBech32)
import Language.Marlowe.Runtime.Integration.Common (Integration, Wallet(..), getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import System.Exit (ExitCode(ExitSuccess))
import qualified System.Process
import Test.Hspec (Spec, fdescribe, it, shouldBe)
import Test.Integration.Marlowe.Local
  (marloweHeaderSyncPort, marloweQueryPort, marloweSyncPort, txJobPort, withLocalMarloweRuntime)

-- import qualified Control.Monad.Trans as Trans

-- https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html

marloweRuntimeCreateProcess :: Wallet -> FilePath -> FilePath -> Integration (ExitCode, String, String)
marloweRuntimeCreateProcess Wallet {addresses = WalletAddresses {changeAddress, extraAddresses}} contractFileName outputFileName = do
  history_sync_port :: Int <- Reader.asks marloweSyncPort
  tx_command_port :: Int <- Reader.asks txJobPort

  let cabalRunMarloweCreate :: System.Process.CreateProcess
      cabalRunMarloweCreate =
        System.Process.proc "cabal" $
          concat
            [ ["run", "marlowe-runtime:marlowe", "--", "create", "--change-address", serializeAddress changeAddress],
              do address <- Set.toList extraAddresses; ["--address", serializeAddress address],
              ["--manual-sign", outputFileName],
              ["--core-file", contractFileName],
              ["--min-utxo", show @Int 2_000_000],
              ["--history-sync-port", show history_sync_port],
              ["--tx-command-port", show tx_command_port]
            ]

  Trans.lift $ System.Process.readCreateProcessWithExitCode cabalRunMarloweCreate ""
  where
    serializeAddress :: Address -> String
    serializeAddress = Text.unpack . Maybe.fromJust . toBech32

-- newtype Address = Address { unAddress :: ByteString }
--   deriving stock (Eq, Ord, Generic)

-- data WalletAddresses = WalletAddresses
--   { changeAddress  :: Address
--   , extraAddresses :: Set Address
--   , collateralUtxos :: Set TxOutRef
--   }
--   deriving (Eq, Show, Generic, Binary, ToJSON)

-- data Wallet = Wallet
--   { addresses :: WalletAddresses
--   , signingKeys :: [ShelleyWitnessSigningKey]
--   }

{-
https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-integration/src/Test/Integration/Marlowe/Local.hs
https://github.com/input-output-hk/marlowe-cardano/blob/jhbertra/connection-refactor/marlowe-integration/src/Test/Integration/Marlowe/Local.hs
data MarloweRuntime = MarloweRuntime
  { marloweHeaderSyncConnector :: SomeClientConnector MarloweHeaderSyncClient IO
  , marloweSyncConnector :: SomeClientConnector MarloweSyncClient IO
  , marloweQueryConnector :: SomeClientConnector MarloweQueryClient IO
  , txJobConnector :: SomeClientConnector (JobClient MarloweTxCommand) IO
  , marloweHeaderSyncPort :: Int
  , marloweSyncPort :: Int
  , marloweQueryPort :: Int
  , txJobPort :: Int
  , runWebClient :: forall a. ClientM a -> IO (Either ClientError a)
  , marloweScripts :: MarloweScripts
  , testnet :: LocalTestnet
  }
-}

{-
https://github.com/input-output-hk/marlowe-cardano/blob/0adc6de60c3d6b746fafa1497635b7f39aa5bdb8/marlowe-runtime/discovery-api/Language/Marlowe/Protocol/HeaderSync/Client.hs
-}

spec :: Spec
spec = fdescribe "Marlowe runtime CLI" do
  it "cabal run marlowe-runtime:marlowe -- create ..." do
    -- // TODO do not accidentally commit this!!!!!!:
    let contractName = "./temporary-close-contract"
        txEnvelope = "./tx-body-1234567.envelope"
    writeFile contractName "\"close\""

    withLocalMarloweRuntime $ runIntegrationTest do
      wallet :: Wallet <- getGenesisWallet 0

      -- a <- Reader.asks marloweHeaderSyncPort
      -- b <- Reader.asks marloweSyncPort
      -- c <- Reader.asks marloweQueryPort
      -- d <- Reader.asks txJobPort

      -- Trans.lift do
      --   putStrLn $ "marloweHeaderSyncPort: " <> show a
      --   putStrLn $ "marloweSyncPort: " <> show b
      --   putStrLn $ "marloweQueryPort: " <> show c
      --   putStrLn $ "txJobPort: " <> show d

      (exitCode, stdout, stderr) <- marloweRuntimeCreateProcess wallet contractName txEnvelope
      Trans.lift do
        Control.unless (exitCode == ExitSuccess) do
          putStrLn $ "Standard out:\n" <> stdout
          putStrLn $ "Standard error:\n" <> stderr
          exitCode `shouldBe` ExitSuccess

{-
cabal run marlowe-runtime:marlowe -- --help
cabal run marlowe-integration-tests
grep "SomeClientConnector" -R --include=*.hs
grep "withLocalMarloweRuntime" -R --include=*.hs
-}
