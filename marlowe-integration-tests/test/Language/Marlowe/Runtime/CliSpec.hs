{--}

module Language.Marlowe.Runtime.CliSpec
  where

import qualified System.IO
import qualified System.Process
-- import Test.Hspec (Spec)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)
-- import Language.Marlowe.Runtime.Integration.Common (runIntegrationTest, getGenesisWallet)
-- import qualified Control.Monad.Trans as Trans

-- https://hackage.haskell.org/package/process-1.6.17.0/docs/System-Process.html

marloweRuntimeCreateProcess :: IO (Maybe System.IO.Handle, Maybe System.IO.Handle, Maybe System.IO.Handle, System.Process.ProcessHandle)
marloweRuntimeCreateProcess =
  System.Process.createProcess
    ( System.Process.proc
        "cabal"
        [ "run",
          "marlowe",
          "--",
          "create"
          -- // TODO fill in runtime create arguments here
          -- --change-address ADDRESS
          -- [-a|--address ADDRESS]
          -- [--collateral-utxo UTXO]
          -- --manual-sign FILE_PATH
          -- [-m|--metadata-file FILE_PATH]
          -- [--v1]
          -- [(-r|--role ROLE=ADDRESS) | --roles-config-file FILE_PATH | --role-token-policy-id POLICY_ID]
          -- (--core-file FILE_PATH | --contract-file FILE_PATH [--args-file FILE_PATH | [--timeout-arg NAME=POSIX_TIMESTAMP] [--value-arg NAME=INTEGER]])
          -- --min-utxo LOVELACE
        ]
    )
      { System.Process.cwd = Just "../marlowe-runtime",
        System.Process.std_out = System.Process.CreatePipe
      }

-- spec :: Spec
-- spec =  pure ()

spec :: Spec
spec =  describe "hey ho" do
  it "does stuff" do
    withLocalMarloweRuntime \_ -> do
      5 `shouldBe` (4 :: Int)

  -- it "does stuff" do
  --   withLocalMarloweRuntime $ runIntegrationTest do
  --     _ <- getGenesisWallet 0
  --     Trans.lift do
  --       5 `shouldBe` (4 :: Int)

{-
cabal run marlowe-integration-tests
cabal run marlowe-integration-tests -- --match Integrations
  --flag=-Wnoall
cabal run --help
-}

{-
https://github.com/input-output-hk/marlowe-cardano/blob/main/marlowe-integration/src/Test/Integration/Marlowe/Local.hs

data MarloweRuntime = MarloweRuntime
  { runChainSyncQueryClient :: RunClient IO (QueryClient ChainSyncQuery)
  , runChainSeekClient :: RunClient IO RuntimeChainSeekClient
  , runDiscoverySyncClient :: RunClient IO MarloweHeaderSyncClient
  , runHistorySyncClient :: RunClient IO MarloweSyncClient
  , runTxJobClient :: RunClient IO (JobClient MarloweTxCommand)
  , runMarloweQueryClient :: RunClient IO MarloweQueryClient
  , runWebClient :: forall a. ClientM a -> IO (Either ClientError a)
  , marloweScripts :: MarloweScripts
  , testnet :: LocalTestnet
  }
-}
