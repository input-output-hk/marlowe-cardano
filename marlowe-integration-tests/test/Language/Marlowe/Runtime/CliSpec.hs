module Language.Marlowe.Runtime.CliSpec
  where

import qualified Control.Monad.Reader as Reader
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import qualified Data.Text as Text
import Language.Marlowe.Runtime.ChainSync.Api (Address(..), toBech32)
import Language.Marlowe.Runtime.Integration.Common (Wallet(..), getGenesisWallet, runIntegrationTest)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import Test.Hspec (Spec, describe, fdescribe, it)
import Test.Integration.Marlowe.Local (execMarlowe_, marloweSyncPort, txJobPort, withLocalMarloweRuntime)

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

serializeAddress :: Address -> String
serializeAddress = Text.unpack . Maybe.fromJust . toBech32

spec :: Spec
spec = fdescribe "Marlowe runtime CLI" do
  describe "create" do
    it "works" do
      -- // TODO do not accidentally commit this!!!!!!:
      let contractName = "./temporary-close-contract"
          txEnvelope = "./tx-body-1234567.envelope"
      writeFile contractName "\"close\""

      withLocalMarloweRuntime $ runIntegrationTest do
        Wallet {addresses = WalletAddresses {changeAddress, extraAddresses}} <- getGenesisWallet 0

        marlowe_sync_port :: Int <- Reader.asks marloweSyncPort
        tx_command_port :: Int <- Reader.asks txJobPort

        execMarlowe_ $
          concat
            [ ["create", "--change-address", serializeAddress changeAddress],
              do address <- Set.toList extraAddresses; ["--address", serializeAddress address],
              ["--manual-sign", txEnvelope],
              ["--core-file", contractName],
              ["--min-utxo", show @Int 2_000_000],
              ["--marlowe-sync-port", show marlowe_sync_port],
              ["--tx-command-port", show tx_command_port]
            ]

{-
bash ./marlowe-integration-tests/marlowe --help
cabal run marlowe-integration-tests
-}
