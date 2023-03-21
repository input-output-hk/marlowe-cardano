{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
module Language.Marlowe.Cardano
  where

import Cardano.Api
  ( AddressInEra(AddressInEra)
  , CardanoMode
  , IsShelleyBasedEra
  , LocalNodeConnectInfo(LocalNodeConnectInfo, localNodeNetworkId)
  , NetworkId(Mainnet)
  , ScriptDataSupportedInEra
  )
import qualified Cardano.Api as C
import qualified Cardano.Api.Shelley as CS
import qualified Cardano.Ledger.BaseTypes as LC (Network(..))
import Cardano.Ledger.Shelley.API (Network)
import qualified Contrib.Data.Aeson.Traversals as A
import Control.Error (note)
import Control.Error.Util (hush)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader.Class (MonadReader, asks)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.OneLine as A
import Data.Bifunctor (Bifunctor(first))
import Data.Has (Has(getter))
import qualified Data.Map.Strict as M
import qualified Data.Text as Text
import GHC.Generics (Generic)
import qualified Language.Marlowe as Marlowe
import Language.Marlowe.CLI.IO (liftCli)
import Language.Marlowe.CLI.Test.CLI.Monad (runCli)
import Language.Marlowe.CLI.Test.Wallet.Types
  ( Currencies(Currencies)
  , Currency(Currency, ccCurrencySymbol)
  , CurrencyNickname(CurrencyNickname)
  , Wallet(waAddress)
  , WalletNickname(WalletNickname)
  , Wallets(Wallets)
  )
import Language.Marlowe.CLI.Types (CliError(CliError))
import qualified Language.Marlowe.Core.V1.Semantics.Types.Address as Marlowe
import Ledger.Address (toPlutusAddress)
import qualified Plutus.V1.Ledger.Value as PV

marloweNetworkFromCardanoAddress
  :: forall env era st m
   . AddressInEra era
  -> Maybe Marlowe.Network
marloweNetworkFromCardanoAddress address = do
  case address of
    AddressInEra _ (CS.ShelleyAddress network _ _) ->
      Just $ if network == LC.Mainnet then Marlowe.mainnet else Marlowe.testnet
    _ -> Nothing

marloweNetworkFromCaradnoNetworkId :: C.NetworkId -> Marlowe.Network
marloweNetworkFromCaradnoNetworkId networkId =
  if networkId == C.Mainnet then Marlowe.mainnet else Marlowe.testnet

marloweNetworkFromLocalNodeConnectInfo
  :: LocalNodeConnectInfo mode
  -> Marlowe.Network
marloweNetworkFromLocalNodeConnectInfo LocalNodeConnectInfo {localNodeNetworkId} =
  marloweNetworkFromCaradnoNetworkId localNodeNetworkId

