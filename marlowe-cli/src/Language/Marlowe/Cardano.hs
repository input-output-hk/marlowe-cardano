{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Language.Marlowe.Cardano where

import Cardano.Api (AddressInEra (AddressInEra), LocalNodeConnectInfo (LocalNodeConnectInfo, localNodeNetworkId))
import Cardano.Api qualified as C
import Cardano.Api.Shelley qualified as CS
import Cardano.Ledger.BaseTypes qualified as LC (Network (..))
import Language.Marlowe.Core.V1.Semantics.Types.Address qualified as Marlowe

marloweNetworkFromCardanoAddress
  :: forall era
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
marloweNetworkFromLocalNodeConnectInfo LocalNodeConnectInfo{localNodeNetworkId} =
  marloweNetworkFromCaradnoNetworkId localNodeNetworkId
