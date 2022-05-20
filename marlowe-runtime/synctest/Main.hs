module Main where

import Cardano.Api
import Language.Marlowe.Runtime.Chain (runMarloweChainSyncClient, stdOutMarloweChainSyncClient)

main :: IO ()
main = do
  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = Testnet $ NetworkMagic 1566
      , localNodeSocketPath = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
      }

  result <- runMarloweChainSyncClient connectionInfo (stdOutMarloweChainSyncClient [])
  either print id result
