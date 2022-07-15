module Main where

import Cardano.Api (Block (Block), BlockHeader (..), BlockInMode (BlockInMode), ConsensusModeParams (..),
                    EpochSlots (..), LocalNodeConnectInfo (..), NetworkId (..), NetworkMagic (..))
import Language.Marlowe.Runtime.ChainSync.NodeClient (ChainSyncEvent (..), runNodeClient)
import Ouroboros.Network.Point (WithOrigin (..))

main :: IO ()
main = do
  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = Testnet $ NetworkMagic 1566
      , localNodeSocketPath = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
      }
    getHeaderAtPoint _ = pure Origin
    getIntersectionPoints _ _ = pure []
  runNodeClient connectionInfo getHeaderAtPoint getIntersectionPoints \case
    RollForward (BlockInMode (Block header _) _) _ -> putStrLn (showHeader header)
    RollBackward _ _                               -> putStrLn "rollback"

showHeader :: BlockHeader -> String
showHeader (BlockHeader slotNo hash blockNo) =
  "BlockHeader "
    <> "(" <> show slotNo <> ")"
    <> "(" <> show hash <> ")"
    <> "(" <> show blockNo <> ")"
