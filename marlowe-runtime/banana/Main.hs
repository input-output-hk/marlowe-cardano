{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TupleSections  #-}
module Main where

import Cardano.Api
import Data.IORef (newIORef, readIORef, writeIORef)
import Language.Marlowe.Runtime.Chain (marloweChainSyncClient, runMarloweChainSyncClient)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainEvent (..), MarloweChainPoint,
                                             MarloweChainSyncClient, MarloweChainTip (..))
import Reactive.Banana (Behavior, Event, compile, filterJust, mapAccum, valueB)
import Reactive.Banana.Frameworks (MomentIO, actuate, changes, liftIO, newEvent, reactimate, reactimate')

main :: IO ()
main = do
  getClientRef <- newIORef $ error "not initialized"

  network <- compile do
    (getClient, eStart, eEvent, eDone) <- frpChainSyncClient [] (pure True)
    liftIO $ writeIORef getClientRef getClient
    runLoggers eStart eEvent eDone

  actuate network
  getClient <- readIORef getClientRef

  let
    connectionInfo = LocalNodeConnectInfo
      { localConsensusModeParams = CardanoModeParams $ EpochSlots 21600
      , localNodeNetworkId = Testnet $ NetworkMagic 1566
      , localNodeSocketPath = "/var/lib/containers/storage/volumes/marlowe-dashboard-client_cardano-ipc/_data/node.socket"
      }

  result <- runMarloweChainSyncClient connectionInfo getClient
  either print id result


frpChainSyncClient
  :: [ChainPoint]
  -> Behavior  Bool
  -> MomentIO (GenesisParameters -> IO (MarloweChainSyncClient IO ()), Event (Maybe MarloweChainPoint, MarloweChainTip), Event MarloweChainEvent, Event ())
frpChainSyncClient startingPoints bShutdown = do
  shutdown <- valueB bShutdown
  shutdownRef <- liftIO $ newIORef shutdown
  (eStart, fireStart) <- newEvent
  (eEvent, fireEvent) <- newEvent
  (eDone, fireDone) <- newEvent

  let
    getClient genesisParams = liftIO $ marloweChainSyncClient
      startingPoints
      genesisParams
      (curry fireStart)
      (readIORef shutdownRef)
      fireEvent
      (fireDone ())

  let
    setShutdown futureShutdown = do
      shutdown' <- futureShutdown
      pure $ writeIORef shutdownRef shutdown'

  shutdownChanges <- changes bShutdown
  reactimate' $ setShutdown <$> shutdownChanges

  pure (getClient, eStart, eEvent, eDone)

runLoggers
  :: Event (Maybe MarloweChainPoint, MarloweChainTip)
  -> Event MarloweChainEvent
  -> Event ()
  -> MomentIO ()
runLoggers eStart eEvent eDone= do
  let
    showEvent (MarloweRollBackward point _) = "rollback to " <> show point
    showEvent (MarloweRollForward (MarloweBlockHeader slot _ block) _ (MarloweChainTip tipSlot _ _)) =
      "roll forward to " <> show slot <> " " <> show block <> " Tip: " <> show tipSlot
    showEvent (MarloweRollForward (MarloweBlockHeader slot _ block) _ MarloweChainTipAtGenesis) =
      "roll forward to " <> show slot <> " " <> show block <> " Tip: Genesis"
  (eThrottled, _) <- mapAccum (0 :: Integer) $
    (\e n -> if n == 1000 then (Just e, 0) else (Nothing, n + 1)) <$> eEvent
  let eStartLog = ("Starting",) . Just . show <$> eStart
  let eEventLog = ("ProcessingEvent",) . Just . showEvent <$> filterJust eThrottled
  let eDoneLog = ("Done", Nothing) <$ eDone
  let eLog = mconcat [eStartLog, eEventLog, eDoneLog]
  let
    printLog (prefix, Nothing)  = putStrLn prefix
    printLog (prefix, Just msg) = putStrLn $ prefix <> ": " <> msg
  reactimate $ printLog <$> eLog
