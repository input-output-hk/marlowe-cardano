{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

module Language.Marlowe.Runtime.ChainSync.QueryServer where

import Cardano.Api (AnyCardanoEra (..), CardanoMode, ConsensusMode (..), ConsensusModeIsMultiEra (..), EraInMode (..),
                    GenesisParameters (..), QueryInEra (..), QueryInMode (..), QueryInShelleyBasedEra (..),
                    ShelleyBasedEra (..), toEraInMode)
import qualified Cardano.Api as Cardano
import Control.Concurrent.Async (Concurrently (Concurrently, runConcurrently))
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch)
import Control.Monad.Trans.Except (ExceptT (ExceptT), except, runExceptT, throwE, withExceptT)
import Data.Void (Void, absurd)
import Language.Marlowe.Runtime.ChainSync.Api (ChainSyncQuery (..), SlotConfig (..))
import Network.Protocol.Query.Server (QueryServer (..), ServerStInit (..), ServerStNext (..), ServerStPage (..))
import Network.Protocol.Query.Types (StNextKind (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Type (AcquireFailure)
import System.IO (hPutStrLn, stderr)

newtype RunQueryServer m = RunQueryServer (forall a. QueryServer ChainSyncQuery m a -> IO a)

data ChainSyncQueryServerDependencies = ChainSyncQueryServerDependencies
  { acceptRunQueryServer :: IO (RunQueryServer IO)
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> IO (Either AcquireFailure result)
  }

newtype ChainSyncQueryServer = ChainSyncQueryServer
  { runChainSyncQueryServer :: IO Void
  }

mkChainSyncQueryServer :: ChainSyncQueryServerDependencies -> STM ChainSyncQueryServer
mkChainSyncQueryServer ChainSyncQueryServerDependencies{..} = do
  let
    runChainSyncQueryServer = do
      runQueryServer <- acceptRunQueryServer
      Worker{..} <- atomically $ mkWorker WorkerDependencies {..}
      runConcurrently $
        Concurrently (runWorker `catch` catchWorker) *> Concurrently runChainSyncQueryServer
  pure $ ChainSyncQueryServer { runChainSyncQueryServer }

catchWorker :: SomeException -> IO ()
catchWorker = hPutStrLn stderr . ("Query worker crashed with exception: " <>) . show

data WorkerDependencies = WorkerDependencies
  { runQueryServer      :: RunQueryServer IO
  , queryLocalNodeState
      :: forall result
       . Maybe Cardano.ChainPoint
      -> QueryInMode CardanoMode result
      -> IO (Either AcquireFailure result)
  }

newtype Worker = Worker
  { runWorker :: IO ()
  }

mkWorker :: WorkerDependencies -> STM Worker
mkWorker WorkerDependencies{..} =
  let
    RunQueryServer run = runQueryServer
  in
    pure Worker { runWorker = run server }

  where
    server :: QueryServer ChainSyncQuery IO ()
    server = QueryServer $ pure $ ServerStInit \case
      GetSlotConfig        -> queryGenesisParameters extractSlotConfig
      GetSecurityParameter -> queryGenesisParameters protocolParamSecurity

    queryGenesisParameters :: (GenesisParameters -> a) -> IO (ServerStNext ChainSyncQuery 'CanReject Void () a IO ())
    queryGenesisParameters f = do
      result <- runExceptT do
        AnyCardanoEra era <- withExceptT (const ())
          $ ExceptT
          $ queryLocalNodeState Nothing
          $ QueryCurrentEra CardanoModeIsMultiEra
        eraInMode <- case toEraInMode era CardanoMode of
          Nothing        -> throwE ()
          Just eraInMode -> pure eraInMode
        shelleyBasedEra <- case eraInMode of
          ByronEraInCardanoMode   -> throwE ()
          ShelleyEraInCardanoMode -> pure ShelleyBasedEraShelley
          AllegraEraInCardanoMode -> pure ShelleyBasedEraAllegra
          MaryEraInCardanoMode    -> pure ShelleyBasedEraMary
          AlonzoEraInCardanoMode  -> pure ShelleyBasedEraAlonzo
          BabbageEraInCardanoMode -> pure ShelleyBasedEraBabbage
        result <- withExceptT (const ())
          $ ExceptT
          $ queryLocalNodeState Nothing
          $ QueryInEra eraInMode
          $ QueryInShelleyBasedEra shelleyBasedEra QueryGenesisParameters
        withExceptT (const ()) $ except result

      case result of
        Left _ -> pure $ SendMsgReject () ()
        Right genesisParameters -> pure $ SendMsgNextPage (f genesisParameters) Nothing $ ServerStPage
          { recvMsgDone = pure ()
          , recvMsgRequestNext = absurd
          }

    extractSlotConfig GenesisParameters{..} = SlotConfig protocolParamSystemStart protocolParamSlotLength
