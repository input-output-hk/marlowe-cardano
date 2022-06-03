{-# LANGUAGE BlockArguments            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DerivingStrategies        #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
module History.Database where

import ChainSync.Database (ChainSyncQueryChan)
import Control.Distributed.Process (Closure, Process, SendPort, newChan, receiveChan, say, sendChan)
import Control.Distributed.Process.Closure (mkClosure, remotable)
import Control.Monad (foldM, unless)
import Control.Monad.IO.Class (liftIO)
import Data.Binary (Binary, decode, decodeOrFail, encode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Data (Typeable)
import Data.Foldable (fold, foldl')
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Traversable (forM)
import GHC.Generics (Generic)
import Language.Marlowe.Runtime.Chain.Types (MarloweBlockHeader (..), MarloweChainPoint (..),
                                             MarlowePolicyId (unMarlowePolicyId), headerPoint, slotToIntegral)
import Language.Marlowe.Runtime.History.Types (ContractCreationTxOut (..), ContractId (..), Event (..),
                                               HistoryEvent (..))
import System.Directory (createDirectory, doesDirectoryExist, doesPathExist, renamePath)

data HistoryDatabaseDependencies = HistoryDatabaseDependencies
  { initDbChan  :: SendPort HistoryQueryChan
  , chainDbChan :: ChainSyncQueryChan
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data HistoryQuery
  = GetIntersectionPoint (SendPort MarloweChainPoint)
  | GetHistory ContractId (SendPort (Maybe History))
  | GetContractIds (SendPort (Set ContractId))
  | AddEvents [Event]
  | RollBackward MarloweChainPoint
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

type HistoryQueryChan = SendPort HistoryQuery

data History = History
  { creationTxOut :: ContractCreationTxOut
  , events        :: [Event]
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass Binary

data State = State
  { histories         :: Map ContractId History
  , creationSlotIndex :: IntMap (Set ContractId)
  }
  deriving (Generic, Typeable, Show, Eq)
  deriving anyclass (Binary)

getIntersectionPoint :: HistoryQueryChan -> Process MarloweChainPoint
getIntersectionPoint sendQuery = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetIntersectionPoint sendPort
  receiveChan receiveResponse

getHistory :: HistoryQueryChan -> ContractId -> Process (Maybe History)
getHistory sendQuery contractId = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetHistory contractId sendPort
  receiveChan receiveResponse

getContractIds :: HistoryQueryChan -> Process (Set ContractId)
getContractIds sendQuery = do
  (sendPort, receiveResponse) <- newChan
  sendChan sendQuery $ GetContractIds sendPort
  receiveChan receiveResponse

addEvents :: HistoryQueryChan -> [Event] -> Process ()
addEvents sendQuery = sendChan sendQuery . AddEvents

rollBackward :: HistoryQueryChan -> MarloweChainPoint -> Process ()
rollBackward sendQuery = sendChan sendQuery . RollBackward

dir :: FilePath
dir = "./history-db"

contractFile :: ContractId -> FilePath
contractFile contractId = dir <> "/history-" <> read (show $ unMarlowePolicyId $ currencySymbol contractId)

slotIndexFile :: FilePath
slotIndexFile = dir <> "/slotIndex"

writeFrequency :: Integer
writeFrequency = 500

historyDatabase :: HistoryDatabaseDependencies -> Process ()
historyDatabase HistoryDatabaseDependencies{..} = do
  (sendQuery, receiveQuery) <- newChan
  sendChan initDbChan sendQuery
  initialState <- liftIO do
    directoryExists <- doesDirectoryExist dir
    unless directoryExists do
      createDirectory dir
      LBS.writeFile slotIndexFile $ encode $ creationSlotIndex emptyState
    slotIndexBytes <- LBS.fromStrict <$> BS.readFile slotIndexFile
    let slotIndex = decode slotIndexBytes
    histories <- Map.fromList . catMaybes <$> forM (Set.toList $ fold slotIndex) \contractId -> do
      let file = contractFile contractId
      exists <- doesPathExist file
      if exists then do
        historyBytes <- LBS.fromStrict <$> BS.readFile (contractFile contractId)
        pure case decodeOrFail historyBytes of
          Left _        -> Nothing
          Right (_,_,a) -> Just (contractId, a)
      else do
        putStrLn $ "file not found for contract" <> show contractId
        pure Nothing
    pure $ State histories slotIndex
  say $ "loaded " <> show (Map.size $ histories initialState) <> " contracts"
  let go state = go =<< handleQuery state =<< receiveChan receiveQuery
  go initialState
    where
      emptyState = State
        { histories = mempty
        , creationSlotIndex = mempty
        }

      handleQuery state@State{..} = \case
        GetIntersectionPoint sendResponse  -> state <$ sendChan sendResponse (foldl' max MarloweChainPointAtGenesis $ fmap (foldr (max . headerPoint . blockHeader) MarloweChainPointAtGenesis. events) $ Map.elems histories)
        GetHistory contractId sendResponse -> state <$ sendChan sendResponse (Map.lookup contractId histories)
        GetContractIds sendResponse        -> state <$ sendChan sendResponse (Map.keysSet histories)
        AddEvents []                       -> pure state
        AddEvents events                   -> saveSlotIndex =<< saveChangedHistories histories =<< (\s -> foldM handleAddEvent s events) =<< foldM handleAddCreation state events
        RollBackward point                 -> saveSlotIndex $ handleRollBackward state point

      handleAddCreation state@State{..} Event{ historyEvent = ContractWasCreated creation@ContractCreationTxOut{..} } =
        case Map.lookup contractId histories of
          Nothing -> do
            let MarloweBlockHeader slot _ _ = header
            pure State
              { histories = Map.insert contractId (History creation []) histories
              , creationSlotIndex = IntMap.insertWith (<>) (slotToIntegral slot) (Set.singleton contractId) creationSlotIndex
              }
          Just _ -> do
            say "Contract history continued with a creation event"
            pure state
      handleAddCreation state _ = pure state

      handleAddEvent state@State{..} event@Event{..} =
        case Map.lookup contractId histories of
          Nothing -> do
            say "Contract history started with a non-creation event"
            pure state
          Just history@History{..} -> pure state { histories = Map.insert contractId history { events = event : events } histories }

      handleRollBackward _ MarloweChainPointAtGenesis = emptyState
      handleRollBackward State{..} (MarloweChainPoint slot _) =
        let
          headerSlot (MarloweBlockHeader s _ _) = s
          rollbackHistory h@History{events} = h { events = filter ((<= slot) . headerSlot . blockHeader) events }
          (createdBefore, createdAfter) = break ((> slotToIntegral slot) . fst) $ IntMap.toAscList creationSlotIndex
        in
          State
            { histories = Map.filter (not . null . events) $ fmap rollbackHistory $ Map.withoutKeys histories $ foldMap snd createdAfter
            , creationSlotIndex = IntMap.fromDistinctAscList createdBefore
            }

      saveChangedHistories oldHistories state = do
        let changedHistories = Map.mapMaybe id $ Map.intersectionWith (\old new -> if old == new then Nothing else Just new) oldHistories $ histories state
        let newHistories = Map.difference (histories state) oldHistories
        say $ "saving contracts. " <> show (Map.size $ histories state) <> " total, " <> show (Map.size changedHistories) <> " changed, " <> show (Map.size newHistories) <> " added"
        let saveHistory contractId = overwrite (contractFile contractId)
        _ <- liftIO $ Map.traverseWithKey saveHistory $ Map.union newHistories changedHistories
        pure state

      overwrite path a = do
        let tmpPath = path <> "-tmp"
        LBS.writeFile tmpPath $ encode a
        renamePath tmpPath path

      saveSlotIndex state = liftIO do
        overwrite slotIndexFile $ creationSlotIndex state
        pure state

remotable ['historyDatabase]

process :: HistoryDatabaseDependencies -> Closure (Process ())
process = $(mkClosure 'historyDatabase)
