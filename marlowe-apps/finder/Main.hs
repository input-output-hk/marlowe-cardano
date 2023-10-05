{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (
  main,
) where

import Language.Marlowe.Runtime.App.Parser (getConfigParser)
import Language.Marlowe.Runtime.App.Types (
  Config,
  FinishOnWait (..),
 )

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Aeson (ToJSON (..))
import Data.Foldable (fold, for_)
import qualified Data.Map as Map
import Data.Time.Units (Second, TimeUnit (toMicroseconds))
import Language.Marlowe.Protocol.BulkSync.Client hiding (runMarloweBulkSyncClient)
import Language.Marlowe.Runtime.App.Run (runClientWithConfig)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader (..), TxOutRef (..))
import Language.Marlowe.Runtime.Client (runMarloweBulkSyncClient)
import Language.Marlowe.Runtime.Core.Api (MarloweVersion (..), Transaction (..))
import Language.Marlowe.Runtime.History.Api (
  CreateStep (..),
  MarloweApplyInputsTransaction (..),
  MarloweBlock (..),
  MarloweCreateTransaction (..),
  MarloweWithdrawTransaction (..),
  SomeCreateStep (..),
 )
import Observe.Event.Dynamic
import Observe.Event.Explicit (addField, withEvent)
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON (defaultRenderSelectorJSON), SomeJSONException)
import Observe.Event.Render.JSON.Handle (jsonHandleBackend)
import qualified Options.Applicative as O
import System.IO (stdout)

main :: IO ()
main = commandParser >>= O.execParser >>= runFinder

runFinder :: Command -> IO ()
runFinder Command{..} = do
  backend <- jsonHandleBackend stdout (toJSON @SomeJSONException) defaultRenderSelectorJSON
  let idle = SendMsgRequestNext 255 next
      next =
        ClientStNext
          { recvMsgRollForward = \blocks tip -> do
              for_ blocks \MarloweBlock{..} -> liftIO do
                let BlockHeader{..} = blockHeader
                withEvent backend (DynamicEventSelector "NewBlock") \ev -> do
                  addField ev $ DynamicField "blockNo" $ toJSON blockNo
                  addField ev $ DynamicField "slotNo" $ toJSON slotNo
                  addField ev $ DynamicField "blockHeaderHash" $ toJSON headerHash
                  addField ev $ DynamicField "tip" $ toJSON tip
                for_ createTransactions \MarloweCreateTransaction{..} ->
                  for_ @_ @_ @_ @() (Map.toAscList newContracts) \(txIx, SomeCreateStep MarloweV1 CreateStep{..}) ->
                    withEvent backend (DynamicEventSelector "NewContract") \ev -> do
                      addField ev $ DynamicField "contractId" $ toJSON $ TxOutRef txId txIx
                      addField ev $ DynamicField "marloweVersion" $ toJSON MarloweV1
                      addField ev $ DynamicField "marloweOutput" $ toJSON createOutput
                      addField ev $ DynamicField "metadata" $ toJSON metadata
                      addField ev $ DynamicField "payoutValidatorHash" $ toJSON payoutValidatorHash
                for_ @_ @_ @_ @() applyInputsTransactions \MarloweApplyInputsTransaction{..} -> do
                  MarloweV1 <- pure marloweVersion
                  let Transaction{blockHeader = _, ..} = marloweTransaction
                  withEvent backend (DynamicEventSelector "InputsApplied") \ev -> do
                    addField ev $ DynamicField "contractId" $ toJSON contractId
                    addField ev $ DynamicField "marloweVersion" $ toJSON MarloweV1
                    addField ev $ DynamicField "transactionId" $ toJSON transactionId
                    addField ev $ DynamicField "metadata" $ toJSON metadata
                    addField ev $ DynamicField "validityLowerBound" $ toJSON validityLowerBound
                    addField ev $ DynamicField "validityUpperBound" $ toJSON validityUpperBound
                    addField ev $ DynamicField "previousOutput" $ toJSON marloweInput
                    addField ev $ DynamicField "inputs" $ toJSON inputs
                    addField ev $ DynamicField "output" $ toJSON output
                for_ withdrawTransactions \MarloweWithdrawTransaction{..} ->
                  withEvent backend (DynamicEventSelector "PayoutsWithdrawn") \ev -> do
                    addField ev $ DynamicField "transactionId" $ toJSON consumingTx
                    addField ev $ DynamicField "payoutsWithdrawn" $ toJSON $ fold consumedPayouts
              pure idle
          , recvMsgRollBackward = \point tip -> liftIO do
              withEvent backend (DynamicEventSelector "RolledBack") \ev -> do
                addField ev $ DynamicField "newPoint" $ toJSON point
                addField ev $ DynamicField "tip" $ toJSON tip
              pure idle
          , recvMsgWait =
              if unFinishOnWait endOnWait
                then pure $ SendMsgCancel $ SendMsgDone ()
                else liftIO do
                  threadDelay $ fromIntegral $ toMicroseconds pollingFrequency
                  pure $ SendMsgPoll next
          }
  runClientWithConfig config $ runMarloweBulkSyncClient $ MarloweBulkSyncClient $ pure idle

data Command = Command
  { config :: Config
  , pollingFrequency :: Second
  , endOnWait :: FinishOnWait
  }
  deriving (Show)

commandParser :: IO (O.ParserInfo Command)
commandParser =
  do
    configParser <- getConfigParser
    let commandOptions =
          Command
            <$> configParser
            <*> fmap
              fromInteger
              ( O.option
                  O.auto
                  (O.long "polling" <> O.value 5 <> O.metavar "SECONDS" <> O.help "The polling frequency for waiting on Marlowe Runtime.")
              )
            <*> O.flag
              (FinishOnWait False)
              (FinishOnWait True)
              (O.long "end-at-tip" <> O.help "Stop the process when the tip of all contracts has been reached.")
    pure $
      O.info
        (O.helper <*> commandOptions)
        ( O.fullDesc
            <> O.progDesc "This command-line tool watches the blockchain for Marlowe contract transactions."
            <> O.header "marlowe-finder : find Marlowe contract transactions"
        )
