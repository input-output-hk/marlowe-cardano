

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}


module Main
  ( main
  ) where


import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (replicateM_, void)
import Control.Monad.Except (MonadIO, liftIO, runExceptT, throwError)
import Data.List.Split (chunksOf)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
  ( Action(..)
  , Bound(Bound)
  , Case(..)
  , ChoiceId(..)
  , Contract(..)
  , Input(NormalInput)
  , InputContent(..)
  , Observation(TrueObs)
  , Party(Address)
  , Token(..)
  , Value(ChoiceValue)
  )
import Language.Marlowe.Core.V1.Semantics.Types.Address (deserialiseAddress)
import Language.Marlowe.Runtime.App.Transact
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress), fromBech32)
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Observe.Event (EventBackend, addField, hoistEvent, withEvent)
import Observe.Event.Dynamic (DynamicEvent, DynamicEventSelector(..))
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (JSONRef, simpleJsonStderrBackend)
import Observe.Event.Syntax ((≔))
import Plutus.V2.Ledger.Api (POSIXTime(..))
import System.Environment (getArgs)
import System.Random (randomRIO)

import qualified Cardano.Api as C
  (AsType(AsPaymentExtendedKey, AsSigningKey), PaymentExtendedKey, SigningKey, readFileTextEnvelope)
import qualified Data.Text as T (Text, pack)
import qualified Data.Time.Clock.POSIX as P (getPOSIXTime)


makeContract
  :: POSIXTime
  -> Party
  -> Contract
makeContract timeout party =
  When
    [
      Case (Choice (ChoiceId "Amount" party) [Bound 1 2000000])
        $ When
        [
           Case (Deposit party party (Token "" "") (ChoiceValue $ ChoiceId "Amount" party))
             $ When
             [
               Case (Notify TrueObs)
                 Close
             ]
             timeout
             Close
        ]
        timeout
        Close
    ]
    timeout
    Close


makeInputs
  :: Party
  -> Integer
  -> [InputContent]
makeInputs party amount =
  [
    IChoice (ChoiceId "Amount" party) amount
  , IDeposit party party (Token "" "") amount
  , INotify
  ]


randomInputs
  :: MonadIO m
  => Party
  -> m [InputContent]
randomInputs = (<$> randomRIO (1_000_000, 2_000_000)) . makeInputs


runScenario
  :: DynamicEvent App JSONRef
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [InputContent]
  -> App ContractId
runScenario event config address key contract inputs =
  runWithEvents event config address key contract (pure . NormalInput <$> inputs) 1_500_000


currentTime :: MonadIO m => m POSIXTime
currentTime = POSIXTime . floor . (* 1000) . nominalDiffTimeToSeconds <$> liftIO P.getPOSIXTime


runOne
  :: EventBackend IO JSONRef DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> IO ()
runOne eventBackend config address key =
  withEvent eventBackend (DynamicEventSelector "Contract")
    $ \event ->
      do
        threadId <- myThreadId
        addField event $ ("threadId" :: T.Text) ≔ show threadId
        let
          event' = hoistEvent liftIO event
        result <-
          runExceptT
            $ do
              now <- currentTime
              party <-
                case deserialiseAddress $ unAddress address of
                  Just address' -> pure $ uncurry Address address'
                  Nothing       -> throwError "Address conversion failed."
              let
                contract = makeContract (now + 15 * 60 * 1000) party
              inputs <- randomInputs party
              runScenario event' config address key contract inputs
        case result of
          Right contractId -> addField event $ ("success" :: T.Text) ≔ contractId
          Left message     -> addField event $ ("failure" :: T.Text) ≔ message


main :: IO ()
main =
  do
    eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
    configFilename : countText : addressKeyEntries <- getArgs
    config <- read <$> readFile configFilename
    let
      count = read countText
    addressKeys <-
      sequence
        [
          do
            Just address <- pure . fromBech32 $ T.pack addressBech32
            Right key <- C.readFileTextEnvelope (C.AsSigningKey C.AsPaymentExtendedKey) keyFilename
            pure (address, key)
        |
          [addressBech32, keyFilename] <- chunksOf 2 addressKeyEntries
        ]
    void
      $ mapConcurrently
        (\(address, key) -> replicateM_ count $ runOne eventBackend config address key)
        addressKeys
