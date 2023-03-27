{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}


module Main
  ( main
  ) where


import Control.Concurrent (myThreadId)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (join, replicateM, unless)
import Control.Monad.Except (MonadIO, liftIO, runExceptT, throwError)
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
import Language.Marlowe.Runtime.App.Parser (addressKeyfileParser, getConfigParser)
import Language.Marlowe.Runtime.App.Transact (App, runWithEvents)
import Language.Marlowe.Runtime.App.Types (Config)
import Language.Marlowe.Runtime.ChainSync.Api (Address(unAddress))
import Language.Marlowe.Runtime.Core.Api (ContractId)
import Observe.Event.Dynamic (DynamicEventSelector(..))
import Observe.Event.Explicit (EventBackend, addField, hoistEventBackend, idInjectSelector, subEventBackend, withEvent)
import Observe.Event.Render.JSON (DefaultRenderSelectorJSON(defaultRenderSelectorJSON))
import Observe.Event.Render.JSON.Handle (JSONRef, simpleJsonStderrBackend)
import Observe.Event.Syntax ((≔))
import Plutus.V2.Ledger.Api (POSIXTime(..))
import System.Random (randomRIO)

import qualified Cardano.Api as C
  (AsType(AsPaymentExtendedKey, AsSigningKey), PaymentExtendedKey, SigningKey, readFileTextEnvelope)
import Data.Either (isRight)
import qualified Data.Text as T (Text)
import qualified Data.Time.Clock.POSIX as P (getPOSIXTime)
import qualified Options.Applicative as O
import System.Exit (exitFailure)


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
  :: EventBackend App r DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> Contract
  -> [InputContent]
  -> App ContractId
runScenario backend config address key contract inputs =
  runWithEvents backend config address key contract (pure . NormalInput <$> inputs) 1_500_000


currentTime :: MonadIO m => m POSIXTime
currentTime = POSIXTime . floor . (* 1000) . nominalDiffTimeToSeconds <$> liftIO P.getPOSIXTime


runOne
  :: EventBackend IO JSONRef DynamicEventSelector
  -> Config
  -> Address
  -> C.SigningKey C.PaymentExtendedKey
  -> IO Bool
runOne eventBackend config address key =
  withEvent eventBackend (DynamicEventSelector "Contract")
    $ \event ->
      do
        threadId <- myThreadId
        addField event $ ("threadId" :: T.Text) ≔ show threadId
        let
          subBackend = hoistEventBackend liftIO $ subEventBackend idInjectSelector event eventBackend
        result <-
          runExceptT
            $ do
              now <- currentTime
              party <-
                case deserialiseAddress $ unAddress address of
                  Just address' -> pure $ uncurry Address address'
                  Nothing       -> throwError "Address conversion failed."
              let
                contract = makeContract (now + 5 * 60 * 60 * 1000) party
              inputs <- randomInputs party
              runScenario subBackend config address key contract inputs
        case result of
          Right contractId -> addField event $ ("success" :: T.Text) ≔ contractId
          Left message     -> addField event $ ("failure" :: T.Text) ≔ message
        pure $ isRight result


main :: IO ()
main =
  do
    Command{..} <- O.execParser =<< commandParser
    addressKeys <-
      sequence
        [
          C.readFileTextEnvelope (C.AsSigningKey C.AsPaymentExtendedKey) keyFilename
            >>= \case
              Right key    -> pure (address, key)
              Left message -> error $ show message
        |
          (address, keyFilename) <- parties
        ]
    eventBackend <- simpleJsonStderrBackend defaultRenderSelectorJSON
    results <- mapConcurrently
      (uncurry $ (replicateM count .) . runOne eventBackend config)
      addressKeys
    unless (and $ join results) exitFailure


data Command =
  Command
  {
    config :: Config
  , count :: Int
  , parties :: [(Address, FilePath)]
  }
    deriving (Show)


commandParser :: IO (O.ParserInfo Command)
commandParser =
  do
    configParser <- getConfigParser
    let
      commandOptions =
        Command
          <$> configParser
          <*> (O.argument O.auto $ O.metavar "NATURAL" <> O.help "The number of contracts to run sequentially for each party.")
          <*> (O.many . O.argument addressKeyfileParser) (O.metavar "ADDRESS=KEYFILE" <> O.help "The addresses of the parties and the files with their signing keys.")
    pure
      $ O.info
        (O.helper {- <*> O.versionOption -} <*> commandOptions)
        (
          O.fullDesc
            <> O.progDesc "This command-line tool is a scaling test client for Marlowe Runtime: it runs multiple contracts in parallel against a Marlowe Runtime backend, with a specified number of contracts run in sequence for each party and each party running contracts in parallel."
            <> O.header "marlowe-scaling : run multiple Marlowe test contracts in parallel"
        )
