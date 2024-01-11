{-# LANGUAGE RecordWildCards #-}

module Language.Marlowe.Runtime.Benchmark.Lifecycle.Scenario (
  Scenario (..),
  choiceDepositNotifyScenario,
  randomScenario,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import Language.Marlowe (POSIXTime (POSIXTime))
import Language.Marlowe.Core.V1.Semantics.Types
import System.Random (randomRIO)

data Scenario = Scenario
  { parties :: [Party]
  , contract :: Contract
  , actions :: [(Party, InputContent)]
  }
  deriving (Eq, Generic, Show)

randomScenario
  :: (MonadIO m)
  => NominalDiffTime
  -> [Party]
  -> m Scenario
randomScenario lifetime parties =
  do
    timeout <- POSIXTime . ceiling . (* 1_000) . nominalDiffTimeToSeconds . (+ lifetime) <$> liftIO getPOSIXTime
    choiceDepositNotifyScenario timeout $ (take 3 . concat . replicate 3) parties

choiceDepositNotifyScenario
  :: (MonadIO m)
  => POSIXTime
  -> [Party]
  -> m Scenario
choiceDepositNotifyScenario timeout parties =
  do
    choiceParty <- (parties !!) <$> randomRIO (0, 2)
    depositParty <- (parties !!) <$> randomRIO (0, 2)
    notifyParty <- (parties !!) <$> randomRIO (0, 2)
    amount <- randomRIO (1_500_000, 2_000_000)
    let contract =
          When
            [ Case (Choice (ChoiceId "Amount" choiceParty) [Bound 1_500_000 2_000_000]) $
                When
                  [ Case (Deposit notifyParty depositParty (Token "" "") (ChoiceValue $ ChoiceId "Amount" choiceParty)) $
                      When
                        [ Case
                            (Notify TrueObs)
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
        actions =
          [ (choiceParty, IChoice (ChoiceId "Amount" choiceParty) amount)
          , (depositParty, IDeposit notifyParty depositParty (Token "" "") amount)
          , (notifyParty, INotify)
          ]
    pure Scenario{..}
