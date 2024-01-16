module Language.Marlowe.Runtime.Benchmark.Lifecycle.Scenario (
  Scenario (..),
  choiceDepositNotifyScenario,
  randomScenario,
) where

import Control.Monad.IO.Class (MonadIO)
import Language.Marlowe.Core.V1.Semantics.Types
import System.Random (randomRIO, randoms)

data Scenario = Scenario
  { contract :: Contract
  , actions :: [(Party, InputContent)]
  }
  deriving (Eq, Generic, Ord, Show)

randomScenario
  :: (MonadIO m)
  => POSIXTime
  -> [Party]
  -> m Scenario
randomScenario = choiceDepositNotifyScenario

choiceDepositNotifyScenario
  :: (MonadIO m)
  => POSIXTime
  -> [Party]
  -> m Scenario
choiceDepositNotifyScenario timeout parties =
  do
    choiceParty : depositParty : notifyParty : _ <- randoms $ (paries !!) <$> randomRIO (0, 2)
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
          [ (choiceParty, IChoice (ChoiceId "Amount" party) amount)
          , (depositParty, IDeposit notifyParty depositParty (Token "" "") amount)
          , (notifyParty, INotify)
          ]
    pure Scenario{..}
