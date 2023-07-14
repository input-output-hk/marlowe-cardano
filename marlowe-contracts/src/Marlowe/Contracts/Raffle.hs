{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Raffle where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.List.Index (deleteAt, indexed)
import Data.List.Split (chunksOf)
import GHC.Generics (Generic)
import Language.Marlowe (POSIXTime)
import Language.Marlowe.Core.V1.Semantics.Types

ada :: Token
ada = Token "" ""

newtype Sponsor = Sponsor Party
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype Oracle = Oracle Party
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype ChunkSize = ChunkSize Int
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

-- newtype DepositDeadline = DepositDeadline Timeout
--
-- newtype SelectDeadline = SelectDeadline Timeout
--
-- newtype PayoutDeadline = PayoutDeadline Timeout

raffle
  :: Sponsor
  -> Oracle
  -> ChunkSize
  -> [Party]
  -> [Integer]
  -> Timeout
  -> Timeout
  -> Timeout
  -> Contract
raffle sponsor oracle chunkSize parties pricesPerRound deposit select payout =
  makeDeposit sponsor pricesPerRound deposit $
    mkRaffleRounds sponsor oracle pricesPerRound chunkSize parties select payout

mkRaffleRounds
  :: Sponsor
  -> Oracle
  -> [Integer]
  -> ChunkSize
  -> [Party]
  -> Timeout
  -> Timeout
  -> Contract
mkRaffleRounds sponsor oracle pricesPerRound chunkSize parties select payout =
  selectWinner oracle (toInteger . length $ parties) select $
    payWinner sponsor oracle pricesPerRound chunkSize parties select payout

makeDeposit
  :: Sponsor
  -> [Integer]
  -> POSIXTime
  -> Contract
  -> Contract
makeDeposit (Sponsor sponsor) pricesPerRound deadline contract =
  makeDeposit' pricesPerRound
  where
    makeDeposit' [] = contract
    makeDeposit' (x : xs) =
      When
        [Case (Deposit sponsor sponsor ada (Constant x)) (makeDeposit' xs)]
        deadline
        Close

selectWinner
  :: Oracle
  -> Integer
  -> POSIXTime
  -> Contract
  -> Contract
selectWinner (Oracle oracle) count deadline contract =
  When
    [Case (Choice (ChoiceId "Random" oracle) [Bound 0 (count - 1)]) contract]
    deadline
    Close

payWinner
  :: Sponsor
  -> Oracle
  -> [Integer]
  -> ChunkSize
  -> [Party]
  -> Timeout
  -> Timeout
  -> Contract
payWinner sponsor@(Sponsor sponsorParty) oracle@(Oracle oracleParty) remainingPricesPerRound chunkSize@(ChunkSize chunkLength) parties select payoutDeadline
  | (length $ parties) <= chunkLength =
      When
        [ Case (Notify (ValueEQ (ChoiceValue (ChoiceId "Random" oracleParty)) (Constant . toInteger $ i))) $
          Pay
            sponsorParty
            (Party party)
            ada
            (Constant . head $ remainingPricesPerRound)
            ( if null remainingPricesPerRound
                then Close
                else mkRaffleRounds sponsor oracle (deleteAt 0 remainingPricesPerRound) chunkSize (deleteAt i parties) select payoutDeadline
            )
        | (i, party) <- indexed parties
        ]
        payoutDeadline
        Close
  | otherwise =
      When
        [ Case (Notify (ValueLE (ChoiceValue (ChoiceId "Random" oracleParty)) (Constant . toInteger . fst $ last parties'))) $
          payWinner sponsor oracle remainingPricesPerRound chunkSize (snd <$> parties') select payoutDeadline
        | let chunks k xs = chunksOf (div (length xs + k - 1) k) xs
        , parties' <- chunks chunkLength (indexed parties)
        ]
        payoutDeadline
        Close
