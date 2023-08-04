{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Raffle (raffle, Sponsor (..), Oracle (..), ChunkSize (..)) where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.List.Index (indexed)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NEL
import Data.List.Split (chunksOf)
import GHC.Generics (Generic)
import Language.Marlowe (POSIXTime)
import Language.Marlowe.Core.V1.Semantics.Types

newtype Sponsor = Sponsor Party
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype Oracle = Oracle Party
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype ChunkSize = ChunkSize Int
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

raffle
  :: Sponsor
  -> Oracle
  -> ChunkSize
  -> NonEmpty Party
  -> NonEmpty Token
  -> Timeout
  -> Timeout
  -> Timeout
  -> Contract
raffle sponsor oracle chunkSize parties prizeNFTPerRound deposit select payout =
  makeDeposit sponsor prizeNFTPerRound deposit $
    mkRaffleRounds sponsor oracle prizeNFTPerRound chunkSize parties select payout

mkRaffleRounds
  :: Sponsor
  -> Oracle
  -> NonEmpty Token
  -> ChunkSize
  -> NonEmpty Party
  -> Timeout
  -> Timeout
  -> Contract
mkRaffleRounds sponsor oracle prizeNFTPerRound chunkSize parties select payout =
  selectWinner oracle (toInteger . length $ parties) select $
    payWinner
      sponsor
      oracle
      prizeNFTPerRound
      chunkSize
      partiesWithDistributedNumber
      partiesWithDistributedNumber
      select
      payout
  where
    partiesWithDistributedNumber = (fmap . first $ toInteger) . NEL.fromList . indexed . NEL.toList $ parties

makeDeposit
  :: Sponsor
  -> NonEmpty Token
  -> POSIXTime
  -> Contract
  -> Contract
makeDeposit (Sponsor sponsor) prizeNFTPerRound deadline contract =
  makeDeposit' (NEL.toList prizeNFTPerRound)
  where
    makeDeposit' [] = contract
    makeDeposit' (x : xs) =
      When
        [Case (Deposit sponsor sponsor x (Constant 1)) (makeDeposit' xs)]
        deadline
        Close

selectWinner
  :: Oracle
  -> Integer
  -> POSIXTime
  -> Contract
  -> Contract
selectWinner (Oracle oracle) nbParties deadline contract =
  When
    [Case (Choice (ChoiceId "RANDOM" oracle) [Bound 0 (nbParties - 1)]) contract]
    deadline
    Close

payWinner
  :: Sponsor
  -> Oracle
  -> NonEmpty Token
  -> ChunkSize
  -> NonEmpty (Integer, Party)
  -> NonEmpty (Integer, Party)
  -> Timeout
  -> Timeout
  -> Contract
payWinner sponsor@(Sponsor sponsorParty) oracle@(Oracle oracleParty) (prizeNFTPerRound :| remainingPrizeNFTPerRound) chunkSize@(ChunkSize chunkLength) allPartiesMinusWinners partiesChunk select payoutDeadline
  | length partiesChunk <= chunkLength =
      When
        [ Case (Notify (ValueEQ (ChoiceValue (ChoiceId "RANDOM" oracleParty)) (Constant i))) $
          Pay
            sponsorParty
            (Party party)
            prizeNFTPerRound
            (Constant 1)
            ( case remainingPrizeNFTPerRound of
                [] -> Close
                remainingPricesPerRound' ->
                  mkRaffleRounds -- start a new raffle without the winner
                    sponsor
                    oracle
                    (NEL.fromList remainingPricesPerRound') -- removing the current round price
                    chunkSize
                    (NEL.fromList . (snd <$>) . NEL.filter (\(i', _) -> i' /= i) $ allPartiesMinusWinners) -- removing the winner
                    select
                    payoutDeadline
            )
        | (i, party) <- NEL.toList partiesChunk
        ]
        payoutDeadline
        Close
  | otherwise =
      When
        [ Case (Notify (ValueLE (ChoiceValue (ChoiceId "RANDOM" oracleParty)) (Constant . fst $ last chunkedParties'))) $
          payWinner
            sponsor
            oracle
            (prizeNFTPerRound :| remainingPrizeNFTPerRound)
            chunkSize
            allPartiesMinusWinners
            (NEL.fromList chunkedParties')
            select
            payoutDeadline
        | let chunks k xs = chunksOf (div (length xs + k - 1) k) xs
        , chunkedParties' <- chunks chunkLength (NEL.toList partiesChunk)
        ]
        payoutDeadline
        Close
