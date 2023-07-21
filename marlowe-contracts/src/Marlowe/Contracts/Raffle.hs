{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.Raffle (raffle, Sponsor (..), Oracle (..), ChunkSize (..), main) where

import Control.Monad.Writer (Writer, runWriter)
import Data.Aeson (ToJSON, encodeFile)
import Data.Aeson.Types (FromJSON)
import Data.Bifunctor (Bifunctor (..))
import Data.List.Index (indexed)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NEL
import Data.List.Split (chunksOf)
import GHC.Generics (Generic)
import Language.Marlowe (POSIXTime)
import Language.Marlowe.Core.V1.Merkle (Continuations, deepMerkleize, shallowMerkleize)
import Language.Marlowe.Core.V1.Semantics.Types

import Data.Map qualified as M (mapKeys)

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

main :: IO ()
main =
  do
    let (contract, continuations) =
          raffle
            (Sponsor $ Role "Sponsor")
            (Oracle $ Role "Oracle")
            (ChunkSize 2)
            (NEL.fromList $ Role <$> ["Alice", "Bob", "Charlie", "David", "Eve"])
            (NEL.fromList $ Token "8bb3b343d8e404472337966a722150048c768d0a92a9813596c5338d" <$> ["First", "Second"])
            1000
            2000
            3000
    encodeFile "contract.json" contract
    encodeFile "continuations.json" . M.mapKeys show $ continuations

raffle
  :: Sponsor
  -> Oracle
  -> ChunkSize
  -> NonEmpty Party
  -> NonEmpty Token
  -> Timeout
  -> Timeout
  -> Timeout
  -> (Contract, Continuations)
raffle sponsor oracle chunkSize parties prizeNFTPerRound deposit select payout =
  runWriter $
    makeDeposit sponsor prizeNFTPerRound deposit
      =<< mkRaffleRounds sponsor oracle prizeNFTPerRound chunkSize parties select payout

mkRaffleRounds
  :: Sponsor
  -> Oracle
  -> NonEmpty Token
  -> ChunkSize
  -> NonEmpty Party
  -> Timeout
  -> Timeout
  -> Writer Continuations Contract
mkRaffleRounds sponsor oracle prizeNFTPerRound chunkSize parties select payout =
  selectWinner oracle (toInteger . length $ parties) select
    =<< deepMerkleize
    =<< payWinner
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
  -> Writer Continuations Contract
makeDeposit (Sponsor sponsor) prizeNFTPerRound deadline contract =
  -- Only merkleize the deposit itself, preventing deeper merkleization.
  shallowMerkleize $
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
  -> Writer Continuations Contract
selectWinner (Oracle oracle) nbParties deadline contract =
  -- Don't merkleize the oracle choice because merkleization prevents the general-purpose oracle from operating.
  pure $
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
  -> Writer Continuations Contract
payWinner sponsor@(Sponsor sponsorParty) oracle@(Oracle oracleParty) (prizeNFTPerRound :| remainingPrizeNFTPerRound) chunkSize@(ChunkSize chunkLength) allPartiesMinusWinners partiesChunk select payoutDeadline
  | length partiesChunk <= chunkLength =
      do
        cases <-
          sequence
            [ Case (Notify (ValueEQ (ChoiceValue (ChoiceId "RANDOM" oracleParty)) (Constant i)))
              . Pay
                sponsorParty
                (Party party)
                prizeNFTPerRound
                (Constant 1)
              <$> case remainingPrizeNFTPerRound of
                [] -> pure Close
                remainingPricesPerRound' ->
                  mkRaffleRounds -- start a new raffle without the winner
                    sponsor
                    oracle
                    (NEL.fromList remainingPricesPerRound') -- removing the current round price
                    chunkSize
                    (NEL.fromList . (snd <$>) . NEL.filter (\(i', _) -> i' /= i) $ allPartiesMinusWinners) -- removing the winner
                    select
                    payoutDeadline
            | (i, party) <- NEL.toList partiesChunk
            ]
        -- Only merkleize this notify, preventing deeper merkleization.
        shallowMerkleize $ When cases payoutDeadline Close
  | otherwise =
      do
        cases <-
          sequence
            [ Case (Notify (ValueLE (ChoiceValue (ChoiceId "RANDOM" oracleParty)) (Constant . fst $ last chunkedParties')))
              <$> payWinner
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
        -- Deeply merkleize the search for the winner.
        deepMerkleize $
          When
            cases
            payoutDeadline
            Close
