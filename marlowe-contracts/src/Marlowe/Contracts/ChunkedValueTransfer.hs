{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.ChunkedValueTransfer where

import Data.Aeson (FromJSON, ToJSON)
import Data.Foldable (fold)
import Data.Functor ((<&>))
import qualified Data.List as List
import qualified Data.List.NonEmpty as List.NonEmpty
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as Map
import GHC.Generics (Generic)
import Language.Marlowe (POSIXTime (..))
import qualified Language.Marlowe as M
import Language.Marlowe.Core.V1.Semantics.Types hiding (Deposit)
import qualified Language.Marlowe.Core.V1.Semantics.Types as V1
import qualified PlutusLedgerApi.V1 as P
import qualified PlutusTx.AssocMap as AssocMap

newtype PayoutChunkSize = PayoutChunkSize Int
  deriving stock (Eq, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype Sender = Sender Party
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype Recipient = Recipient Party
  deriving stock (Eq, Ord, Generic, Show)
  deriving newtype (ToJSON, FromJSON)

newtype RecipientsAmounts = RecipientsAmounts (Map Recipient P.Value)
  deriving stock (Eq, Generic, Show)

data Deposit = Deposit !Sender !Token !Integer
  deriving stock (Eq, Generic, Show)

data Payout = Payout !Sender !Recipient !Token !Integer
  deriving stock (Eq, Generic, Show)

newtype BaseTimeout = BaseTimeout POSIXTime
  deriving stock (Eq, Generic, Show)

suspendContract :: Contract -> BaseTimeout -> Contract
suspendContract continuation (BaseTimeout (POSIXTime timeoutMilliseconds)) = do
  let -- Mere mortal rough approximation of infinity.
      never = POSIXTime do
        let hour = 1_000 * 60 * 60
        timeoutMilliseconds + hour * 24 * 365 * 10_000
  suspendContractWithTimeoutCleanup continuation never Close

suspendContractWithTimeoutCleanup :: Contract -> Timeout -> Contract -> Contract
suspendContractWithTimeoutCleanup continuation = When [Case (Notify TrueObs) continuation]

-- Just flipped `foldr`
foldrLoop :: (Foldable t) => b -> t a -> (a -> b -> b) -> b
foldrLoop zero foldable step = foldr step zero foldable

foldMapFlipped :: (Monoid m) => (Foldable f) => f a -> (a -> m) -> m
foldMapFlipped = flip foldMap

mkChunkedPayouts :: [Payout] -> PayoutChunkSize -> BaseTimeout -> Contract
mkChunkedPayouts [] _ _ = Close
mkChunkedPayouts payouts (PayoutChunkSize chunkSize) timeout = do
  let chunks = chunksOf chunkSize payouts

      suspendContinuation Close = Close
      suspendContinuation continuation = suspendContract continuation timeout

  foldrLoop Close chunks \chunk accContinuation -> do
    let continuation' = suspendContinuation accContinuation
    foldrLoop continuation' chunk \payout accContinuation'' -> do
      let Payout (Sender senderParty) (Recipient recipientParty) token amount = payout
      Pay senderParty (Party recipientParty) token (Constant amount) accContinuation''

allDeposits :: Sender -> RecipientsAmounts -> [Deposit]
allDeposits sender (RecipientsAmounts recipient2Amount) = do
  let totalValue = fold recipient2Amount
      unsorted =
        foldMapFlipped (AssocMap.toList $ P.getValue totalValue) \(currencySymbol, tokenName2Amount) -> do
          foldMapFlipped (AssocMap.toList tokenName2Amount) \(tokenName, tokenAmount) -> do
            [Deposit sender (M.Token currencySymbol tokenName) tokenAmount]
  List.sortOn (\(Deposit _ token _) -> token) unsorted

-- | At every step during deposit phase in theory we can encounter a timeout.
-- In such a case we should payback in chunks.
data DepositStep = DepositStep {dsAlreadDeposited :: ![Deposit], dsCurrent :: !Deposit}

-- | Deposits are not really chunked but the timeout payouts are.
-- We store deposits in the sender account during the execution.
mkDepositWithChunkedPaybacks :: DepositStep -> PayoutChunkSize -> Timeout -> Contract -> Contract
mkDepositWithChunkedPaybacks (DepositStep alreadyDeposited deposit) payoutChunkSize timeout continuation = do
  let paybacks =
        alreadyDeposited <&> \(Deposit sender@(Sender s) t a) ->
          Payout sender (Recipient s) t a

      -- On timeout we are paying back to the sender.
      -- This suspension is for merkleization.
      timeoutContract :: Contract
      timeoutContract = suspendContract (mkChunkedPayouts paybacks payoutChunkSize (BaseTimeout timeout)) (BaseTimeout timeout)

      Deposit (Sender senderParty) token amount = deposit
  -- When the contract closes the current chunk payouts are made automatically and we know that the size of the
  -- remaining payout is lower then the chunk size.
  When [Case (V1.Deposit senderParty senderParty token (Constant amount)) continuation] timeout timeoutContract

mkDeposits :: [Deposit] -> PayoutChunkSize -> Timeout -> Contract -> Contract
mkDeposits deposits payoutChunkSize timeout continuation = do
  let depositSteps = do
        zip (List.NonEmpty.toList $ List.NonEmpty.inits deposits) deposits <&> \(alreadyDeposited, deposit) -> do
          DepositStep alreadyDeposited deposit
  foldrLoop continuation depositSteps \depositStep accContinuation ->
    mkDepositWithChunkedPaybacks depositStep payoutChunkSize timeout accContinuation

allPayouts :: Sender -> RecipientsAmounts -> [Payout]
allPayouts sender (RecipientsAmounts recipient2Value) = do
  foldMapFlipped (Map.toList recipient2Value) \(recipient, value) ->
    foldMapFlipped (AssocMap.toList $ P.getValue value) \(currencySymbol, tokenName2Amount) ->
      foldMapFlipped (AssocMap.toList tokenName2Amount) \(tokenName, tokenAmount) ->
        [Payout sender recipient (M.Token currencySymbol tokenName) tokenAmount]

-- | This contract is not really practical given the current limitations of Marlowe validator - we are able to handle
-- only few distinct accounts/tokens. It lives here as a baseline for performance testing.
--
-- Contract logic:
-- Given a set of recipients with specific expected `Value` payouts and a single sender
-- construct a contract which will accept deposits (in alphabetical order of (currencySymbol, tokenName)) and then
-- pays out in "chunks" (based on `PayoutChunkSize`) to the recipients.
-- The payouts are chunked because of the on chain limits - the payout loop is suspended and resumed on every
-- payout chunk.
-- The contract is still possibly unsafe because at some point multiple "notifies" can in theory expire so a single
-- trigger can turn into a larger set of payouts which can exceed on chain limits. Because of that we use infinity
-- approximation (+10_000 years) for all the timeouts in the `Notify` constructs.
chunkedValueTransfer
  :: Sender
  -> RecipientsAmounts
  -> PayoutChunkSize
  -> Timeout
  -> Contract
chunkedValueTransfer sender recipientsAmounts payoutsChunkSize timeout = do
  let deposits = allDeposits sender recipientsAmounts
      payouts = allPayouts sender recipientsAmounts
      baseTimeout = BaseTimeout timeout
      payoutsContract = mkChunkedPayouts payouts payoutsChunkSize baseTimeout
  mkDeposits deposits payoutsChunkSize timeout (suspendContract payoutsContract baseTimeout)
