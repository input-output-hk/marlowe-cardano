{-# LANGUAGE OverloadedStrings #-}

module Marlowe.Contracts.ChunkedValueTransfer where

import Data.List.NonEmpty (inits, nonEmpty, toList)
import Data.List.Split (chunksOf)
import Language.Marlowe (POSIXTime (..))
import Language.Marlowe.Core.V1.Semantics.Types

newtype PayoutChunkSize = PayoutChunkSize Int

newtype DepositChunk = DepositChunk [(Token, Integer)]

newtype Sender = Sender Party

newtype Recipient = Recipient Party

data DepositStep = DepositStep {dsAlreadDeposited :: ![DepositChunk], dsChunk :: !DepositChunk}

notifyTrue :: Timeout -> Contract -> Contract -> Contract
notifyTrue timeout continuation = When [Case (Notify TrueObs) continuation] timeout

mkChunkPayouts :: Sender -> Recipient -> Timeout -> DepositChunk -> Contract -> Contract
mkChunkPayouts (Sender senderParty) (Recipient recipientParty) timeout (DepositChunk chunk) continuation = do
  let step (token, amount) continuation' = Pay senderParty (Party recipientParty) token (Constant amount) continuation'
      suspendedContinuation = notifyTrue timeout continuation continuation
  foldr step suspendedContinuation chunk

-- | We store deposits in the sender account during execution
mkChunkDeposits :: Sender -> Timeout -> DepositStep -> Contract -> Contract
mkChunkDeposits sender@(Sender senderParty) timeout@(POSIXTime timeoutMilliseconds) (DepositStep alreadyDeposited (DepositChunk chunk)) continuation = do
  let interval = 1000 * 60 * 60 * 24 * 365 * 100 -- ~ 100 years in milliseconds
      timeouts = map (\n -> POSIXTime (n * interval + timeoutMilliseconds)) [1 ..]

      alreadyDeposited' :: [(DepositChunk, Timeout)]
      alreadyDeposited' = zip alreadyDeposited timeouts

      -- On timeout we are paying back to the sender
      timeoutContract :: Contract
      timeoutContract =
        foldr
          (\(depositedChunk, chunkTimeout) -> mkChunkPayouts sender (Recipient senderParty) chunkTimeout depositedChunk)
          Close
          alreadyDeposited'

      -- When the contract closes the current chunk payouts are made automatically and we know that the size of the
      -- remaining payout is lower then the chunk size.
      step (token, amount) continuation' = When [Case (Deposit senderParty senderParty token (Constant amount)) continuation'] timeout timeoutContract
  foldr step continuation chunk

newtype Deposits = Deposits [(Token, Integer)]

chunkedValueTransfer
  :: Sender
  -> Recipient
  -> Timeout
  -> Deposits
  -> PayoutChunkSize
  -> Contract
chunkedValueTransfer sender recipient timeout (Deposits ds) (PayoutChunkSize chunkSize) = do
  case nonEmpty (chunksOf chunkSize ds) of
    Nothing -> Close
    Just chunks -> do
      let depositsSteps = do
            let step alreadyDepositedChuns chunk = DepositStep (fmap DepositChunk alreadyDepositedChuns) (DepositChunk chunk)
            zipWith step (toList $ inits chunks) (toList chunks)
          payouts = foldr (mkChunkPayouts sender recipient timeout) Close (fmap DepositChunk $ toList chunks)
      foldr (mkChunkDeposits sender timeout) payouts depositsSteps
