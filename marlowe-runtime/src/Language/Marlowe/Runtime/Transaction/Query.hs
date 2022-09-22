{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Transaction.Query
  where

import Data.List (scanl')
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Type.Equality (testEquality, type (:~:)(Refl))
import Language.Marlowe.Protocol.Sync.Client
import Language.Marlowe.Runtime.ChainSync.Api (StakeReference(..), stakeReference)
import Language.Marlowe.Runtime.Core.Api
import Language.Marlowe.Runtime.History.Api
import Language.Marlowe.Runtime.Transaction.Api
import Language.Marlowe.Runtime.Transaction.Constraints
import qualified Language.Marlowe.Runtime.Transaction.Constraints as Constraints

type LoadWalletContext = WalletAddresses -> IO WalletContext

type LoadMarloweContext = forall v
   . MarloweVersion v
  -> ContractId
  -> IO (Either LoadMarloweContextError (MarloweContext v))

loadWalletContext :: LoadWalletContext
loadWalletContext = error "not implemented"

-- | Loads the current MarloweContext for a contract by its ID.
loadMarloweContext
  :: (forall a. MarloweSyncClient IO a -> IO a)
  -> LoadMarloweContext
loadMarloweContext runClient desiredVersion contractId = runClient client
  where
    client = MarloweSyncClient $ pure clientInit
    -- Start by following the contract
    clientInit = SendMsgFollowContract contractId clientFollow

    clientFollow = ClientStFollow
      -- If the contract isn't found, return an error
      { recvMsgContractNotFound = pure $ Left LoadMarloweContextErrorNotFound
      -- Otherwise,
      , recvMsgContractFound = \blockHeader actualVersion createStep ->
          -- Otherwise, check the desired version matches the actual one
          pure case testEquality desiredVersion actualVersion of
            Nothing -> SendMsgDone
              $ Left
              $ LoadMarloweContextErrorVersionMismatch
              $ SomeMarloweVersion actualVersion
            -- Build the initial context and use it as a seed for the loop.
            Just Refl -> clientIdle $ pure (blockHeader, initialContext createStep)
      }

    -- Request the next steps in the contract
    clientIdle = SendMsgRequestNext . clientNext
    clientNext contexts = ClientStNext
      -- If the creation event was rolled back, return an error
      { recvMsgRollBackCreation =
          (pure :: forall a. a -> IO a) $ Left LoadMarloweContextErrorNotFound
      -- Handle rollbacks
      , recvMsgRollBackward =
          pure . handleRollback contexts
      -- Handle new steps
      , recvMsgRollForward = \blockHeader ->
          pure . clientIdle . appendSteps contexts blockHeader
      -- If we are told to wait (because there are no more steps to send),
      -- we've reached the tip of the contract. Return the most recent
      -- accumulated context.
      , recvMsgWait =
          pure $ SendMsgCancel $ SendMsgDone $ Right $ snd $ NE.head contexts
      }

    initialContext :: CreateStep v -> MarloweContext v
    initialContext CreateStep{..} = MarloweContext
      -- Get the stake credential from the script address.
      { stakeCredential = case stakeReference scriptAddress of
          Just (StakeCredential credential) -> Just credential
          _ -> Nothing
      -- Get the script output of the create event.
      , scriptOutput = Just createOutput
      -- No payouts to start with
      , payoutOutputs = mempty
      }
      where
        TransactionScriptOutput scriptAddress _ _ _ = createOutput

    handleRollback contexts@((blockHeader, _) :| contexts') rollbackBlock
      -- If the latest block is after the rollback block
      | blockHeader > rollbackBlock = case contexts' of
          -- And the previous contexts is non-empty, keep rolling back
          s : ss -> handleRollback (s :| ss) rollbackBlock
          -- And the previous contexts is empty, return an error
          [] -> SendMsgDone $ Left LoadMarloweContextErrorNotFound
      -- Otherwise, resume from this block.
      | otherwise = clientIdle contexts

    appendSteps contexts@((_, prevContext) :| _) blockHeader steps =
      prependList
        -- drop 1 because scanl' starts with the seed value (i.e. prevContext)
        ((blockHeader,) <$> drop 1 (scanl' applyStep prevContext steps))
        contexts

    prependList :: [a] -> NonEmpty a -> NonEmpty a
    prependList = \case
      [] -> id
      x : xs -> \(y :| ys) -> x :| xs <> (y : ys)

    -- Update the context with a new step
    applyStep context = \case
      -- For ApplyTransaction steps
      ApplyTransaction Transaction{output = TransactionOutput{..}} -> context
        -- Replace the scriptOutput with that of the new transaction
        { Constraints.scriptOutput = scriptOutput
        -- Add new payouts
        , Constraints.payoutOutputs = payoutOutputs context <> payouts
        }
      -- For RedeemPayout steps
      RedeemPayout RedeemStep{..} -> context
        -- Remove the payout that was redeemed
        { Constraints.payoutOutputs = Map.delete utxo $ payoutOutputs context
        }
