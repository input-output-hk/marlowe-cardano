{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Integration.Basic
  where

import Cardano.Api (getTxId)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Map as Map
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HeaderSync
import qualified Language.Marlowe.Protocol.Sync.Client as MarloweSync
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (AssetId(..), TxOutRef(..))
import Language.Marlowe.Runtime.Client (runMarloweHeaderSyncClient, runMarloweSyncClient)
import Language.Marlowe.Runtime.Core.Api
  ( ContractId(..)
  , MarloweVersion(..)
  , MarloweVersionTag(..)
  , Transaction(..)
  , TransactionOutput(..)
  , TransactionScriptOutput(..)
  )
import Language.Marlowe.Runtime.History.Api (ContractStep(..), RedeemStep(..))
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated(..), InputsApplied(..))
import Test.Hspec (Spec, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = it "Basic e2e scenario" $ withLocalMarloweRuntime $ runIntegrationTest do
  partyAWallet <- getGenesisWallet 0
  partyBWallet <- getGenesisWallet 1
  let
    -- 1. Start MarloweHeaderSyncClient (request next)
    startDiscoveryClient :: Integration TxOutRef
    startDiscoveryClient = runMarloweHeaderSyncClient
        $ HeaderSync.MarloweHeaderSyncClient
        $ pure
        -- 2. Expect wait
        $ headerSyncRequestNextExpectWait do
          -- 3. Create standard contract
          contract@StandardContractInit{..} <- createStandardContract partyAWallet partyBWallet
          -- 4. Poll
          -- 5. Expect new headers
          headerSyncPollExpectNewHeaders createdBlock [contractCreatedToContractHeader createdBlock contractCreated]
            $ continueWithNewHeaders contract

      -- 6. RequestNext (header sync)
      -- 7. Expect Wait
    continueWithNewHeaders contract = pure $ HeaderSync.SendMsgRequestNext $ headerSyncExpectWait do
      -- 8. Deposit funds
      fundsDeposited <- makeInitialDeposit contract
      txOutRef <- runMarloweSyncClient $ marloweSyncClient contract fundsDeposited
      -- 33. Poll
      -- 34. Expect wait
      -- 35. Cancel
      -- 36. Done
      pure $ HeaderSync.SendMsgPoll $ headerSyncExpectWait $ pure $ HeaderSync.SendMsgCancel $ HeaderSync.SendMsgDone txOutRef

    -- 9. Start MarloweSyncClient (follow contract)
    marloweSyncClient
      :: StandardContractInit 'V1
      -> StandardContractFundsDeposited 'V1
      -> MarloweSync.MarloweSyncClient Integration TxOutRef
    marloweSyncClient StandardContractInit{..} StandardContractFundsDeposited{..} = MarloweSync.MarloweSyncClient do
      let ContractCreated{contractId, rolesCurrency} = contractCreated
      pure
        $ MarloweSync.SendMsgFollowContract contractId
        -- 10. Expect contract found
        $ marloweSyncExpectContractFound \actualBlock MarloweV1 createStep -> do
          liftIO $ actualBlock `shouldBe` createdBlock
          liftIO $ createStep `shouldBe` contractCreatedToCreateStep contractCreated
          -- 11. Request next
          -- 12. Expect roll forward with deposit
          marloweSyncRequestNextExpectRollForward initialDepositBlock [ApplyTransaction $ inputsAppliedToTransaction initialDepositBlock initialFundsDeposited] do
            -- 13. Request next
            -- 14. Expect wait, poll, expect wait
            pure $ marloweSyncRequestNextExpectWait $ pure $ marloweSyncPollExpectWait do
              -- 15. Make choice as party B
              StandardContractChoiceMade{..} <- chooseGimmeTheMoney
              -- 16. Poll
              -- 17. Expect roll forward with choice
              marloweSyncPollExpectRollForward choiceBlock [ApplyTransaction $ inputsAppliedToTransaction choiceBlock gimmeTheMoneyChosen] do
                -- 18. Request next
                -- 19. Expect wait
                pure $ marloweSyncRequestNextExpectWait do
                  -- 20. Notify
                  StandardContractNotified{..} <- sendNotify

                  -- 21. Deposit as party B
                  StandardContractClosed{..} <- makeReturnDeposit

                  -- 22. Withdraw as party A
                  (withdrawTxBody, withdrawBlock) <- withdrawPartyAFunds

                  -- 23. Poll
                  -- 24. Expect roll forward with notify
                  marloweSyncPollExpectRollForward notifiedBlock [ApplyTransaction $ inputsAppliedToTransaction notifiedBlock notified] do
                    let depositTransaction@Transaction{output = TransactionOutput{payouts}} = inputsAppliedToTransaction returnDepositBlock returnDeposited
                    -- 25. Request next
                    -- 26. Expect roll forward with deposit
                    marloweSyncRequestNextExpectRollForward returnDepositBlock [ApplyTransaction depositTransaction] do
                      -- 27. Request next
                      -- 28. Expect roll forward with withdraw
                      payoutTxOutRef <- expectJust "Failed to extract payout from deposit" case Map.toList payouts of
                        [(txOutRef, _)] -> Just txOutRef
                        _ -> Nothing
                      let withdrawTxId = fromCardanoTxId $ getTxId withdrawTxBody
                      marloweSyncRequestNextExpectRollForward withdrawBlock [RedeemPayout $ RedeemStep payoutTxOutRef withdrawTxId $ AssetId rolesCurrency "Party A"] do
                        -- 29. Request next (marlowe sync)
                        -- 30. Expect wait
                        -- 31. Cancel
                        -- 32. Done
                        let InputsApplied{output} = notified
                        TransactionScriptOutput{utxo = notifyTxOutRef} <- expectJust "Failed to obtain deposit output" output
                        pure $ marloweSyncRequestNextExpectWait $ pure $ MarloweSync.SendMsgCancel $ MarloweSync.SendMsgDone notifyTxOutRef


  txOutRef <- startDiscoveryClient
  -- 37. Start MarloweSyncClient (follow a tx in the contract)
  -- 38. Expect contract not found
  runMarloweSyncClient $ MarloweSync.MarloweSyncClient $ pure $ MarloweSync.SendMsgFollowContract (ContractId txOutRef) $ MarloweSync.ClientStFollow
    { recvMsgContractFound = \_ _ _ -> fail "Expected contract not found, got contract found"
    , recvMsgContractNotFound = pure ()
    }
