{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Integration.Basic where

import Cardano.Api (getTxId)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Map as Map
import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (..),
  Bound (..),
  Case (..),
  ChoiceId (..),
  Contract (..),
  Input (..),
  InputContent (..),
  Party (..),
 )
import qualified Language.Marlowe.Protocol.HeaderSync.Client as HeaderSync
import qualified Language.Marlowe.Protocol.Sync.Client as MarloweSync
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), TxOutRef (..))
import Language.Marlowe.Runtime.Client (createContract, runMarloweHeaderSyncClient, runMarloweSyncClient)
import Language.Marlowe.Runtime.Core.Api (
  ContractId (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Transaction (..),
  TransactionOutput (..),
  TransactionScriptOutput (..),
  emptyMarloweTransactionMetadata,
 )
import Language.Marlowe.Runtime.History.Api (ContractStep (..), RedeemStep (..))
import Language.Marlowe.Runtime.Integration.ApplyInputs (utcTimeToPOSIXTime)
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (
  ContractCreated (..),
  ContractCreatedInEra (..),
  InputsApplied (..),
  InputsAppliedInEra (..),
  RoleTokensConfig (..),
  WithdrawTx (..),
  WithdrawTxInEra (..),
 )
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = describe "Basic scenarios" do
  it "Basic e2e scenario" $ withLocalMarloweRuntime $ runIntegrationTest do
    partyAWallet <- getGenesisWallet 0
    partyBWallet <- getGenesisWallet 1
    let -- 1. Start MarloweHeaderSyncClient (request next)
        startDiscoveryClient :: Integration TxOutRef
        startDiscoveryClient = runMarloweHeaderSyncClient $
          HeaderSync.MarloweHeaderSyncClient $
            pure
            -- 2. Expect wait
            $
              headerSyncRequestNextExpectWait do
                -- 3. Create standard contract
                contract@StandardContractInit{..} <- createStandardContract partyAWallet partyBWallet
                -- 4. Poll
                -- 5. Expect new headers
                headerSyncPollExpectNewHeaders createdBlock [contractCreatedToContractHeader createdBlock contractCreated] $
                  continueWithNewHeaders contract

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
          let ContractCreated _ ContractCreatedInEra{contractId, rolesCurrency} = contractCreated
          pure $
            MarloweSync.SendMsgFollowContract contractId
            -- 10. Expect contract found
            $
              marloweSyncExpectContractFound \actualBlock MarloweV1 createStep -> do
                liftIO $ actualBlock `shouldBe` createdBlock
                liftIO $ createStep `shouldBe` contractCreatedToCreateStep contractCreated
                -- 11. Request next
                -- 12. Expect roll forward with deposit
                marloweSyncRequestNextExpectRollForward
                  initialDepositBlock
                  [ApplyTransaction $ inputsAppliedToTransaction initialDepositBlock initialFundsDeposited]
                  do
                    -- 13. Request next
                    -- 14. Expect wait, poll, expect wait
                    pure $ marloweSyncRequestNextExpectWait $ pure $ marloweSyncPollExpectWait do
                      -- 15. Make choice as party B
                      StandardContractChoiceMade{..} <- chooseGimmeTheMoney
                      -- 16. Poll
                      -- 17. Expect roll forward with choice
                      marloweSyncPollExpectRollForward
                        choiceBlock
                        [ApplyTransaction $ inputsAppliedToTransaction choiceBlock gimmeTheMoneyChosen]
                        do
                          -- 18. Request next
                          -- 19. Expect wait
                          pure $ marloweSyncRequestNextExpectWait do
                            -- 20. Notify
                            StandardContractNotified{..} <- sendNotify

                            -- 21. Deposit as party B
                            StandardContractClosed{..} <- makeReturnDeposit

                            -- 22. Withdraw as party A
                            (WithdrawTx _ WithdrawTxInEra{txBody = withdrawTxBody}, withdrawBlock) <- withdrawPartyAFunds

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
                                let withdrawTxId' = fromCardanoTxId $ getTxId withdrawTxBody
                                marloweSyncRequestNextExpectRollForward
                                  withdrawBlock
                                  [RedeemPayout $ RedeemStep payoutTxOutRef withdrawTxId' $ AssetId rolesCurrency "Party A"]
                                  do
                                    -- 29. Request next (marlowe sync)
                                    -- 30. Expect wait
                                    -- 31. Cancel
                                    -- 32. Done
                                    let InputsApplied _ InputsAppliedInEra{output} = notified
                                    TransactionScriptOutput{utxo = notifyTxOutRef} <- expectJust "Failed to obtain deposit output" $ scriptOutput output
                                    pure $ marloweSyncRequestNextExpectWait $ pure $ MarloweSync.SendMsgCancel $ MarloweSync.SendMsgDone notifyTxOutRef

    txOutRef <- startDiscoveryClient
    -- 37. Start MarloweSyncClient (follow a tx in the contract)
    -- 38. Expect contract not found
    runMarloweSyncClient $
      MarloweSync.MarloweSyncClient $
        pure $
          MarloweSync.SendMsgFollowContract (ContractId txOutRef) $
            MarloweSync.ClientStFollow
              { recvMsgContractFound = \_ _ _ -> fail "Expected contract not found, got contract found"
              , recvMsgContractNotFound = pure ()
              }

  -- This is an adaptation of https://nbviewer.org/gist/bwbush/4e8a7196902bfdb0f7f6f7f4a6e3e643
  it "PLT-6904 ADA role tokens" $ withLocalMarloweRuntime $ runIntegrationTest do
    wallet <- getGenesisWallet 0
    now <- liftIO getCurrentTime
    let deadline = addUTCTime (secondsToNominalDiffTime $ 30 * 60) now
        contract =
          When
            [ Case (Choice (ChoiceId "Option A" $ Role "") [Bound 1 1]) Close
            ]
            (utcTimeToPOSIXTime deadline)
            Close
    ContractCreated era0 created <-
      expectRight "Failed to create contract"
        =<< createContract
          Nothing
          MarloweV1
          (wallet.addresses)
          (RoleTokensUsePolicy "")
          emptyMarloweTransactionMetadata
          2_000_000
          (Left contract)
    _ <- submit wallet era0 created.txBody
    InputsApplied era1 applied <-
      choose wallet created.contractId "Option A" (Role "") 1
    _ <- submit wallet era1 applied.txBody
    liftIO do
      applied.input
        `shouldBe` TransactionScriptOutput
          { address = created.marloweScriptAddress
          , assets = created.assets
          , utxo = unContractId created.contractId
          , datum = created.datum
          }
      applied.output.payouts `shouldBe` mempty
      applied.output.scriptOutput `shouldBe` Nothing
      applied.inputs `shouldBe` [NormalInput $ IChoice (ChoiceId "Option A" $ Role "") 1]
