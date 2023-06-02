{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Language.Marlowe.Runtime.Integration.Intersections
  where

import Cardano.Api (getTxId)
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (BlockHeader, TxId, TxOutRef(..))
import Language.Marlowe.Runtime.Core.Api (ContractId(..))
import Language.Marlowe.Runtime.Integration.Common
import Language.Marlowe.Runtime.Integration.StandardContract
import Language.Marlowe.Runtime.Transaction.Api (ContractCreated(..), InputsApplied(..))
import Test.Hspec (Spec, it)
import Test.Integration.Marlowe.Local (withLocalMarloweRuntime)

spec :: Spec
spec = it "Intersections" $ withLocalMarloweRuntime $ runIntegrationTest do
  wallet1 <- getGenesisWallet 0
  wallet2 <- getGenesisWallet 1
  -- 1. Let X be current point
  pX <- getTip
  -- 2. Create a standard contract
  contractA <- createStandardContract wallet1 wallet2
  let ContractCreated{contractId = idA} = contractCreated contractA
  -- 3. Let A0 be the block the previous contract was created in
  let pA0 = createdBlock contractA
  -- 4. Create a standard contract
  contractB <- createStandardContract wallet1 wallet2
  -- 5. Let B0 be the block the previous contract was created in
  let pB0 = createdBlock contractB
  -- 6. Create a standard contract
  contractC <- createStandardContract wallet1 wallet2
  -- 7. Let C0 be the block the previous contract was created in
  let pC0 = createdBlock contractC
  -- 8. Apply all inputs to first contract, let A1-A4 be the blocks where all the inputs were applied in order
  -- 9. Withdraw funds from first contract, let A5 be the block where this happens
  (pA1, txIdA1, pA2, pA3, pA4, pA5) <- completeContract contractA
  -- 10. Do a MarloweSync Intersect with first contract, empty points
  -- 11. Expect Intersect not found
  marloweSyncIntersectExpectNotFound idA []
  -- 12. Do a MarloweSync Intersect with points [A0..A3]
  -- 13. Expect Intersect found @ A3
  marloweSyncIntersectExpectFound idA [pA0, pA1, pA2, pA3] pA3 [pA4, pA5]
  -- 14. Do a MarloweSync Intersect with points [A2..A4]
  -- 15. Expect Intersect found @ A4
  marloweSyncIntersectExpectFound idA [pA2, pA3, pA4] pA4 [pA5]
  marloweSyncIntersectExpectFound idA [pA2, pA3, pA5] pA3 [pA4, pA5]
  -- 16. Do a MarloweSync Intersect with points [B0,A1-A5]
  -- 17. Expect Intersect not found
  marloweSyncIntersectExpectNotFound idA [pB0, pA1, pA2, pA3, pA4, pA5]
  -- 18. Do a MarloweHeaderSync with points []
  -- 19. Expect Intersect not found
  headerSyncIntersectExpectNotFound []
  -- 20. Do a MarloweHeaderSync with points [A0, B0, C0]
  -- 21. Expect Intersect found @ C0
  headerSyncIntersectExpectFound [pA0, pB0, pC0] pC0 []
  -- 22. Do a MarloweHeaderSync with points [A0, B0]
  -- 23. Expect Intersect found @ B0
  headerSyncIntersectExpectFound [pA0, pB0] pB0 [pC0]
  -- 24. Do a MarloweHeaderSync with points [X, A0, B0, C0]
  -- 25. Expect Intersect not found
  headerSyncIntersectExpectNotFound [pX, pA0, pB0, pC0]
  -- 26. Do a MarloweSync Intersect for "contractId" of tx at point A1 with points [A1..A5]
  -- 27. Expect Intersect not found
  marloweSyncIntersectExpectNotFound (ContractId $ TxOutRef txIdA1 1) [pA1, pA2, pA3, pA4, pA5]

completeContract :: StandardContractInit v -> Integration (BlockHeader, TxId, BlockHeader, BlockHeader, BlockHeader, BlockHeader)
completeContract StandardContractInit{..} = do
  StandardContractFundsDeposited{..} <- makeInitialDeposit
  StandardContractChoiceMade{..} <- chooseGimmeTheMoney
  StandardContractNotified{..} <- sendNotify
  StandardContractClosed{..} <- makeReturnDeposit
  (_, withdrawBlock) <- withdrawPartyAFunds
  let InputsApplied{txBody} = initialFundsDeposited
  pure (initialDepositBlock, fromCardanoTxId $ getTxId txBody, choiceBlock, notifiedBlock, returnDepositBlock, withdrawBlock)
