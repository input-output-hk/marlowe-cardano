{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Integration.StandardContract where

import Cardano.Api (getTxId)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics.Types (
  Action (Choice, Deposit, Notify),
  Bound (Bound),
  Case (Case),
  ChoiceId (ChoiceId),
  Contract (Close, Pay, When),
  Observation (TrueObs),
  Party (..),
  Payee (Party),
  Value (AvailableMoney, Constant),
 )
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Protocol.Load.Client (pushContract)
import Language.Marlowe.Protocol.Query.Types (PayoutHeader (..))
import Language.Marlowe.Runtime.Cardano.Api (fromCardanoTxId)
import Language.Marlowe.Runtime.ChainSync.Api (AssetId (..), BlockHeader, PolicyId, TxId)
import qualified Language.Marlowe.Runtime.ChainSync.Api as Chain
import Language.Marlowe.Runtime.Client (createContract, runMarloweLoadClient)
import Language.Marlowe.Runtime.Core.Api (
  ContractId,
  Datum,
  MarloweMetadata (..),
  MarloweMetadataTag,
  MarloweTransactionMetadata (..),
  MarloweVersion (..),
  MarloweVersionTag (..),
  Payout (..),
  TransactionOutput (..),
  emptyMarloweTransactionMetadata,
 )
import Language.Marlowe.Runtime.Discovery.Api (ContractHeader)
import Language.Marlowe.Runtime.Integration.Common (
  Integration,
  Wallet (..),
  buildBurnRoleTokensTx,
  choose,
  contractCreatedToContractHeader,
  deposit,
  expectJust,
  expectRight,
  notify,
  submit,
  withdraw,
 )
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (
  BurnRoleTokensTx,
  ContractCreated (..),
  ContractCreatedInEra (..),
  Destination (ToAddress),
  InputsApplied (..),
  InputsAppliedInEra (..),
  RoleTokenFilter' (..),
  RoleTokensConfig (..),
  WalletAddresses (changeAddress),
  WithdrawTx (..),
  WithdrawTxInEra (..),
  mkMint,
 )
import qualified PlutusLedgerApi.V2 as PV2

data StandardContractLifecycleInit v = StandardContractLifecycleInit
  { makeInitialDeposit :: Integration (StandardContractFundsDeposited v)
  , contractCreated :: ContractCreated v
  , createdBlock :: BlockHeader
  }

standardContractHeader :: StandardContractLifecycleInit v -> ContractHeader
standardContractHeader StandardContractLifecycleInit{..} = contractCreatedToContractHeader createdBlock contractCreated

standardContractId :: StandardContractLifecycleInit v -> ContractId
standardContractId StandardContractLifecycleInit{contractCreated = ContractCreated _ ContractCreatedInEra{..}} = contractId

standardContractDatum :: StandardContractLifecycleInit v -> Datum v
standardContractDatum StandardContractLifecycleInit{contractCreated = ContractCreated _ ContractCreatedInEra{..}} = datum

standardContractPayout :: StandardContractClosed 'V1 -> Maybe (WithdrawTx 'V1) -> PayoutHeader
standardContractPayout StandardContractClosed{returnDeposited = InputsApplied _ InputsAppliedInEra{..}} mWithdraw =
  PayoutHeader
    { contractId
    , payoutId
    , withdrawalId = withdrawTxId <$> mWithdraw
    , role = datum
    }
  where
    (payoutId, Payout{..}) = head . Map.toList $ payouts output

withdrawTxId :: WithdrawTx 'V1 -> TxId
withdrawTxId (WithdrawTx _ WithdrawTxInEra{..}) = fromCardanoTxId $ getTxId txBody

data StandardContractFundsDeposited v = StandardContractFundsDeposited
  { chooseGimmeTheMoney :: Integration (StandardContractChoiceMade v)
  , initialFundsDeposited :: InputsApplied v
  , initialDepositBlock :: BlockHeader
  }

data StandardContractChoiceMade v = StandardContractChoiceMade
  { sendNotify :: Integration (StandardContractNotified v)
  , gimmeTheMoneyChosen :: InputsApplied v
  , choiceBlock :: BlockHeader
  }

data StandardContractNotified v = StandardContractNotified
  { makeReturnDeposit :: Integration (StandardContractClosed v)
  , notified :: InputsApplied v
  , notifiedBlock :: BlockHeader
  }

data StandardContractClosed v = StandardContractClosed
  { withdrawPartyAFunds :: Integration (WithdrawTx v, BlockHeader)
  , rolesCurrency :: PolicyId
  , burnPartyARoleTokenByAssetIdPartyA :: Integration (BurnRoleTokensTx v)
  , burnPartyARoleTokenByContractId :: Integration (BurnRoleTokensTx v)
  , burnPartyARoleTokenByPolicyId :: Integration (BurnRoleTokensTx v)
  , burnPartyARoleTokenByAny :: Integration (BurnRoleTokensTx v)
  , returnDeposited :: InputsApplied v
  , returnDepositBlock :: BlockHeader
  }

createStandardContract :: Wallet -> Wallet -> Integration (StandardContractLifecycleInit 'V1)
createStandardContract = createStandardContractWithTags mempty

createStandardContractWithTags
  :: Set MarloweMetadataTag -> Wallet -> Wallet -> Integration (StandardContractLifecycleInit 'V1)
createStandardContractWithTags tags partyAWallet =
  createStandardContractWithTagsAndRolesConfig
    Nothing
    ( RoleTokensMint $
        mkMint $
          pure ("Party A", Nothing, ToAddress . changeAddress $ addresses partyAWallet, Chain.Quantity 1)
    )
    tags
    partyAWallet

createStandardContractWithRolesConfig
  :: Maybe Chain.TokenName
  -> RoleTokensConfig
  -> Wallet
  -> Wallet
  -> Integration (StandardContractLifecycleInit 'V1)
createStandardContractWithRolesConfig threadName rolesConfig =
  createStandardContractWithTagsAndRolesConfig threadName rolesConfig mempty

createStandardContractWithTagsAndRolesConfig
  :: Maybe Chain.TokenName
  -> RoleTokensConfig
  -> Set MarloweMetadataTag
  -> Wallet
  -> Wallet
  -> Integration (StandardContractLifecycleInit 'V1)
createStandardContractWithTagsAndRolesConfig threadName rolesConfig tags partyAWallet partyBWallet = do
  partyBAddress <-
    expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress $ addresses partyBWallet
  now <- liftIO getCurrentTime
  let (contract, partyA, partyB) = standardContract partyBAddress now $ secondsToNominalDiffTime 100
  contractHash <- expectJust "Failed to push contract" =<< runMarloweLoadClient (pushContract contract)
  result <-
    createContract
      Nothing
      MarloweV1
      (addresses partyAWallet)
      threadName
      rolesConfig
      ( if Set.null tags
          then emptyMarloweTransactionMetadata
          else
            emptyMarloweTransactionMetadata
              { marloweMetadata =
                  Just
                    MarloweMetadata
                      { tags = Map.fromSet (const Nothing) tags
                      , continuations = Nothing
                      }
              }
      )
      Nothing
      mempty
      (Right contractHash)
  contractCreated@(ContractCreated era0 ContractCreatedInEra{contractId, txBody = createTxBody, rolesCurrency}) <-
    expectRight "failed to create standard contract" result
  createdBlock <- submit partyAWallet era0 createTxBody

  pure
    StandardContractLifecycleInit
      { createdBlock
      , contractCreated
      , makeInitialDeposit = do
          initialFundsDeposited@(InputsApplied era1 InputsAppliedInEra{txBody = initialDepositTxBody}) <-
            deposit
              partyAWallet
              contractId
              partyA
              partyA
              ada
              100_000_000
          initialDepositBlock <- submit partyAWallet era1 initialDepositTxBody

          pure
            StandardContractFundsDeposited
              { initialDepositBlock
              , initialFundsDeposited
              , chooseGimmeTheMoney = do
                  gimmeTheMoneyChosen@(InputsApplied era2 InputsAppliedInEra{txBody = choiceTxBody}) <-
                    choose
                      partyBWallet
                      contractId
                      "Gimme the money"
                      partyB
                      0
                  choiceBlock <- submit partyBWallet era2 choiceTxBody

                  pure
                    StandardContractChoiceMade
                      { choiceBlock
                      , gimmeTheMoneyChosen
                      , sendNotify = do
                          notified@(InputsApplied era3 InputsAppliedInEra{txBody = notifyTxBody}) <- notify partyAWallet contractId
                          notifiedBlock <- submit partyAWallet era3 notifyTxBody

                          pure
                            StandardContractNotified
                              { notifiedBlock
                              , notified
                              , makeReturnDeposit = do
                                  returnDeposited@(InputsApplied era4 InputsAppliedInEra{txBody = returnTxBody, output}) <-
                                    deposit
                                      partyBWallet
                                      contractId
                                      partyA
                                      partyB
                                      ada
                                      100_000_000
                                  returnDepositBlock <- submit partyBWallet era4 returnTxBody

                                  let buildBurnRoleTokensByPartyATx = buildBurnRoleTokensTx partyAWallet
                                  pure
                                    StandardContractClosed
                                      { rolesCurrency
                                      , returnDepositBlock
                                      , returnDeposited
                                      , withdrawPartyAFunds = do
                                          withdrawTx@(WithdrawTx era5 WithdrawTxInEra{txBody = withdrawTxBody}) <-
                                            withdraw partyAWallet $ Map.keysSet $ payouts output
                                          (withdrawTx,) <$> submit partyAWallet era5 withdrawTxBody
                                      , burnPartyARoleTokenByAssetIdPartyA =
                                          buildBurnRoleTokensByPartyATx $ RoleTokenFilterByTokens $ Set.singleton $ AssetId rolesCurrency "Party A"
                                      , burnPartyARoleTokenByContractId =
                                          buildBurnRoleTokensByPartyATx $ RoleTokenFilterByContracts $ Set.singleton contractId
                                      , burnPartyARoleTokenByPolicyId =
                                          buildBurnRoleTokensByPartyATx $ RoleTokenFilterByPolicyIds $ Set.singleton rolesCurrency
                                      , burnPartyARoleTokenByAny = buildBurnRoleTokensByPartyATx RoleTokenFilterAny
                                      }
                              }
                      }
              }
      }

-- | A standard contract that can be used for testing.
-- | The contract is a simple escrow contract where Party A deposits 100 ADA and Party B can claim the funds by choosing a choice.
-- | The contract is parameterized by the address of Party B, the start time, and the timeout length.
-- | The contract is structured as follows:
-- | 1. Party A deposits 100 ADA.
-- | 2. Party B can choose to claim the funds by choosing the choice "Gimme the money".
-- | 3. If Party B chooses the choice, Party A must deposit 100 ADA to Party B within the timeout length.
-- | 4. If Party B does not choose the choice within the timeout length, the contract closes.
-- | 5. A Payout is only available for Party B when the contract closes.
standardContract
  :: PV2.Address
  -> UTCTime
  -> NominalDiffTime
  -> (Contract, Party, Party)
standardContract partyBAddress startTime timeoutLength = (contract, partyA, partyB)
  where
    toPosixTime t = PV2.POSIXTime $ floor $ 1000 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t)
    contract =
      When
        [ Case
            (Deposit partyA partyA ada (Constant 100_000_000))
            ( When
                [ Case
                    (Choice (ChoiceId "Gimme the money" partyB) [Bound 0 0])
                    ( When
                        [ Case
                            (Notify TrueObs)
                            ( Pay
                                partyA
                                (Party partyB)
                                ada
                                (AvailableMoney partyA ada)
                                ( When
                                    [ Case (Deposit partyA partyB ada (Constant 100_000_000)) Close
                                    ]
                                    ( toPosixTime $
                                        timeoutLength
                                          `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime)))
                                    )
                                    Close
                                )
                            )
                        ]
                        (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime)))
                        Close
                    )
                ]
                (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime))
                Close
            )
        ]
        (toPosixTime $ timeoutLength `addUTCTime` startTime)
        Close

    partyA = Role "Party A"
    -- 0x00 = testnet
    partyB = Address (toEnum 0x00) partyBAddress
