{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}

module Language.Marlowe.Runtime.Web.StandardContract (
  createStandardContract,
  createStandardContractWithTags,
  executeCompleteStandardContractLifecycle,
  StandardContractLifecycleInit (..),
  StandardContractFundsDeposited (..),
  StandardContractChoiceMade (..),
  StandardContractNotified (..),
  StandardContractClosed (..),
  StandardContractPayoutsPartyAWithdrawn (..),
  StandardContractLifecycleEnded (..),
) where

import Control.Monad.RWS.Strict (MonadIO (liftIO))
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Object.Types (
  LabelledObject (LabelledObject),
  ObjectBundle (ObjectBundle),
  ObjectType (..),
  fromCoreContract,
 )
import Language.Marlowe.Runtime.Integration.Common (Wallet (..), expectJust)
import Language.Marlowe.Runtime.Integration.StandardContract (standardContract)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses (..))

import Language.Marlowe.Runtime.Web.Adapter.Server.DTO (ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Client (Page (..), getPayouts, postContract, postContractSource)
import Language.Marlowe.Runtime.Web.Common (
  buildBurnRoleTokenTx,
  choose,
  deposit,
  notify,
  submitBurnRoleTokensTx,
  submitContract,
  submitTransaction,
  submitWithdrawal,
  withdraw,
 )
import qualified Language.Marlowe.Runtime.Web.Core.MarloweVersion as Web
import qualified Language.Marlowe.Runtime.Web.Core.Tx as Web
import Language.Marlowe.Runtime.Web.Payout.API (PayoutHeader (payoutId), PayoutStatus (..))

import Language.Marlowe.Runtime.Web.Contract.API (ContractOrSourceId (..), PostContractSourceResponse (..))
import qualified Language.Marlowe.Runtime.Web.Contract.API as Web
import Language.Marlowe.Runtime.Web.Core.BlockHeader (
  BlockHeader,
 )
import qualified Language.Marlowe.Runtime.Web.Core.Metadata as Web
import Language.Marlowe.Runtime.Web.Core.Roles (RoleTokenConfig (..), RoleTokenRecipient (..))
import qualified Language.Marlowe.Runtime.Web.Core.Roles as Web
import Language.Marlowe.Runtime.Web.Role.API (
  BurnRoleTokensTxEnvelope,
 )

import qualified Language.Marlowe.Runtime.Web.Role.TokenFilter as Web
import Language.Marlowe.Runtime.Web.Tx.API (
  ApplyInputsTxEnvelope (transactionId),
  CardanoTxBody,
  CreateTxEnvelope (contractId),
  WithdrawTxEnvelope,
 )
import qualified Language.Marlowe.Runtime.Web.Tx.API as Web
import Pipes (yield)
import Servant.Client.Streaming (ClientM)

data StandardContractLifecycleInit = StandardContractLifecycleInit
  { makeInitialDeposit :: ClientM StandardContractFundsDeposited
  , contractCreated :: CreateTxEnvelope CardanoTxBody
  , createdBlock :: BlockHeader
  }

data StandardContractFundsDeposited = StandardContractFundsDeposited
  { chooseGimmeTheMoney :: ClientM StandardContractChoiceMade
  , initialFundsDeposited :: ApplyInputsTxEnvelope CardanoTxBody
  , initialDepositBlock :: BlockHeader
  }

data StandardContractChoiceMade = StandardContractChoiceMade
  { sendNotify :: ClientM StandardContractNotified
  , gimmeTheMoneyChosen :: ApplyInputsTxEnvelope CardanoTxBody
  , choiceBlock :: BlockHeader
  }

data StandardContractNotified = StandardContractNotified
  { makeReturnDeposit :: ClientM StandardContractClosed
  , notified :: ApplyInputsTxEnvelope CardanoTxBody
  , notifiedBlock :: BlockHeader
  }

data StandardContractClosed = StandardContractClosed
  { withdrawPartyAPayout :: ClientM StandardContractPayoutsPartyAWithdrawn
  , returnDeposited :: ApplyInputsTxEnvelope CardanoTxBody
  , returnDepositBlock :: BlockHeader
  }

data StandardContractPayoutsPartyAWithdrawn = StandardContractPayoutsPartyAWithdrawn
  { burnRoleTokens :: ClientM StandardContractLifecycleEnded
  , tx :: WithdrawTxEnvelope CardanoTxBody
  , block :: BlockHeader
  }

data StandardContractLifecycleEnded where
  StandardContractLifecycleEnded
    :: { tx
          :: BurnRoleTokensTxEnvelope CardanoTxBody
       }
    -> StandardContractLifecycleEnded

createStandardContract :: Wallet -> Wallet -> ClientM StandardContractLifecycleInit
createStandardContract = createStandardContractWithTags mempty

createStandardContractWithTags :: Map Text Web.Metadata -> Wallet -> Wallet -> ClientM StandardContractLifecycleInit
createStandardContractWithTags tags partyAWallet partyBWallet = do
  let partyAWalletAddresses = addresses partyAWallet
  let partyAWebChangeAddress = toDTO $ changeAddress partyAWalletAddresses
  let partyAWebExtraAddresses = Set.map toDTO $ extraAddresses partyAWalletAddresses
  let partyAWebCollateralUtxos = Set.map toDTO $ collateralUtxos partyAWalletAddresses

  let partyBWalletAddresses = addresses partyBWallet

  partyBAddress <-
    liftIO $ expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress partyBWalletAddresses
  now <- liftIO getCurrentTime
  let (contract, partyA, partyB) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

  PostContractSourceResponse{contractSourceId} <-
    postContractSource "main" $ yield $ ObjectBundle $ pure $ LabelledObject "main" ContractType $ fromCoreContract contract

  contractCreated@Web.CreateTxEnvelope{contractId} <-
    postContract
      Nothing
      partyAWebChangeAddress
      (Just partyAWebExtraAddresses)
      (Just partyAWebCollateralUtxos)
      Web.PostContractsRequest
        { metadata = mempty
        , version = Web.V1
        , threadTokenName = Nothing
        , roles =
            Just
              . Web.Mint
              . Map.singleton "Party A"
              $ RoleTokenConfig (Map.singleton (ClosedRole partyAWebChangeAddress) 1) Nothing
        , contract = ContractOrSourceId $ Right contractSourceId
        , accounts = mempty
        , minUTxODeposit = Nothing
        , tags = tags
        }

  createdBlock <- submitContract partyAWallet contractCreated

  pure
    StandardContractLifecycleInit
      { createdBlock
      , contractCreated
      , makeInitialDeposit = do
          initialFundsDeposited <-
            deposit
              partyAWallet
              contractId
              partyA
              partyA
              ada
              100_000_000
          initialDepositBlock <- submitTransaction partyAWallet initialFundsDeposited

          pure
            StandardContractFundsDeposited
              { initialDepositBlock
              , initialFundsDeposited
              , chooseGimmeTheMoney = do
                  gimmeTheMoneyChosen <-
                    choose
                      partyBWallet
                      contractId
                      "Gimme the money"
                      partyB
                      0
                  choiceBlock <- submitTransaction partyBWallet gimmeTheMoneyChosen

                  pure
                    StandardContractChoiceMade
                      { choiceBlock
                      , gimmeTheMoneyChosen
                      , sendNotify = do
                          notified <- notify partyAWallet contractId
                          notifiedBlock <- submitTransaction partyAWallet notified

                          pure
                            StandardContractNotified
                              { notifiedBlock
                              , notified
                              , makeReturnDeposit = do
                                  returnDeposited <-
                                    deposit
                                      partyBWallet
                                      contractId
                                      partyA
                                      partyB
                                      ada
                                      100_000_000
                                  returnDepositBlock <- submitTransaction partyBWallet returnDeposited

                                  pure
                                    StandardContractClosed
                                      { returnDepositBlock
                                      , returnDeposited
                                      , withdrawPartyAPayout = do
                                          Page{..} <- getPayouts (Just $ Set.singleton contractId) Nothing (Just Available) Nothing
                                          let payouts = Set.fromList $ payoutId <$> items
                                          withdrawPartyATxBody <- withdraw partyAWallet payouts
                                          blockPartyA <- submitWithdrawal partyAWallet withdrawPartyATxBody
                                          pure
                                            StandardContractPayoutsPartyAWithdrawn
                                              { tx = withdrawPartyATxBody
                                              , block = blockPartyA
                                              , burnRoleTokens = do
                                                  let roleFilter = Web.RoleTokenFilterByContracts $ Set.singleton contractId
                                                  txSubmittedPartyA <- buildBurnRoleTokenTx partyAWallet roleFilter
                                                  submitBurnRoleTokensTx partyAWallet txSubmittedPartyA
                                                  pure
                                                    StandardContractLifecycleEnded
                                                      { tx = txSubmittedPartyA
                                                      }
                                              }
                                      }
                              }
                      }
              }
      }

executeCompleteStandardContractLifecycle :: Wallet -> Wallet -> ClientM (Web.TxOutRef, [Web.TxId])
executeCompleteStandardContractLifecycle partyAWallet partyBWallet = do
  StandardContractLifecycleInit{contractCreated = Web.CreateTxEnvelope{contractId}, makeInitialDeposit} <-
    createStandardContract partyAWallet partyBWallet
  StandardContractFundsDeposited
    { initialFundsDeposited = Web.ApplyInputsTxEnvelope{transactionId = initialFundsDepositedTxId}
    , chooseGimmeTheMoney
    } <-
    makeInitialDeposit
  StandardContractChoiceMade
    { gimmeTheMoneyChosen = Web.ApplyInputsTxEnvelope{transactionId = gimmeTheMoneyChosenTxId}
    , sendNotify
    } <-
    chooseGimmeTheMoney
  StandardContractNotified{notified = Web.ApplyInputsTxEnvelope{transactionId = notifiedTx}, makeReturnDeposit} <-
    sendNotify
  StandardContractClosed
    { returnDeposited = Web.ApplyInputsTxEnvelope{transactionId = returnDepositedTx}
    , withdrawPartyAPayout
    } <-
    makeReturnDeposit
  StandardContractPayoutsPartyAWithdrawn{burnRoleTokens} <- withdrawPartyAPayout
  _ <- burnRoleTokens

  pure
    ( contractId
    ,
      [ initialFundsDepositedTxId
      , gimmeTheMoneyChosenTxId
      , notifiedTx
      , returnDepositedTx
      ]
    )
