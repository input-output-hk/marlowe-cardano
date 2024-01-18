{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Web.StandardContract where

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
import Language.Marlowe.Runtime.Web (
  ApplyInputsTxEnvelope,
  BlockHeader,
  ContractOrSourceId (..),
  CreateTxEnvelope,
  PayoutHeader (..),
  PayoutStatus (..),
  RoleTokenConfig (..),
  RoleTokenRecipient (ClosedRole),
  WithdrawTxEnvelope,
 )
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (Page (..), getPayouts, postContract, postContractSource)
import Language.Marlowe.Runtime.Web.Common (
  choose,
  deposit,
  notify,
  submitContract,
  submitTransaction,
  submitWithdrawal,
  withdraw,
 )
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO (toDTO))
import Language.Marlowe.Runtime.Web.Types (PostContractSourceResponse (..))
import Pipes (yield)
import Servant.Client.Streaming (ClientM)

data StandardContractInit = StandardContractInit
  { makeInitialDeposit :: ClientM StandardContractFundsDeposited
  , contractCreated :: CreateTxEnvelope
  , createdBlock :: BlockHeader
  }

data StandardContractFundsDeposited = StandardContractFundsDeposited
  { chooseGimmeTheMoney :: ClientM StandardContractChoiceMade
  , initialFundsDeposited :: ApplyInputsTxEnvelope
  , initialDepositBlock :: BlockHeader
  }

data StandardContractChoiceMade = StandardContractChoiceMade
  { sendNotify :: ClientM StandardContractNotified
  , gimmeTheMoneyChosen :: ApplyInputsTxEnvelope
  , choiceBlock :: BlockHeader
  }

data StandardContractNotified = StandardContractNotified
  { makeReturnDeposit :: ClientM StandardContractClosed
  , notified :: ApplyInputsTxEnvelope
  , notifiedBlock :: BlockHeader
  }

data StandardContractClosed = StandardContractClosed
  { withdrawPartyAFunds :: ClientM (WithdrawTxEnvelope, BlockHeader)
  , returnDeposited :: ApplyInputsTxEnvelope
  , returnDepositBlock :: BlockHeader
  }

createStandardContract :: Wallet -> Wallet -> ClientM StandardContractInit
createStandardContract = createStandardContractWithTags mempty

createStandardContractWithTags :: Map Text Web.Metadata -> Wallet -> Wallet -> ClientM StandardContractInit
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
    StandardContractInit
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
                                      , withdrawPartyAFunds = do
                                          Page{..} <- getPayouts (Just $ Set.singleton contractId) Nothing (Just Available) Nothing
                                          let payouts = Set.fromList $ payoutId <$> items
                                          withdrawTxBody <- withdraw partyAWallet payouts
                                          (withdrawTxBody,) <$> submitWithdrawal partyAWallet withdrawTxBody
                                      }
                              }
                      }
              }
      }

createFullyExecutedStandardContract :: Wallet -> Wallet -> ClientM (Web.TxOutRef, [Web.TxId])
createFullyExecutedStandardContract partyAWallet partyBWallet = do
  StandardContractInit{contractCreated, makeInitialDeposit} <- createStandardContract partyAWallet partyBWallet
  StandardContractFundsDeposited{initialFundsDeposited, chooseGimmeTheMoney} <- makeInitialDeposit
  StandardContractChoiceMade{gimmeTheMoneyChosen, sendNotify} <- chooseGimmeTheMoney
  StandardContractNotified{notified, makeReturnDeposit} <- sendNotify
  StandardContractClosed{returnDeposited, withdrawPartyAFunds} <- makeReturnDeposit
  (_, _) <- withdrawPartyAFunds
  createContractId <- case contractCreated of
    Web.CreateTxEnvelope{contractId} -> pure contractId
  transactionId1 <- case initialFundsDeposited of
    Web.ApplyInputsTxEnvelope{transactionId} -> pure transactionId
  transactionId2 <- case gimmeTheMoneyChosen of
    Web.ApplyInputsTxEnvelope{transactionId} -> pure transactionId
  transactionId3 <- case notified of
    Web.ApplyInputsTxEnvelope{transactionId} -> pure transactionId
  transactionId4 <- case returnDeposited of
    Web.ApplyInputsTxEnvelope{transactionId} -> pure transactionId
  let transactionIds = [transactionId1, transactionId2, transactionId3, transactionId4]
  pure (createContractId, transactionIds)
