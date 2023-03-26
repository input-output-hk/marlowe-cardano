{-# LANGUAGE DataKinds #-}

module Language.Marlowe.Runtime.Web.StandardContract
  where

import Control.Monad.RWS.Strict (MonadIO(liftIO))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time
  (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Language.Marlowe.Core.V1.Semantics.Types
import Language.Marlowe.Extended.V1 (ada)
import Language.Marlowe.Runtime.Integration.Common (Wallet(..), expectJust)
import Language.Marlowe.Runtime.Plutus.V2.Api (toPlutusAddress)
import Language.Marlowe.Runtime.Transaction.Api (WalletAddresses(..))
import Language.Marlowe.Runtime.Web
  (ApplyInputsTxBody, BlockHeader, CreateTxBody, RoleTokenConfig(RoleTokenSimple), WithdrawTxBody)
import qualified Language.Marlowe.Runtime.Web as Web
import Language.Marlowe.Runtime.Web.Client (postContract)
import Language.Marlowe.Runtime.Web.Common
  (choose, deposit, notify, submitContract, submitTransaction, submitWithdrawal, withdraw)
import Language.Marlowe.Runtime.Web.Server.DTO (ToDTO(toDTO))
import qualified Plutus.V2.Ledger.Api as PV2
import Servant.Client (ClientM)

data StandardContractInit = StandardContractInit
  { makeInitialDeposit :: ClientM StandardContractFundsDeposited
  , contractCreated :: CreateTxBody
  , createdBlock :: BlockHeader
  }

data StandardContractFundsDeposited  = StandardContractFundsDeposited
  { chooseGimmeTheMoney :: ClientM StandardContractChoiceMade
  , initialFundsDeposited :: ApplyInputsTxBody
  , initialDepositBlock :: BlockHeader
  }

data StandardContractChoiceMade  = StandardContractChoiceMade
  { sendNotify :: ClientM StandardContractNotified
  , gimmeTheMoneyChosen :: ApplyInputsTxBody
  , choiceBlock :: BlockHeader
  }

data StandardContractNotified = StandardContractNotified
  { makeReturnDeposit :: ClientM StandardContractClosed
  , notified :: ApplyInputsTxBody
  , notifiedBlock :: BlockHeader
  }

data StandardContractClosed = StandardContractClosed
  { withdrawPartyAFunds :: ClientM (WithdrawTxBody, BlockHeader)
  , returnDeposited :: ApplyInputsTxBody
  , returnDepositBlock :: BlockHeader
  }

createStandardContract :: Wallet -> Wallet -> ClientM StandardContractInit
createStandardContract partyAWallet partyBWallet = do
  let partyAWalletAddresses = addresses partyAWallet
  let partyAWebChangeAddress = toDTO $ changeAddress partyAWalletAddresses
  let partyAWebExtraAddresses = Set.map toDTO $ extraAddresses partyAWalletAddresses
  let partyAWebCollataralUtxos = Set.map toDTO $ collateralUtxos partyAWalletAddresses

  let partyBWalletAddresses = addresses partyBWallet

  partyBAddress <- liftIO $ expectJust "Failed to convert party B address" $ toPlutusAddress $ changeAddress partyBWalletAddresses
  now <- liftIO getCurrentTime
  let (contract, partyA, partyB) = standardContract partyBAddress now $ secondsToNominalDiffTime 100

  contractCreated@Web.CreateTxBody{contractId} <- postContract
    partyAWebChangeAddress
    (Just partyAWebExtraAddresses)
    (Just partyAWebCollataralUtxos)
    Web.PostContractsRequest
      { metadata = mempty
      , version = Web.V1
      , roles = Just $ Web.Mint $ Map.singleton "PartyA" $ RoleTokenSimple partyAWebChangeAddress
      , contract = contract
      , minUTxODeposit = 2_000_000
      , tags = mempty
      }

  createdBlock <- submitContract partyAWallet contractCreated

  pure StandardContractInit
    { createdBlock
    , contractCreated
    , makeInitialDeposit = do
        initialFundsDeposited <- deposit
          partyAWallet
          contractId
          partyA
          partyA
          ada
          100_000_000
        initialDepositBlock <- submitTransaction partyAWallet initialFundsDeposited

        pure StandardContractFundsDeposited
          { initialDepositBlock
          , initialFundsDeposited
          , chooseGimmeTheMoney = do
              gimmeTheMoneyChosen <- choose
                partyBWallet
                contractId
                "Gimme the money"
                partyB
                0
              choiceBlock <- submitTransaction partyBWallet gimmeTheMoneyChosen

              pure StandardContractChoiceMade
                { choiceBlock
                , gimmeTheMoneyChosen
                , sendNotify = do
                    notified <- notify partyAWallet contractId
                    notifiedBlock <- submitTransaction partyAWallet notified

                    pure StandardContractNotified
                      { notifiedBlock
                      , notified
                      , makeReturnDeposit = do
                          returnDeposited <- deposit
                            partyBWallet
                            contractId
                            partyA
                            partyB
                            ada
                            100_000_000
                          returnDepositBlock <- submitTransaction partyBWallet returnDeposited

                          pure StandardContractClosed
                            { returnDepositBlock
                            , returnDeposited
                            , withdrawPartyAFunds = do
                                withdrawTxBody <- withdraw partyAWallet contractId "PartyA"
                                (withdrawTxBody,) <$> submitWithdrawal partyAWallet withdrawTxBody
                            }
                      }
                }
          }
    }

standardContract
  :: PV2.Address
  -> UTCTime
  -> NominalDiffTime
  -> (Contract, Party, Party)
standardContract partyBAddress startTime timeoutLength = (contract, partyA, partyB)
  where
    toPosixTime t = PV2.POSIXTime $ floor $ 1000 * nominalDiffTimeToSeconds (utcTimeToPOSIXSeconds t)
    contract = When
      [ Case (Deposit partyA partyA ada (Constant 100_000_000))
          ( When
              [ Case (Choice (ChoiceId "Gimme the money" partyB) [Bound 0 0])
                  ( When
                      [ Case (Notify TrueObs)
                          ( Pay partyA (Party partyB) ada (AvailableMoney partyA ada)
                              ( When
                                  [ Case (Deposit partyA partyB ada (Constant 100_000_000)) Close
                                  ]
                                  (toPosixTime $ timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` (timeoutLength `addUTCTime` startTime))))
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


    partyA = Role "PartyA"
    -- 0x00 = testnet
    partyB = Address (toEnum 0x00) partyBAddress

createFullyExecutedStandardContract :: Wallet -> Wallet -> ClientM (Web.TxOutRef, [Web.TxId])
createFullyExecutedStandardContract partyAWallet partyBWallet = do
    StandardContractInit{contractCreated, makeInitialDeposit} <- createStandardContract partyAWallet partyBWallet
    StandardContractFundsDeposited{initialFundsDeposited, chooseGimmeTheMoney} <- makeInitialDeposit
    StandardContractChoiceMade{gimmeTheMoneyChosen, sendNotify} <- chooseGimmeTheMoney
    StandardContractNotified{notified, makeReturnDeposit} <- sendNotify
    StandardContractClosed{returnDeposited, withdrawPartyAFunds} <- makeReturnDeposit
    (_, _) <- withdrawPartyAFunds
    createContractId <- case contractCreated of
      Web.CreateTxBody{contractId} -> pure contractId
    transactionId1 <- case initialFundsDeposited of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    transactionId2 <- case gimmeTheMoneyChosen of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    transactionId3 <- case notified of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    transactionId4 <- case returnDeposited of
      Web.ApplyInputsTxBody{transactionId} -> pure transactionId
    let
      transactionIds = [transactionId1, transactionId2, transactionId3, transactionId4]
    pure (createContractId, transactionIds)
