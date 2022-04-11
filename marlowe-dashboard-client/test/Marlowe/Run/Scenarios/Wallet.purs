module Test.Marlowe.Run.Action.Scenarios.Wallet where

import Prologue

import Control.Monad.UUID (generateUUID)
import Data.BigInt.Argonaut as BigInt
import Data.Map as Map
import Data.WalletNickname as WN
import Language.Marlowe.Client (ContractHistory(..))
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Marlowe.Semantics (Contract(..))
import Marlowe.Time (unixEpoch)
import MarloweContract (MarloweContract(..))
import Plutus.V1.Ledger.Address (Address(..))
import Plutus.V1.Ledger.Credential (Credential(..))
import Test.Data.Marlowe
  ( expectJust
  , makeTestWalletNickname
  , marloweData
  , newMarloweParams
  , newMnemonicPhrase
  , newWalletInfo
  , semanticState
  )
import Test.Marlowe.Run (fundWallet, marloweRunTest)
import Test.Marlowe.Run.Action.Flows (createWallet, dropWallet, restoreWallet)
import Test.Marlowe.Run.Commands
  ( handlePostActivate
  , openMyWalletDialog
  , recvInstanceSubscribe
  , sendWalletCompanionUpdate
  )
import Test.Marlowe.Run.Queries
  ( getWalletBalance
  , getWalletName
  , getWalletStatus
  )
import Test.Network.HTTP (expectNoRequest)
import Test.Spec (Spec)
import Test.Web.DOM.Assertions (shouldHaveText, shouldHaveTextM)

createAndRestoreWallet :: Spec Unit
createAndRestoreWallet = marloweRunTest "Create and Restore Wallet" do
  -- Arrange
  walletNickname <- makeTestWalletNickname "Wallet1"
  mnemonic <- newMnemonicPhrase
  walletInfo <- newWalletInfo

  -- Act
  _ <- createWallet walletNickname mnemonic walletInfo

  -- Assert
  openMyWalletDialog do
    getWalletName `shouldHaveTextM` WN.toString walletNickname
    balanceElement <- getWalletBalance
    statusElement <- getWalletStatus

    balanceElement `shouldHaveText` "₳ 0.000000"
    statusElement `shouldHaveText` "Out of sync"

    fundWallet walletNickname "" "" (BigInt.fromInt 1000000000) true

    balanceElement `shouldHaveText` "₳ 1,000.000000"
    statusElement `shouldHaveText` "Synchronized"

  -- Act
  dropWallet walletInfo
  _ <- restoreWallet walletNickname []

  -- Assert
  openMyWalletDialog do
    getWalletName `shouldHaveTextM` WN.toString walletNickname
    balanceElement <- getWalletBalance
    statusElement <- getWalletStatus

    balanceElement `shouldHaveText` "₳ 1,000.000000"
    statusElement `shouldHaveText` "Synchronized"

  dropWallet walletInfo

multipleCompanionUpdates :: Spec Unit
multipleCompanionUpdates = marloweRunTest "Receiving multiple companion updates"
  do
    -- Arrange
    walletNickname <- makeTestWalletNickname "Wallet1"
    mnemonic <- newMnemonicPhrase
    walletInfo@(WalletInfo { walletId }) <- newWalletInfo
    let emptyMarloweData = marloweData Close $ semanticState [] [] [] unixEpoch
    contract1 <- Tuple <$> newMarloweParams <@> emptyMarloweData
    contract2 <- Tuple <$> newMarloweParams <@> emptyMarloweData
    contract3 <- Tuple <$> newMarloweParams <@> emptyMarloweData
    followerId2 <- generateUUID
    followerId3 <- generateUUID

    -- Act
    _ <- createWallet walletNickname mnemonic walletInfo
    dropWallet walletInfo
    appIds <- restoreWallet walletNickname
      [ ContractHistory
          { chAddress: Address
              { addressCredential: ScriptCredential ""
              , addressStakingCredential: Nothing
              }
          , chParams: fst contract1
          , chInitialData: emptyMarloweData
          , chHistory: []
          }
      ]
    followerId1 <- expectJust "followerId not found" $ Map.lookup
      (fst contract1)
      appIds.followerAppIds
    recvInstanceSubscribe followerId1
    sendWalletCompanionUpdate appIds.walletCompanionId [ contract1, contract2 ]
    sendWalletCompanionUpdate appIds.walletCompanionId
      [ contract1, contract2, contract3 ]
    handlePostActivate walletId MarloweFollower followerId2
    recvInstanceSubscribe followerId2
    handlePostActivate walletId MarloweFollower followerId3
    recvInstanceSubscribe followerId3

    --Assert
    expectNoRequest
