module Test.Marlowe.Run.Action.Scenarios.Contract (contractScenarios) where

import Prologue

import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.UUID (class MonadUUID, generateUUID)
import Data.AddressBook (AddressBook(..))
import Data.Bimap as Bimap
import Data.Time.Duration (Milliseconds(..))
import Data.WalletNickname (WalletNickname)
import Effect.Aff (Error, delay)
import Effect.Aff.Class (liftAff)
import Language.Marlowe.Client (ContractHistory(..))
import MainFrame.Types as MF
import MarloweContract (MarloweContract(..))
import Plutus.V1.Ledger.Address as PAB
import Plutus.V1.Ledger.Credential (Credential(..))
import Test.Control.Monad.Time (class MonadMockTime)
import Test.Control.Monad.UUID (class MonadMockUUID)
import Test.Data.Marlowe
  ( followerEndpoints
  , makeTestContractNickname
  , makeTestWalletNickname
  , marloweData
  , newAddress
  , newMarloweParams
  )
import Test.Halogen (class MonadHalogenTest)
import Test.Marlowe.Run
  ( Coenv
  , defaultTestParameters
  , getWallet
  , marloweRunTestWith
  )
import Test.Marlowe.Run.Action.Flows
  ( createLoan
  , createLoanWithoutUpdates
  , restoreWallet
  )
import Test.Marlowe.Run.Commands
  ( clickButtonRegex
  , clickConfirm
  , handlePostActivate
  , handlePostFollow
  , recvInstanceSubscribe
  , sendCreateSuccess
  , sendFollowerUpdate
  , sendNewActiveEndpoints
  , sendWalletCompanionUpdate
  )
import Test.Network.HTTP (class MonadMockHTTP)
import Test.Spec (Spec, focus)
import Test.Spec.Assertions (expectError)
import Test.Web.DOM.Query (findBy, getBy, nameRegexi, role, text)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))

contractScenarios :: Spec Unit
contractScenarios = do
  loanContract
  startContractCompanionBeforeMarloweApp
  startContractMarloweAppBeforeCompanion

loanContract :: Spec Unit
loanContract = loanContractTest
  "Run a loan contract"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    lenderApps <- restoreWallet lenderNickname []
    lenderWallet <- getWallet lenderNickname
    marloweParams <- newMarloweParams
    contractNickname <- makeTestContractNickname "Test loan"
    _followerId <- createLoan
      lenderWallet
      lenderApps
      marloweParams
      contractNickname
      borrowerNickname
      lenderNickname
      1000
      100
    card <- getBy role do
      nameRegexi "Test loan"
      pure Listitem
    withContainer card $ clickButtonRegex "deposit"
    confirmDialog <- findBy role $ pure Dialog
    withContainer confirmDialog clickConfirm
    pure unit

-- reqId <- getNextUUID
-- let
--   interval = TimeInterval (POSIXTime unixEpoch) (POSIXTime unixEpoch)
--   depositInput = TransactionInput { inputs: mempty, interval }
-- handlePostApplyInputs lenderApps.marloweAppId reqId marloweParams depositInput

startContractCompanionBeforeMarloweApp :: Spec Unit
startContractCompanionBeforeMarloweApp =
  focus $ loanContractTest
    "The wallet companion responds before the marlowe app"
    \lenderNickname borrowerNickname -> do
      -- Arrange
      lenderApps <- restoreWallet lenderNickname []
      lenderWallet <- getWallet lenderNickname
      marloweParams <- newMarloweParams
      contractNickname <- makeTestContractNickname "Test loan"
      -- Act
      { contract, contractState, reqId } <- createLoanWithoutUpdates
        lenderWallet
        lenderApps
        marloweParams
        contractNickname
        borrowerNickname
        lenderNickname
        1000
        100
      -- TODO remove this when race condition for awaitMarloweParams is fixed
      liftAff $ delay $ Milliseconds 1000.0
      canFindStartingContractCard
      sendWalletCompanionUpdate lenderApps.walletCompanionId
        [ Tuple marloweParams $ marloweData contract contractState
        ]
      followerId <- generateUUID
      handlePostActivate lenderWallet.walletId MarloweFollower followerId
      recvInstanceSubscribe followerId
      sendNewActiveEndpoints followerId followerEndpoints
      handlePostFollow followerId marloweParams
      canFindStartingContractCard
      sendCreateSuccess lenderApps.marloweAppId reqId marloweParams
      sendFollowerUpdate followerId $ ContractHistory
        { chAddress: PAB.Address
            { addressCredential: ScriptCredential ""
            , addressStakingCredential: Nothing
            }
        , chParams: marloweParams
        , chInitialData: marloweData contract contractState
        , chHistory: []
        }
      expectError canFindStartingContractCard
      canFindStartedContractCard
  where
  canFindStartingContractCard
    :: forall m. MonadTest m => MonadError Error m => m Unit
  canFindStartingContractCard = do
    card <- getBy role do
      nameRegexi "Test loan"
      pure Listitem
    withContainer card $ void $ getBy text $ pure "Starting contract…"

  canFindStartedContractCard
    :: forall m. MonadTest m => MonadError Error m => m Unit
  canFindStartedContractCard = do
    card <- getBy role do
      nameRegexi "Test loan"
      pure Listitem
    withContainer card $ void $ getBy text $ pure "Current step:1"

startContractMarloweAppBeforeCompanion :: Spec Unit
startContractMarloweAppBeforeCompanion =
  focus $ loanContractTest
    "The marlowe app responds before the wallet companion"
    \lenderNickname borrowerNickname -> do
      -- Arrange
      lenderApps <- restoreWallet lenderNickname []
      lenderWallet <- getWallet lenderNickname
      marloweParams <- newMarloweParams
      contractNickname <- makeTestContractNickname "Test loan"
      -- Act
      { contract, contractState, reqId } <- createLoanWithoutUpdates
        lenderWallet
        lenderApps
        marloweParams
        contractNickname
        borrowerNickname
        lenderNickname
        1000
        100
      -- TODO remove this when race condition for awaitMarloweParams is fixed
      liftAff $ delay $ Milliseconds 1000.0
      canFindStartingContractCard
      sendCreateSuccess lenderApps.marloweAppId reqId marloweParams
      followerId <- generateUUID
      handlePostActivate lenderWallet.walletId MarloweFollower followerId
      recvInstanceSubscribe followerId
      sendNewActiveEndpoints followerId followerEndpoints
      handlePostFollow followerId marloweParams
      sendWalletCompanionUpdate lenderApps.walletCompanionId
        [ Tuple marloweParams $ marloweData contract contractState
        ]
      sendFollowerUpdate followerId $ ContractHistory
        { chAddress: PAB.Address
            { addressCredential: ScriptCredential ""
            , addressStakingCredential: Nothing
            }
        , chParams: marloweParams
        , chInitialData: marloweData contract contractState
        , chHistory: []
        }
      expectError canFindStartingContractCard
      canFindStartedContractCard
  where
  canFindStartingContractCard
    :: forall m. MonadTest m => MonadError Error m => m Unit
  canFindStartingContractCard = do
    card <- getBy role do
      nameRegexi "Test loan"
      pure Listitem
    withContainer card $ void $ getBy text $ pure "Starting contract…"

  canFindStartedContractCard
    :: forall m. MonadTest m => MonadError Error m => m Unit
  canFindStartedContractCard = do
    card <- getBy role do
      nameRegexi "Test loan"
      pure Listitem
    withContainer card $ void $ getBy text $ pure "Current step:1"

loanContractTest
  :: String
  -> ( WalletNickname
       -> WalletNickname
       -> forall m
        . MonadReader Coenv m
       => MonadError Error m
       => MonadHalogenTest MF.Query Unit MF.Msg m
       => MonadMockHTTP m
       => MonadMockTime m
       => MonadMockUUID m
       => MonadUUID m
       => MonadTime m
       => MonadLogger String m
       => m Unit
     )
  -> Spec Unit
loanContractTest title action = marloweRunTestWith title setupWallets do
  lenderNickname <- makeTestWalletNickname "Lender"
  borrowerNickname <- makeTestWalletNickname "Borrower"
  action lenderNickname borrowerNickname
  where
  setupWallets = do
    borrowerNickname <- makeTestWalletNickname "Borrower"
    lenderNickname <- makeTestWalletNickname "Lender"
    borrowerAddress <- newAddress
    lenderAddress <- newAddress
    pure $ defaultTestParameters
      { addressBook = AddressBook
          $ Bimap.fromFoldable
              [ Tuple borrowerNickname borrowerAddress
              , Tuple lenderNickname lenderAddress
              ]
      }
