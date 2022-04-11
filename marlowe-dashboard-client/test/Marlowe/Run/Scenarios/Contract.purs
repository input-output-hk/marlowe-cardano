module Test.Marlowe.Run.Action.Scenarios.Contract (contractScenarios) where

import Prologue

import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.UUID (class MonadUUID, generateUUID)
import Data.Address (Address)
import Data.AddressBook (AddressBook(..))
import Data.Array.NonEmpty as AN
import Data.Bimap as Bimap
import Data.Map (Map)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.PubKeyHash (PubKeyHash)
import Data.String.Regex.Flags (ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Time.Duration (Minutes(..), Seconds(..))
import Data.Traversable (traverse)
import Data.UUID.Argonaut (UUID)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect.Aff (Error)
import Language.Marlowe.Client (MarloweError(..))
import MainFrame.Types as MF
import Marlowe.Semantics (Assets, Contract, MarloweParams)
import Marlowe.Semantics as Semantics
import MarloweContract (MarloweContract(..))
import Test.Control.Monad.Time (class MonadMockTime, advanceTime)
import Test.Control.Monad.UUID (class MonadMockUUID, getLastUUID)
import Test.Data.Marlowe
  ( adjustInstant
  , contractHistory
  , followerEndpoints
  , iDepositRoleAda
  , makeTestContractNickname
  , makeTestWalletNickname
  , marloweData
  , newAddress
  , newMarloweParams
  , timeInterval
  , transactionInput
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
  ( clickConfirm
  , handleGetRoleToken
  , handlePostActivate
  , handlePostApplyInputs
  , handlePostFollow
  , recvInstanceSubscribe
  , sendApplyInputsSuccess
  , sendCreateException
  , sendCreateSuccess
  , sendFollowerUpdate
  , sendNewActiveEndpoints
  , sendWalletCompanionUpdate
  )
import Test.Network.HTTP (class MonadMockHTTP)
import Test.Spec (Spec)
import Test.Spec.Assertions (expectError)
import Test.Web.DOM.Assertions
  ( shouldBeDisabled
  , shouldCast
  , shouldHaveText
  , shouldNotBeDisabled
  )
import Test.Web.DOM.Query (findBy, getAllBy, getBy, nameRegexi, role, text)
import Test.Web.Event.User (click, clickM)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))

contractScenarios :: Spec Unit
contractScenarios = do
  loanContract
  startContractCompanionBeforeMarloweApp
  startContractMarloweAppBeforeCompanion
  startContractMarloweAppHangs
  startContractMarloweAppFails
  loanContractTimeout

loanContractTimeout :: Spec Unit
loanContractTimeout = loanContractTest
  "Time a loan contract out"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    lenderApps <- restoreWallet lenderNickname []
    lenderWallet <- getWallet lenderNickname
    marloweParams <- newMarloweParams
    contractNickname <- makeTestContractNickname "Test loan"
    startTime <- now
    { followerId, marloweData: datum } <- createLoan
      lenderWallet
      lenderApps
      marloweParams
      contractNickname
      borrowerNickname
      lenderNickname
      1000
      100
    advanceTime $ Minutes 10.0
    card <- getBy role do
      nameRegexi "Test loan"
      pure Listitem
    withContainer card do
      void $ findBy text $ pure "Timed out"
      void $ findBy text $ pure "Lender (you)"
      void $ findBy text $ pure "Borrower (Borrower)"
      closeButtons <- traverse shouldCast =<< getAllBy role do
        nameRegexi "close contract"
        pure Button
      let lenderButton = AN.head closeButtons
      let borrowerButton = AN.last closeButtons
      lenderButton `shouldHaveText` "Close contract"
      borrowerButton `shouldHaveText` "Close contract"
      shouldNotBeDisabled lenderButton
      shouldBeDisabled borrowerButton
      click lenderButton

    confirmDialog <- findBy role $ pure Dialog
    withContainer confirmDialog clickConfirm
    pure unit

    intervalMin <- adjustInstant (Minutes (10.0)) startTime
    let intervalMax = top
    reqId <- getLastUUID
    let input = transactionInput (timeInterval intervalMin intervalMax) []
    handlePostApplyInputs lenderApps.marloweAppId reqId marloweParams input
    sendApplyInputsSuccess lenderApps.marloweAppId reqId
    sendFollowerUpdate followerId
      $ contractHistory marloweParams datum [ input ]

    clickM $ getBy role do
      nameRegexi "completed contracts"
      pure Link

    completedCard <- findBy role do
      nameRegexi "Test loan"
      pure Listitem

    void
      $ withContainer completedCard
      $ findBy text
      $ pure "This contract is now closed"

    clickM $ getBy role do
      nameRegexi "running contracts"
      pure Link

    expectError $ getBy role do
      nameRegexi "Test loan"
      pure Listitem

loanContract :: Spec Unit
loanContract = loanContractTest
  "Run a loan contract"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    lenderApps <- restoreWallet lenderNickname []
    lenderWallet <- getWallet lenderNickname
    marloweParams <- newMarloweParams
    contractNickname <- makeTestContractNickname "Test loan"
    startTime <- now
    { followerId, marloweData: datum } <- createLoan
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
    withContainer card do
      void $ findBy text $ pure "Current step:1"
      void $ findBy text $ pure "Lender (you)"
      timeoutText <- getBy text $ pure $ unsafeRegex "left" ignoreCase
      timeoutText `shouldHaveText` "10min 0s left"
      advanceTime (Minutes 1.5)
      timeoutText `shouldHaveText` "8min 30s left"
      depositButton <- shouldCast =<< getBy role do
        nameRegexi "deposit"
        pure Button
      depositButton `shouldHaveText` "Deposit:₳ 1,000.000000"
      shouldNotBeDisabled depositButton
      click depositButton

    confirmDialog <- findBy role $ pure Dialog
    withContainer confirmDialog clickConfirm
    pure unit

    intervalMin <- adjustInstant (Minutes (-2.0)) startTime
    intervalMax <- adjustInstant (Seconds (-1.0))
      =<< adjustInstant (Minutes 10.0) startTime
    reqId <- getLastUUID
    let
      input = transactionInput
        (timeInterval intervalMin intervalMax)
        [ iDepositRoleAda "Lender" "Lender" 1000 ]
    handlePostApplyInputs lenderApps.marloweAppId reqId marloweParams input
    sendFollowerUpdate followerId
      $ contractHistory marloweParams datum [ input ]
    sendApplyInputsSuccess lenderApps.marloweAppId reqId

    withContainer card do
      void $ findBy text $ pure "Current step:2"
      void $ findBy text $ pure "Borrower (Borrower)"
      timeoutText <- getBy text $ pure $ unsafeRegex "left" ignoreCase
      timeoutText `shouldHaveText` "23min 30s left"
      advanceTime (Minutes 1.5)
      timeoutText `shouldHaveText` "22min 0s left"
      depositButton <- shouldCast =<< getBy role do
        nameRegexi "deposit"
        pure Button
      depositButton `shouldHaveText` "Deposit:₳ 1,100.000000"
      shouldBeDisabled depositButton

startContractCompanionBeforeMarloweApp :: Spec Unit
startContractCompanionBeforeMarloweApp = loanContractTest
  "The wallet companion responds before the marlowe app"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    { contract, contractState, reqId, lenderApps, marloweParams, lenderWallet } <-
      setupLoanWithoutNotifications lenderNickname borrowerNickname
    -- Act
    assertStartingContractShown
    sendWalletCompanionUpdate lenderApps.walletCompanionId
      [ Tuple marloweParams $ marloweData contract contractState
      ]
    assertStartingContractShown
    followerId <- generateUUID
    handlePostActivate lenderWallet.walletId MarloweFollower followerId
    assertStartingContractShown
    recvInstanceSubscribe followerId
    sendNewActiveEndpoints followerId followerEndpoints
    handlePostFollow followerId marloweParams
    assertStartingContractShown
    sendCreateSuccess lenderApps.marloweAppId reqId marloweParams
    assertStartingContractShown
    sendFollowerUpdate followerId
      $ contractHistory marloweParams (marloweData contract contractState) []
    handleGetRoleToken marloweParams "Borrower" borrowerNickname
    handleGetRoleToken marloweParams "Lender" lenderNickname
    assertStartedContractShown
    expectError assertStartingContractShown

startContractMarloweAppHangs :: Spec Unit
startContractMarloweAppHangs = loanContractTest
  "The MarloweApp hangs"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    { contract, contractState, lenderApps, marloweParams, lenderWallet } <-
      setupLoanWithoutNotifications lenderNickname borrowerNickname
    -- Act
    assertStartingContractShown
    sendWalletCompanionUpdate lenderApps.walletCompanionId
      [ Tuple marloweParams $ marloweData contract contractState
      ]
    assertStartingContractShown
    followerId <- generateUUID
    handlePostActivate lenderWallet.walletId MarloweFollower followerId
    assertStartingContractShown
    recvInstanceSubscribe followerId
    sendNewActiveEndpoints followerId followerEndpoints
    handlePostFollow followerId marloweParams
    assertStartingContractShown
    sendFollowerUpdate followerId
      $ contractHistory marloweParams (marloweData contract contractState) []
    handleGetRoleToken marloweParams "Borrower" borrowerNickname
    handleGetRoleToken marloweParams "Lender" lenderNickname
    assertStartedContractShown
    expectError assertStartingContractShown

startContractMarloweAppBeforeCompanion :: Spec Unit
startContractMarloweAppBeforeCompanion = loanContractTest
  "The marlowe app responds before the wallet companion"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    { contract, contractState, reqId, lenderApps, marloweParams, lenderWallet } <-
      setupLoanWithoutNotifications
        lenderNickname
        borrowerNickname
    -- Act
    assertStartingContractShown
    sendCreateSuccess lenderApps.marloweAppId reqId marloweParams
    assertStartingContractShown
    sendWalletCompanionUpdate lenderApps.walletCompanionId
      [ Tuple marloweParams $ marloweData contract contractState
      ]
    assertStartingContractShown
    followerId <- generateUUID
    handlePostActivate lenderWallet.walletId MarloweFollower followerId
    assertStartingContractShown
    recvInstanceSubscribe followerId
    sendNewActiveEndpoints followerId followerEndpoints
    handlePostFollow followerId marloweParams
    assertStartingContractShown
    sendFollowerUpdate followerId
      $ contractHistory marloweParams (marloweData contract contractState) []
    handleGetRoleToken marloweParams "Borrower" borrowerNickname
    handleGetRoleToken marloweParams "Lender" lenderNickname
    assertStartedContractShown
    expectError assertStartingContractShown

startContractMarloweAppFails :: Spec Unit
startContractMarloweAppFails = loanContractTest
  "The marlowe app sends a creation failure response"
  \lenderNickname borrowerNickname -> do
    -- Arrange
    { reqId, lenderApps } <-
      setupLoanWithoutNotifications
        lenderNickname
        borrowerNickname
    -- Act
    assertStartingContractShown
    sendCreateException lenderApps.marloweAppId reqId TransitionError
    assertFailedContractShown
    expectError assertStartingContractShown

assertStartingContractShown
  :: forall m. MonadTest m => MonadError Error m => m Unit
assertStartingContractShown = do
  card <- findBy role do
    nameRegexi "Test loan"
    pure Listitem
  withContainer card $ void $ findBy text $ pure "Starting contract…"

assertFailedContractShown
  :: forall m. MonadTest m => MonadError Error m => m Unit
assertFailedContractShown = do
  card <- findBy role do
    nameRegexi "Test loan"
    pure Alert
  withContainer card $ void $ findBy text $ pure "Failed to start contract"

assertStartedContractShown
  :: forall m. MonadTest m => MonadError Error m => m Unit
assertStartedContractShown = do
  card <- findBy role do
    nameRegexi "Test loan"
    pure Listitem
  withContainer card $ void $ findBy text $ pure "Current step:1"

setupLoanWithoutNotifications
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadUUID m
  => MonadLogger String m
  => MonadMockHTTP m
  => MonadError Error m
  => MonadReader Coenv m
  => MonadTime m
  => MonadMockUUID m
  => WalletNickname
  -> WalletNickname
  -> m
       { contract :: Contract
       , contractState :: Semantics.State
       , lenderApps ::
           { followerAppIds :: Map MarloweParams UUID
           , marloweAppId :: UUID
           , walletCompanionId :: UUID
           }
       , lenderWallet ::
           { address :: Address
           , assets :: Assets
           , mnemonic :: MnemonicPhrase
           , nickname :: WalletNickname
           , pubKeyHash :: PubKeyHash
           , walletId :: WalletId
           }
       , marloweParams :: MarloweParams
       , reqId :: UUID
       }
setupLoanWithoutNotifications lenderNickname borrowerNickname = do
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
  pure
    { contract, contractState, reqId, lenderApps, lenderWallet, marloweParams }

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
