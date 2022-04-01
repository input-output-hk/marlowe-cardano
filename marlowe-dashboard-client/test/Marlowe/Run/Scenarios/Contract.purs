module Test.Marlowe.Run.Action.Scenarios.Contract (contractScenarios) where

import Prologue

import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader (class MonadReader)
import Control.Monad.UUID (class MonadUUID, generateUUID)
import Data.Address (Address)
import Data.AddressBook (AddressBook(..))
import Data.Bimap as Bimap
import Data.Map (Map)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.PubKeyHash (PubKeyHash)
import Data.UUID.Argonaut (UUID)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect.Aff (Error)
import Language.Marlowe.Client (ContractHistory(..), MarloweError(..))
import MainFrame.Types as MF
import Marlowe.Semantics (Assets, Contract, MarloweParams)
import Marlowe.Semantics as Semantics
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
  , handlePostActivate
  , handlePostFollow
  , recvInstanceSubscribe
  , sendCreateException
  , sendCreateSuccess
  , sendFollowerUpdate
  , sendNewActiveEndpoints
  , sendWalletCompanionUpdate
  )
import Test.Network.HTTP (class MonadMockHTTP)
import Test.Spec (Spec)
import Test.Spec.Assertions (expectError)
import Test.Web.DOM.Query (findBy, getBy, nameRegexi, role, text)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))

contractScenarios :: Spec Unit
contractScenarios = do
  loanContract
  startContractCompanionBeforeMarloweApp
  startContractMarloweAppBeforeCompanion
  startContractMarloweAppFails

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

-- confirmDialog <- findBy role $ pure Dialog
-- withContainer confirmDialog clickConfirm
-- pure unit

-- reqId <- getNextUUID
-- let
--   interval = TimeInterval (POSIXTime unixEpoch) (POSIXTime unixEpoch)
--   depositInput = TransactionInput { inputs: mempty, interval }
-- handlePostApplyInputs lenderApps.marloweAppId reqId marloweParams depositInput

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
    sendFollowerUpdate followerId $ ContractHistory
      { chAddress: PAB.Address
          { addressCredential: ScriptCredential ""
          , addressStakingCredential: Nothing
          }
      , chParams: marloweParams
      , chInitialData: marloweData contract contractState
      , chHistory: []
      }
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
    sendFollowerUpdate followerId $ ContractHistory
      { chAddress: PAB.Address
          { addressCredential: ScriptCredential ""
          , addressStakingCredential: Nothing
          }
      , chParams: marloweParams
      , chInitialData: marloweData contract contractState
      , chHistory: []
      }
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
