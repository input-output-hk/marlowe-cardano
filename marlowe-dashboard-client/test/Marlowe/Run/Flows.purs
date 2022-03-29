-- Flows are reusable sequences of commands that are intended to setup tests
-- for assertion easily.
module Test.Marlowe.Run.Action.Flows where

import Prologue

import Control.Logger.Capability (class MonadLogger)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk)
import Control.Monad.UUID (class MonadUUID, generateUUID)
import Data.Address (Address)
import Data.Address as Address
import Data.Argonaut (jsonEmptyArray)
import Data.Bifunctor (lmap)
import Data.ContractNickname (ContractNickname)
import Data.ContractNickname as CN
import Data.Foldable (traverse_)
import Data.Int (decimal)
import Data.Int as Int
import Data.Lens (takeBoth, traversed, (^..))
import Data.Map (Map)
import Data.Map as Map
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.PaymentPubKeyHash as PPKH
import Data.PubKeyHash as PK
import Data.Time.Duration (Minutes(..))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut (UUID)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff (Error)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Client (_chInitialData, _chParams, getMarloweParams)
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Marlowe.Semantics
  ( Assets(..)
  , CurrencySymbol
  , MarloweParams
  , Party(..)
  , ValidatorHash
  )
import MarloweContract (MarloweContract(..))
import Test.Control.Monad.UUID (class MonadMockUUID, getLastUUID)
import Test.Data.Marlowe
  ( adaToken
  , companionEndpoints
  , followerEndpoints
  , fromNow
  , loan
  , loanRoles
  , marloweAppEndpoints
  , marloweData
  , marloweParams
  , semanticState
  , walletCompantionState
  )
import Test.Data.Plutus (appInstanceActive)
import Test.Marlowe.Run
  ( Coenv
  , TestWallet
  , getWallet
  , sendWalletFunds
  , setWallet
  )
import Test.Marlowe.Run.Commands
  ( clickCreateWallet
  , clickDrop
  , clickLinkRegex
  , clickNewContact
  , clickOk
  , clickPayAndStart
  , clickRestoreWallet
  , clickReview
  , clickSave
  , clickSetup
  , handleGetContractInstances
  , handlePostActivate
  , handlePostCreate
  , handlePostCreateWallet
  , handlePostRestoreWallet
  , openContactsDialog
  , openGenerateDialog
  , openMyWalletDialog
  , openNewContractDialog
  , openRestoreDialog
  , recvInstanceSubscribe
  , sendCreateSuccess
  , sendFollowerUpdate
  , sendNewActiveEndpoints
  , sendWalletCompanionUpdate
  , typeAddress
  , typeContractTitle
  , typeContractValue
  , typeMnemonicPhrase
  , typeTextboxRegex
  , typeWalletNickname
  )
import Test.Network.HTTP (class MonadMockHTTP)
import Test.Web.DOM.Assertions (shouldHaveText)
import Test.Web.DOM.Query (findBy, role)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Web.ARIA (ARIARole(..))

createWallet
  :: forall m
   . MonadTest m
  => MonadError Error m
  => MonadLogger String m
  => MonadUser m
  => MonadUUID m
  => MonadAsk Coenv m
  => MonadMockHTTP m
  => WalletNickname
  -> MnemonicPhrase
  -> WalletInfo
  -> m { marloweAppId :: UUID, walletCompanionId :: UUID }
createWallet walletName mnemonic walletInfo = do
  let WalletInfo { walletId, pubKeyHash, address } = walletInfo
  openGenerateDialog do
    typeWalletNickname $ WN.toString walletName
    clickCreateWallet
    handlePostCreateWallet walletName mnemonic walletInfo

    mnemonicText <- findBy role $ pure Mark
    mnemonicText `shouldHaveText` MP.toString mnemonic
    clickOk
    typeMnemonicPhrase $ MP.toString mnemonic
    clickOk
    setWallet walletName
      { address
      , assets: Assets Map.empty
      , mnemonic
      , pubKeyHash: PPKH.toPubKeyHash pubKeyHash
      , walletId
      }
    handleGetContractInstances walletId []

    walletCompanionId <- generateUUID
    marloweAppId <- generateUUID
    handlePostActivate walletId WalletCompanion walletCompanionId
    handlePostActivate walletId MarloweApp marloweAppId
    recvInstanceSubscribe walletCompanionId
    sendNewActiveEndpoints walletCompanionId companionEndpoints
    recvInstanceSubscribe marloweAppId
    sendNewActiveEndpoints marloweAppId marloweAppEndpoints
    pure { marloweAppId, walletCompanionId }

restoreWallet
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadUUID m
  => MonadLogger String m
  => MonadMockHTTP m
  => MonadError Error m
  => MonadAsk Coenv m
  => WalletNickname
  -> Array ContractHistory
  -> m
       { marloweAppId :: UUID
       , walletCompanionId :: UUID
       , followerAppIds :: Map MarloweParams UUID
       }
restoreWallet walletName contracts = do
  { address, mnemonic, pubKeyHash, walletId } <- getWallet walletName
  openRestoreDialog do
    typeWalletNickname $ WN.toString walletName
    typeMnemonicPhrase $ MP.toString mnemonic
    clickRestoreWallet
    handlePostRestoreWallet walletName address mnemonic pubKeyHash walletId

    walletCompanionId <- generateUUID
    marloweAppId <- generateUUID
    followerIdsAndHistories <-
      traverse (\history -> Tuple history <$> generateUUID) contracts
    let
      companionContracts =
        contracts ^.. traversed <<< takeBoth _chParams _chInitialData
      marloweAppInstance =
        appInstanceActive walletId MarloweApp marloweAppId jsonEmptyArray
      walletCompanionInstance =
        appInstanceActive walletId WalletCompanion walletCompanionId
          $ walletCompantionState companionContracts
      followerInstances = map
        (uncurry $ flip $ appInstanceActive walletId MarloweFollower)
        followerIdsAndHistories
      followerAppIds = Map.fromFoldable
        $ map (lmap $ getMarloweParams) followerIdsAndHistories
      instances = join
        [ [ marloweAppInstance, walletCompanionInstance ]
        , followerInstances
        ]
    handleGetContractInstances walletId instances
    recvInstanceSubscribe walletCompanionId
    sendNewActiveEndpoints walletCompanionId companionEndpoints
    recvInstanceSubscribe marloweAppId
    sendNewActiveEndpoints marloweAppId marloweAppEndpoints
    sendWalletCompanionUpdate walletCompanionId companionContracts
    traverse_ (flip sendNewActiveEndpoints followerEndpoints) followerAppIds
    traverse_ (uncurry $ flip sendFollowerUpdate) followerIdsAndHistories
    sendWalletFunds walletName
    pure { marloweAppId, walletCompanionId, followerAppIds }

dropWallet
  :: forall m
   . MonadTest m
  => MonadError Error m
  => MonadLogger String m
  => MonadUser m
  => MonadAsk Coenv m
  => MonadMockHTTP m
  => WalletInfo
  -> m Unit
dropWallet (WalletInfo { walletId }) = do
  openMyWalletDialog clickDrop
  handleGetContractInstances walletId []

addContact
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => MonadLogger String m
  => WalletNickname
  -> Address
  -> m Unit
addContact walletName address = do
  openContactsDialog do
    clickNewContact
    typeWalletNickname $ WN.toString walletName
    typeAddress $ Address.toString address
    clickSave

createLoan
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => MonadTime m
  => MonadUUID m
  => MonadLogger String m
  => MonadAsk Coenv m
  => MonadMockUUID m
  => MonadMockHTTP m
  => TestWallet
  -> UUID
  -> UUID
  -> CurrencySymbol
  -> ValidatorHash
  -> ContractNickname
  -> WalletNickname
  -> WalletNickname
  -> Int
  -> Int
  -> m Unit
createLoan
  wallet
  marloweAppId
  walletCompanionId
  currencySymbol
  rolePayoutValidatorHash
  contractTitle
  borrower
  lender
  amount
  interest = do
  openNewContractDialog do
    clickLinkRegex "loan"
    clickSetup
    typeContractTitle $ CN.toString contractTitle
    typeTextboxRegex "borrower" $ WN.toString borrower
    typeTextboxRegex "lender" $ WN.toString lender
    typeContractValue "amount" $ Int.toStringAs decimal amount
    typeContractValue "interest" $ Int.toStringAs decimal interest
    clickReview
    clickPayAndStart
  reqId <- getLastUUID
  createdAt <- now
  loanDeadline <- fromNow (Minutes 10.0)
  repaymentDeadline <- fromNow (Minutes 25.0)
  let
    contract = loan loanDeadline repaymentDeadline amount interest
    params = marloweParams currencySymbol rolePayoutValidatorHash
    contractState = semanticState
      [ (PK $ PK.toString wallet.pubKeyHash) /\ adaToken /\ 200000 ]
      []
      []
      createdAt
  borrowerWallet <- getWallet borrower
  lenderWallet <- getWallet lender
  handlePostCreate marloweAppId reqId
    (loanRoles borrowerWallet.address lenderWallet.address)
    contract
  sendWalletCompanionUpdate walletCompanionId
    [ Tuple params $ marloweData contract contractState
    ]
  sendCreateSuccess marloweAppId reqId params
  followerId <- generateUUID
  handlePostActivate wallet.walletId MarloweFollower followerId
  recvInstanceSubscribe followerId
  sendNewActiveEndpoints followerId followerEndpoints
