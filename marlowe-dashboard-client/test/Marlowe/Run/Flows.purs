-- Flows are reusable sequences of commands that are intended to setup tests
-- for assertion easily.
module Test.Marlowe.Run.Action.Flows where

import Prologue

import Control.Logger.Capability (class MonadLogger, info)
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
import Data.Lens (takeBoth, traversed, (^.), (^..))
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
  , Contract
  , MarloweData
  , MarloweParams
  , Party(..)
  , _rolesCurrency
  )
import Marlowe.Semantics as Semantics
import MarloweContract (MarloweContract(..))
import Test.Control.Monad.UUID (class MonadMockUUID, getNextUUID)
import Test.Data.Marlowe
  ( adaToken
  , companionEndpoints
  , contractHistory
  , followerEndpoints
  , fromNow
  , loan
  , loanRoles
  , marloweAppEndpoints
  , marloweData
  , semanticState
  , walletCompantionState
  , walletInfo
  )
import Test.Data.Plutus (appInstanceActive)
import Test.Marlowe.Run
  ( Coenv
  , TestWallet
  , fundWallet
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
  , handlePostFollow
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
  info $ "Create new wallet " <> WN.toString walletName
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
      , nickname: walletName
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
  info $ "Restore wallet " <> WN.toString walletName
  { address, mnemonic, pubKeyHash, walletId } <- getWallet walletName
  openRestoreDialog do
    typeWalletNickname $ WN.toString walletName
    typeMnemonicPhrase $ MP.toString mnemonic
    clickRestoreWallet
    handlePostRestoreWallet walletName mnemonic
      $ walletInfo walletId address pubKeyHash

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
  info "Drop wallet"
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
  info $ "Add contact " <> WN.toString walletName
  openContactsDialog do
    clickNewContact
    typeWalletNickname $ WN.toString walletName
    typeAddress $ Address.toString address
    clickSave

createLoan
  :: forall m r
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
  -> { marloweAppId :: UUID, walletCompanionId :: UUID | r }
  -> MarloweParams
  -> ContractNickname
  -> WalletNickname
  -> WalletNickname
  -> Int
  -> Int
  -> m { followerId :: UUID, marloweData :: MarloweData }
createLoan
  wallet
  { marloweAppId, walletCompanionId }
  params
  contractTitle
  borrower
  lender
  amount
  interest = do
  { contract, contractState, reqId } <- createLoanWithoutUpdates
    wallet
    { marloweAppId }
    params
    contractTitle
    borrower
    lender
    amount
    interest
  sendCreateSuccess marloweAppId reqId params
  sendNewActiveEndpoints marloweAppId marloweAppEndpoints
  sendWalletCompanionUpdate walletCompanionId
    [ Tuple params $ marloweData contract contractState
    ]
  followerId <- generateUUID
  handlePostActivate wallet.walletId MarloweFollower followerId
  recvInstanceSubscribe followerId
  sendNewActiveEndpoints followerId followerEndpoints
  handlePostFollow followerId params
  sendFollowerUpdate followerId
    $ contractHistory params (marloweData contract contractState) []
  pure { followerId, marloweData: marloweData contract contractState }

createLoanWithoutUpdates
  :: forall m r
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
  -> { marloweAppId :: UUID | r }
  -> MarloweParams
  -> ContractNickname
  -> WalletNickname
  -> WalletNickname
  -> Int
  -> Int
  -> m { contract :: Contract, contractState :: Semantics.State, reqId :: UUID }
createLoanWithoutUpdates
  wallet
  { marloweAppId }
  params
  contractTitle
  borrower
  lender
  amount
  interest = do
  info $ "Create loan contract"
  createdAt <- now
  reqId <- getNextUUID
  loanDeadline <- fromNow (Minutes 10.0)
  repaymentDeadline <- fromNow (Minutes 25.0)
  let contract = loan loanDeadline repaymentDeadline amount interest
  borrowerWallet <- getWallet borrower
  lenderWallet <- getWallet lender
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
  handlePostCreate marloweAppId reqId
    (loanRoles borrowerWallet.address lenderWallet.address)
    contract
  fundWallet lender (params ^. _rolesCurrency) "Lender" one
    $ wallet.nickname == lender
  fundWallet borrower (params ^. _rolesCurrency) "Borrower" one
    $ wallet.nickname == borrower
  let
    contractState = semanticState
      [ (PK $ PK.toString wallet.pubKeyHash) /\ adaToken /\ 200000 ]
      []
      []
      createdAt
  pure { contract, contractState, reqId }
