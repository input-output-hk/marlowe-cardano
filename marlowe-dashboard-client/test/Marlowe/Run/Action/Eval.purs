module Test.Marlowe.Run.Action.Eval (runScriptedTest) where

import Prologue

import Concurrent.Queue as Queue
import Control.Monad.Error.Class (class MonadError, throwError, try)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Data.Address (Address)
import Data.Address as Address
import Data.Align (crosswalk)
import Data.Argonaut
  ( encodeJson
  , jsonEmptyArray
  , printJsonDecodeError
  , stringifyWithIndent
  )
import Data.Argonaut.Extra (parseDecodeJson)
import Data.Array (snoc)
import Data.Bifunctor (lmap)
import Data.Bimap as Bimap
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (find, foldM, for_, traverse_)
import Data.Lens (_2, takeBoth, traversed, (^..), (^?))
import Data.Map as Map
import Data.Maybe (maybe)
import Data.MnemonicPhrase as MP
import Data.Newtype (over2)
import Data.String (Pattern(..), Replacement(..), joinWith, null, replaceAll)
import Data.String.Extra (repeat)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff (Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Marlowe.Client (_chInitialData, _chParams)
import Marlowe.Semantics (Assets(..), Party(..))
import MarloweContract (MarloweContract(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Control.Monad.Time (class MonadMockTime, advanceTime)
import Test.Control.Monad.UUID (class MonadMockUUID, getLastUUID)
import Test.Data.Marlowe
  ( adaToken
  , companionEndpoints
  , expectJust
  , followerEndpoints
  , fromNow
  , loan
  , loanRoles
  , marloweAppEndpoints
  , marloweData
  , marloweParams
  , semanticState
  )
import Test.Data.Plutus (MarloweContractInstanceClientState, appInstanceActive)
import Test.Marlowe.Run (Coenv, fundWallet, marloweRunTest)
import Test.Marlowe.Run.Action.Types
  ( AppInstance(..)
  , CreateContractRecord
  , CreateWalletRecord
  , MarloweRunAction(..)
  , MarloweRunScript
  , ScriptError(..)
  , _MarloweFollowerInstance
  , _WalletCompanionInstance
  , renderScriptError
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
  , sendNewActiveEndpoints
  , sendWalletCompanionUpdate
  , typeAddress
  , typeContractTitle
  , typeContractValue
  , typeMnemonicPhrase
  , typeTextboxRegex
  , typeWalletNickname
  )
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , expectNoRequest
  , renderMatcherError
  )
import Test.Spec (Spec)
import Test.Web.DOM.Assertions (shouldHaveText)
import Test.Web.DOM.Query (findBy, nameRegex, role)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import Web.ARIA (ARIARole(..))

runScriptedTest :: String -> Spec Unit
runScriptedTest scriptName = marloweRunTest scriptName do
  scriptText <-
    liftEffect $ FS.readTextFile UTF8 $ "test/scripts/" <> scriptName <> ".json"
  script <- parseDecodeJson scriptText # case _ of
    Left e -> throwError
      $ error
      $ "Failed to parse script file:\n\n" <> printJsonDecodeError e
    Right s -> pure s
  pabWebsocketOut <- asks _.pabWebsocketOut
  httpRequests <- asks _.httpRequests
  runError <- either (Just <<< renderScriptError) (const Nothing)
    <$> evalScript (lmap Milliseconds <$> script)
  -- Ensure there are no unhandled HTTP requests
  httpMsg <- liftAff do
    lines <- flip tailRecM [] \errors -> do
      mRequest <- Queue.tryRead httpRequests
      pure case mRequest of
        Nothing -> Done errors
        Just request ->
          Loop
            $ snoc errors
            $ renderMatcherError request
            $ MatcherError [ "✗ An HTTP request was not expected" ]
    pure $ joinWith ("\n  " <> repeat 80 "=" <> "\n  ") lines
  -- Ensure there are no unhandled WebSocket messages
  wsMsg <- liftAff do
    lines <- flip tailRecM [] \errors -> do
      mRequest <- Queue.tryRead pabWebsocketOut
      pure case mRequest of
        Nothing -> Done errors
        Just msg ->
          Loop $ snoc errors $ stringifyWithIndent 2 $ encodeJson msg
    pure $ joinWith ("\n  " <> repeat 80 "=" <> "\n  ") lines

  let
    httpError =
      if null httpMsg then Nothing
      else Just $ joinWith "\n  "
        [ "Test finished with unhandled HTTP requests:"
        , repeat 80 "="
        , httpMsg
        ]
    wsError =
      if null wsMsg then Nothing
      else Just $ joinWith "\n  "
        [ "Test finished with unhandled WebSocket messages:"
        , repeat 80 "="
        , wsMsg
        ]
    combinedError =
      joinWith "\n\n  " <$> crosswalk identity [ runError, httpError, wsError ]
  traverse_
    (throwError <<< error <<< replaceAll (Pattern "\n") (Replacement "\n  "))
    combinedError

evalScript
  :: forall m
   . MonadAff m
  => MonadTest m
  => MonadUser m
  => MonadError Error m
  => MonadMockHTTP m
  => MonadMockTime m
  => MonadMockUUID m
  => MonadTime m
  => MonadAsk Coenv m
  => MarloweRunScript
  -> m (Either ScriptError Unit)
evalScript = runExceptT <<< void <<< foldM (map uncurry evalAction') []
  where
  evalAction'
    :: Array MarloweRunAction
    -> Milliseconds
    -> MarloweRunAction
    -> ExceptT ScriptError m (Array MarloweRunAction)
  evalAction' succeeded millis action =
    mapExceptT mkScriptError do
      currentTime <- now
      let currentMillis = unInstant currentTime
      let millisUntilAction = over2 Milliseconds (-) millis currentMillis
      advanceTime (millisUntilAction :: Milliseconds)
      ExceptT $ try $ evalAction action
      pure $ snoc succeeded action
    where
    mkScriptError writer = do
      Tuple result steps <- runWriterT writer
      pure $ lmap (ScriptError succeeded action steps) result

evalAction
  :: forall m
   . MonadAff m
  => MonadTest m
  => MonadTell (Array String) m
  => MonadUser m
  => MonadError Error m
  => MonadMockHTTP m
  => MonadMockUUID m
  => MonadTime m
  => MonadAsk Coenv m
  => MarloweRunAction
  -> m Unit
evalAction = case _ of
  DropWallet { walletId } -> dropWallet walletId
  CreateWallet params -> createWallet params
  CreateContract params -> createContract params
  FundWallet { walletName, lovelace } -> fundWallet walletName "" "" lovelace
  AddContact params -> addContact params
  RestoreWallet params -> restore params
  ExpectNoHTTPCall -> expectNoRequest

dropWallet
  :: forall m
   . MonadTest m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadUser m
  => MonadAsk Coenv m
  => MonadMockHTTP m
  => WalletId
  -> m Unit
dropWallet walletId = do
  openMyWalletDialog clickDrop
  handleGetContractInstances walletId []

addContact
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => MonadTell (Array String) m
  => { walletName :: WalletNickname, address :: Address }
  -> m Unit
addContact { walletName, address } = do
  openContactsDialog do
    clickNewContact
    typeWalletNickname $ WN.toString walletName
    typeAddress $ Address.toString address
    clickSave
  expectSuccessToast "contact added"

createContract
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => MonadTime m
  => MonadTell (Array String) m
  => MonadAsk Coenv m
  => MonadMockUUID m
  => MonadMockHTTP m
  => CreateContractRecord
  -> m Unit
createContract
  { templateName
  , contractTitle
  , fields
  , roles
  , marloweAppId
  , followerId
  , walletCompanionId
  , currencySymbol
  , rolePayoutValidatorHash
  , walletId
  } = do
  openNewContractDialog do
    clickLinkRegex templateName
    clickSetup
    typeContractTitle contractTitle
    for_ roles \r -> do typeTextboxRegex r.roleName $ WN.toString r.walletName
    for_ fields \field -> typeContractValue field.role field.name field.value
    clickReview
    clickPayAndStart
  reqId <- getLastUUID
  createdAt <- now
  loanDeadline <- fromNow (Minutes 10.0)
  repaymentDeadline <- fromNow (Minutes 25.0)
  let
    contract = loan loanDeadline repaymentDeadline 1000000 10000000
    params = marloweParams currencySymbol rolePayoutValidatorHash
    contractState = semanticState
      [ (PK "e08cfb83f317447d18fad74ce06eab5a91d44480d0f7459abc187136")
          /\ adaToken
          /\ 200000
      ]
      []
      []
      createdAt
  lender <- expectJust "lender role expected"
    $ find (eq "Lender" <<< _.roleName) roles
  borrower <- expectJust "borrower role expected"
    $ find (eq "Borrower" <<< _.roleName) roles
  handlePostCreate marloweAppId reqId
    (loanRoles borrower.address lender.address)
    contract
  expectSuccessToast
    "The request to initialize this contract has been submitted."
  sendWalletCompanionUpdate walletCompanionId
    [ Tuple params $ marloweData contract contractState
    ]
  sendCreateSuccess marloweAppId reqId params
  handlePostActivate walletId MarloweFollower followerId
  recvInstanceSubscribe followerId
  sendNewActiveEndpoints followerId followerEndpoints
  expectSuccessToast "Contract initialized."

-- Assert that there is a success toast with the provided message.
-- This should be executed from the main container
expectSuccessToast
  :: forall m
   . MonadTest m
  => MonadTell (Array String) m
  => MonadError Error m
  => String
  -> m Unit
expectSuccessToast message = do
  tell [ "Expect success toast: " <> message ]
  void $ findBy role do
    nameRegex message ignoreCase
    pure Status

-- -- Assert that there is a error toast with the provided message.
-- -- This should be executed from the main container
-- expectErrorToast
--   :: forall m
--    . MonadTest m
--   => MonadError Error m
--   => String
--   -> m Unit
-- expectErrorToast message =
--   void $ getBy role do
--     nameRegex message ignoreCase
--     pure Alert

createWallet
  :: forall m
   . MonadTest m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadUser m
  => MonadAsk Coenv m
  => MonadMockHTTP m
  => CreateWalletRecord
  -> m Unit
createWallet
  { walletName
  , mnemonic
  , walletId
  , pubKeyHash
  , address
  , walletCompanionId
  , marloweAppId
  } = do
  openGenerateDialog do
    fillCreateWalletDialog
    handlePostCreateWallet walletName address mnemonic pubKeyHash walletId
    fillConfirmMnemonicDialog
    expectWalletActivation

  where
  fillCreateWalletDialog = do
    tell [ "Fill create wallet dialog" ]
    typeWalletNickname $ WN.toString walletName
    clickCreateWallet

  fillConfirmMnemonicDialog = do
    tell [ "Fill confirm mnemonic dialot" ]
    mnemonicText <- findBy role $ pure Mark
    mnemonicText `shouldHaveText` MP.toString mnemonic
    clickOk
    typeMnemonicPhrase $ MP.toString mnemonic
    clickOk
    wallets <- asks _.wallets
    liftEffect $ Ref.modify_
      ( Bimap.insert walletName
          { address, assets: Assets Map.empty, mnemonic, pubKeyHash, walletId }
      )
      wallets

  expectWalletActivation = do
    handleGetContractInstances walletId []
    handlePostActivate walletId WalletCompanion walletCompanionId
    handlePostActivate walletId MarloweApp marloweAppId
    recvInstanceSubscribe walletCompanionId
    sendNewActiveEndpoints walletCompanionId companionEndpoints
    recvInstanceSubscribe marloweAppId
    sendNewActiveEndpoints marloweAppId marloweAppEndpoints

restore
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadTell (Array String) m
  => MonadMockHTTP m
  => MonadError Error m
  => MonadAsk Coenv m
  => { walletName :: WalletNickname, instances :: Array AppInstance }
  -> m Unit
restore { instances, walletName } = do
  wallets <- asks _.wallets
  mWallet <- liftEffect $ Bimap.lookupL walletName <$> Ref.read wallets
  wallet <- maybe
    ( throwError
        $ error
        $ "Test error: unknown wallet " <> (WN.toString walletName)
    )
    pure
    mWallet
  openRestoreDialog do
    fillRestoreWalletDialog wallet.mnemonic
    expectRestoreHttpRequest wallet
    expectWalletActivation wallet

  where
  fillRestoreWalletDialog mnemonic = do
    tell [ "Fill restore wallet dialog" ]
    typeWalletNickname $ WN.toString walletName
    typeMnemonicPhrase $ MP.toString mnemonic
    clickRestoreWallet

  expectRestoreHttpRequest { address, mnemonic, pubKeyHash, walletId } = do
    handlePostRestoreWallet walletName address mnemonic pubKeyHash walletId

  expectWalletActivation { walletId } = do
    handleGetContractInstances walletId
      $ appInstanceToCic walletId <$> instances
    for_ instances case _ of
      MarloweAppInstance instanceId -> do
        recvInstanceSubscribe instanceId
        sendNewActiveEndpoints instanceId marloweAppEndpoints
      WalletCompanionInstance instanceId -> do
        recvInstanceSubscribe instanceId
        sendNewActiveEndpoints instanceId companionEndpoints
      MarloweFollowerInstance instanceId _ -> do
        recvInstanceSubscribe instanceId
        sendNewActiveEndpoints instanceId followerEndpoints
    let
      mCompanion = instances ^? traversed <<< _WalletCompanionInstance
      companionContracts =
        instances ^.. traversed
          <<< _MarloweFollowerInstance
          <<< _2
          <<< takeBoth _chParams _chInitialData
    traverse_ (flip sendWalletCompanionUpdate companionContracts) mCompanion

appInstanceToCic
  :: WalletId -> AppInstance -> MarloweContractInstanceClientState
appInstanceToCic walletId = case _ of
  MarloweAppInstance instanceId ->
    appInstanceActive walletId MarloweApp instanceId jsonEmptyArray
  WalletCompanionInstance instanceId ->
    appInstanceActive walletId WalletCompanion instanceId jsonEmptyArray
  MarloweFollowerInstance instanceId history ->
    appInstanceActive walletId MarloweFollower instanceId history
