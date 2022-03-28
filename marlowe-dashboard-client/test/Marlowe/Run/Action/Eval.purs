module Test.Marlowe.Run.Action.Eval (runScriptedTest) where

import Prologue

import Concurrent.Queue as Queue
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , throwError
  , try
  )
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Control.Parallel (parOneOf)
import Data.Address (Address)
import Data.Address as Address
import Data.Align (crosswalk)
import Data.Argonaut
  ( class EncodeJson
  , encodeJson
  , jsonEmptyArray
  , printJsonDecodeError
  , stringifyWithIndent
  )
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Array (snoc)
import Data.Bifunctor (lmap)
import Data.Bimap as Bimap
import Data.DateTime.Instant (unInstant)
import Data.Either (either)
import Data.Foldable (class Foldable, find, foldM, for_, traverse_)
import Data.HTTP.Method (Method(..))
import Data.Lens (_2, takeBoth, traversed, (^..), (^?))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.MnemonicPhrase as MP
import Data.Newtype (over2)
import Data.PubKeyHash (PubKeyHash)
import Data.String (Pattern(..), Replacement(..), joinWith, null, replaceAll)
import Data.String.Extra (repeat)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..), Minutes(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut (UUID)
import Data.UUID.Argonaut as UUID
import Data.Undefinable (toUndefinable)
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect.Aff (Error, delay, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription (notify)
import Marlowe.Client (_chInitialData, _chParams)
import Marlowe.PAB (PlutusAppId(..))
import Marlowe.Semantics
  ( Assets(..)
  , Contract
  , MarloweData
  , MarloweParams
  , Party(..)
  )
import MarloweContract (MarloweContract(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , ContractInstanceClientState
  )
import Test.Assertions (shouldEqualJson)
import Test.Control.Monad.Time (class MonadMockTime, advanceTime)
import Test.Control.Monad.UUID (class MonadMockUUID, mkTestUUID)
import Test.Data.Marlowe
  ( adaToken
  , companionEndpoints
  , createContent
  , createEndpoint
  , createSuccessMessage
  , createWalletRequest
  , createWalletResponse
  , expectJust
  , followerEndpoints
  , fromNow
  , loan
  , loanRoles
  , marloweAppEndpoints
  , marloweData
  , marloweParams
  , restoreRequest
  , restoreResponse
  , semanticState
  , walletCompantionMessage
  )
import Test.Data.Plutus
  ( MarloweContractInstanceClientState
  , appInstanceActive
  , contractActivationArgs
  , instanceUpdate
  , newActiveEndpoints
  , subscribeApp
  )
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
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , RequestMatcher
  , expectJsonContent
  , expectJsonRequest
  , expectMethod
  , expectNoRequest
  , expectUri
  , renderMatcherError
  )
import Test.Spec (Spec)
import Test.Web.DOM.Assertions (shouldCast, shouldHaveText, shouldNotBeDisabled)
import Test.Web.DOM.Query (findBy, getBy, nameRegex, role)
import Test.Web.Event.User (click, clickM, type_)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest, withContainer)
import Web.ARIA (ARIARole(..))
import WebSocket.Support (FromSocket(..))

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
  openMyWallet do
    clickM $ getBy role do
      nameRegex "drop" ignoreCase
      pure Button
  handleGetContractInstances walletId []

openMyWallet
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => m Unit
  -> m Unit
openMyWallet action = do
  clickM $ getBy role do
    nameRegex "my wallet" ignoreCase
    pure Link
  card <- findBy role $ pure Dialog
  withContainer card action

addContact
  :: forall m
   . MonadError Error m
  => MonadTest m
  => MonadUser m
  => MonadTell (Array String) m
  => { walletName :: WalletNickname, address :: Address }
  -> m Unit
addContact { walletName, address } = do
  card <- openContactList
  withContainer card do
    tell [ "Open add new contact dialog" ]
    clickM $ getBy role do
      nameRegex "new contact" ignoreCase
      pure Button
    fillContactDetails

    tell [ "Submit the form" ]
    clickM $ getBy role do
      nameRegex "save" ignoreCase
      pure Button
  expectSuccessToast "contact added"
  where
  openContactList = do
    tell [ "Open contact list" ]
    clickM $ getBy role do
      nameRegex "contacts" ignoreCase
      pure Link
    findBy role $ pure Dialog
  fillContactDetails = do
    tell [ "Fill contact details" ]
    nicknameField <- getBy role do
      nameRegex "wallet nickname" ignoreCase
      pure Textbox
    click nicknameField
    type_ nicknameField (WN.toString walletName) Nothing
    walletField <- getBy role do
      nameRegex "address" ignoreCase
      pure Textbox
    click walletField
    type_ walletField (Address.toString address) Nothing

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
  card <- openContractTemplates
  createRequestId <- mkTestUUID "aef7e9ad-c283-4039-9a80-28faa0c04c33"
  withContainer card do
    selectTemplate
    fillContractFields
    tell [ "Review terms" ]
    clickM $ getBy role do
      nameRegex "review" ignoreCase
      pure Button

    tell [ "Pay and start" ]
    clickM $ getBy role do
      nameRegex "pay and start" ignoreCase
      pure Button
  createdAt <- now
  loanDeadline <- fromNow (Minutes 10.0)
  repaymentDeadline <- fromNow (Minutes 25.0)
  let contract = loan loanDeadline repaymentDeadline 1000000 10000000
  expectCreateContractHttpRequest createRequestId contract
  expectSuccessToast
    "The request to initialize this contract has been submitted."
  expectWalletCompanionUpdate createdAt contract
  sendCreateSuccess marloweAppId createRequestId params
  expectFollowerContract
  expectSuccessToast "Contract initialized."

  where
  params = marloweParams currencySymbol rolePayoutValidatorHash
  openContractTemplates = do
    tell [ "Open contract templates" ]
    clickM $ getBy role do
      nameRegex "create a new contract" ignoreCase
      pure Link
    findBy role $ pure Dialog

  selectTemplate = do
    tell [ "Select template: " <> templateName ]
    clickM $ getBy role do
      nameRegex templateName ignoreCase
      pure Link
    clickM $ getBy role do
      nameRegex "setup" ignoreCase
      pure Button

  fillContractFields = do
    tell [ "Fill contract title" ]
    titleField <-
      getBy role do
        nameRegex "contract title" ignoreCase
        pure Textbox
    click titleField
    type_ titleField contractTitle Nothing

    -- Fill roles
    for_ roles \r -> do
      tell [ "Fill " <> r.roleName <> " role" ]
      fieldElement <- getBy role do
        nameRegex r.roleName ignoreCase
        pure Textbox
      click fieldElement
      type_ fieldElement (WN.toString r.walletName) Nothing

    -- Fill other fields
    for_ fields \field -> do
      tell [ "Fill " <> field.name <> " field" ]
      fieldElement <- getBy role do
        nameRegex field.name ignoreCase
        pure field.role
      click fieldElement
      type_ fieldElement field.value $ Just
        { skipClick: false
        , skipAutoClose: true
        , initialSelectionStart: toUndefinable $ Just 0
        , initialSelectionEnd: toUndefinable $ Just 10
        }

  expectCreateContractHttpRequest reqId contract = do
    lender <- expectJust "lender role expected"
      $ find (eq "Lender" <<< _.roleName) roles
    borrower <- expectJust "borrower role expected"
      $ find (eq "Borrower" <<< _.roleName) roles
    handlePostCreate marloweAppId reqId
      (loanRoles borrower.address lender.address)
      contract

  expectWalletCompanionUpdate createdAt contract =
    sendWalletCompanionUpdate walletCompanionId
      [ Tuple params
          $ marloweData contract
          $ semanticState
              [ (PK "e08cfb83f317447d18fad74ce06eab5a91d44480d0f7459abc187136")
                  /\ adaToken
                  /\ 200000
              ]
              []
              []
              createdAt
      ]

  expectFollowerContract = do
    handlePostActivate walletId MarloweFollower followerId
    recvInstanceSubscribe followerId
    sendNewActiveEndpoints followerId followerEndpoints

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
  clickM $ getBy role do
    nameRegex "generate" ignoreCase
    pure Button
  dialog <- getBy role $ pure Dialog
  withContainer dialog do
    fillCreateWalletDialog
    handlePostCreateWallet walletName address mnemonic pubKeyHash walletId
    fillConfirmMnemonicDialog
    expectWalletActivation

  where
  fillCreateWalletDialog = do
    tell [ "Fill create wallet dialog" ]
    nicknameField <- getBy role do
      nameRegex "wallet nickname" ignoreCase
      pure Textbox
    click nicknameField
    type_ nicknameField (WN.toString walletName) Nothing
    clickM $ getBy role do
      nameRegex "create wallet" ignoreCase
      pure Button

  fillConfirmMnemonicDialog = do
    tell [ "Fill confirm mnemonic dialot" ]
    mnemonicText <- findBy role $ pure Mark
    mnemonicText `shouldHaveText` MP.toString mnemonic
    clickM $ getBy role do
      nameRegex "ok" ignoreCase
      pure Button
    mnemonicField <- getBy role do
      nameRegex "mnemonic phrase" ignoreCase
      pure Textbox
    type_ mnemonicField (MP.toString mnemonic) Nothing
    okButton <- shouldCast =<< getBy role do
      nameRegex "ok" ignoreCase
      pure Button
    shouldNotBeDisabled okButton
    click okButton
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
  clickM $ getBy role do
    nameRegex "restore" ignoreCase
    pure Button
  dialog <- getBy role $ pure Dialog
  withContainer dialog do
    fillRestoreWalletDialog wallet.mnemonic
    expectRestoreHttpRequest wallet
    expectWalletActivation wallet

  where
  fillRestoreWalletDialog mnemonic = do
    tell [ "Fill restore wallet dialog" ]
    nicknameField <- getBy role do
      nameRegex "wallet nickname" ignoreCase
      pure Textbox
    mnemonicField <- getBy role do
      nameRegex "mnemonic phrase" ignoreCase
      pure Textbox
    type_ nicknameField (WN.toString walletName) Nothing
    type_ mnemonicField (MP.toString mnemonic) Nothing
    clickM $ getBy role do
      nameRegex "restore wallet" ignoreCase
      pure Button

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

-------------------------------------------------------------------------------
-- Mock HTTP Server
-------------------------------------------------------------------------------

handlePostCreateWallet
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => MonadThrow Error m
  => WalletNickname
  -> Address
  -> MnemonicPhrase
  -> PubKeyHash
  -> WalletId
  -> m Unit
handlePostCreateWallet walletName address mnemonic pubKeyHash walletId =
  handleHTTPRequest POST "/api/wallet/v1/centralized-testnet/create" ado
    expectJsonContent $ createWalletRequest walletName
    in createWalletResponse mnemonic walletId address pubKeyHash

handlePostRestoreWallet
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => MonadThrow Error m
  => WalletNickname
  -> Address
  -> MnemonicPhrase
  -> PubKeyHash
  -> WalletId
  -> m Unit
handlePostRestoreWallet walletName address mnemonic pubKeyHash walletId =
  handleHTTPRequest POST "/api/wallet/v1/centralized-testnet/restore" ado
    expectJsonContent $ restoreRequest walletName mnemonic
    in restoreResponse walletId address pubKeyHash

handleGetContractInstances
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => WalletId
  -> Array (ContractInstanceClientState MarloweContract)
  -> m Unit
handleGetContractInstances walletId = handleHTTPRequest GET uri <<< pure
  where
  uri = "/pab/api/contract/instances/wallet/" <> WI.toString walletId <> "?"

handlePostCreate
  :: forall m
   . MonadTell (Array String) m
  => MonadMockHTTP m
  => UUID
  -> UUID
  -> Map String Address
  -> Contract
  -> m Unit
handlePostCreate marloweAppId reqId roles contract = do
  handlePostEndpoint marloweAppId createEndpoint ado
    expectJsonContent $ createContent reqId roles contract
    in jsonEmptyArray

handlePostEndpoint
  :: forall a m
   . EncodeJson a
  => MonadMockHTTP m
  => MonadTell (Array String) m
  => UUID
  -> String
  -> RequestMatcher a
  -> m Unit
handlePostEndpoint instanceId endpoint =
  handleHTTPRequest POST $ joinWith "/"
    [ "/pab/api/contract/instance"
    , UUID.toString instanceId
    , "endpoint"
    , endpoint
    ]

handlePostActivate
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => WalletId
  -> MarloweContract
  -> UUID
  -> m Unit
handlePostActivate walletId contractType instanceId =
  handleHTTPRequest POST "/pab/api/contract/activate" ado
    expectJsonContent $ contractActivationArgs walletId contractType
    in PlutusAppId instanceId

handleHTTPRequest
  :: forall a m
   . EncodeJson a
  => MonadMockHTTP m
  => MonadTell (Array String) m
  => Method
  -> String
  -> RequestMatcher a
  -> m Unit
handleHTTPRequest method uri matcher = do
  tell [ joinWith " " [ "⇵", show method, uri ] ]
  expectJsonRequest ado
    expectMethod method
    expectUri uri
    response <- matcher
    in response

-------------------------------------------------------------------------------
-- Mock WebSocket Server
-------------------------------------------------------------------------------

sendCreateSuccess
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadTell (Array String) m
  => UUID
  -> UUID
  -> MarloweParams
  -> m Unit
sendCreateSuccess appId reqId =
  sendWebsocketMessage <<< createSuccessMessage appId reqId

sendWalletCompanionUpdate
  :: forall f m
   . Foldable f
  => Functor f
  => MonadAsk Coenv m
  => MonadTell (Array String) m
  => MonadEffect m
  => UUID
  -> f (Tuple MarloweParams MarloweData)
  -> m Unit
sendWalletCompanionUpdate companionId =
  sendWebsocketMessage <<< walletCompantionMessage companionId

sendNewActiveEndpoints
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => UUID
  -> Array String
  -> m Unit
sendNewActiveEndpoints instanceId =
  sendWebsocketMessage <<< instanceUpdate instanceId <<< newActiveEndpoints

recvInstanceSubscribe
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => UUID
  -> m Unit
recvInstanceSubscribe instanceId = do
  recvWebsocketMessage $ subscribeApp instanceId

sendWebsocketMessage
  :: forall m
   . MonadAsk Coenv m
  => MonadTell (Array String) m
  => MonadEffect m
  => CombinedWSStreamToClient
  -> m Unit
sendWebsocketMessage payload = do
  tell [ "↑ Send websocket message to app: " <> encodeStringifyJson payload ]
  listener <- asks _.pabWebsocketIn
  liftEffect $ notify listener $ ReceiveMessage $ Right payload

recvWebsocketMessage
  :: forall m a
   . MonadAsk Coenv m
  => MonadError Error m
  => MonadTell (Array String) m
  => EncodeJson a
  => MonadAff m
  => a
  -> m Unit
recvWebsocketMessage expected = do
  tell [ "↓ Recv websocket message from app: " <> encodeStringifyJson expected ]
  queue <- asks _.pabWebsocketOut
  msg <- liftAff $ parOneOf
    [ Just <$> Queue.read queue
    , Nothing <$ delay (Milliseconds 1000.0)
    ]
  case msg of
    Nothing -> throwError $ error $ joinWith "\n"
      [ "A websocket message was expected to be sent:"
      , encodeStringifyJson expected
      ]
    Just msg' -> encodeJson msg' `shouldEqualJson` encodeJson expected
