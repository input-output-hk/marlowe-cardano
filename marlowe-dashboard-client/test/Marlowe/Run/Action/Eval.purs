module Test.Marlowe.Run.Action.Eval where

import Prologue

import Capability.PlutusApps.MarloweApp.Types (EndpointName)
import Concurrent.Queue as Queue
import Control.Monad.Error.Class (class MonadError, throwError, try)
import Control.Monad.Except (ExceptT(..), mapExceptT, runExceptT)
import Control.Monad.Now (class MonadTime, now)
import Control.Monad.Reader (class MonadAsk, asks)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Writer (class MonadTell, runWriterT, tell)
import Control.Parallel (parOneOf)
import Data.Align (crosswalk)
import Data.Argonaut
  ( class EncodeJson
  , Json
  , decodeJson
  , encodeJson
  , jsonEmptyArray
  , jsonNull
  , printJsonDecodeError
  , stringifyWithIndent
  )
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Array (snoc)
import Data.Bifunctor (lmap)
import Data.BigInt.Argonaut (BigInt)
import Data.BigInt.Argonaut as BigInt
import Data.Bimap as Bimap
import Data.DateTime (adjust)
import Data.DateTime.Instant (Instant, fromDateTime, toDateTime, unInstant)
import Data.Either (either)
import Data.Foldable (foldM, for_, traverse_)
import Data.HTTP.Method (Method(..))
import Data.Int (floor)
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Newtype (over2, unwrap)
import Data.String
  ( Pattern(..)
  , Replacement(..)
  , joinWith
  , null
  , replaceAll
  , split
  )
import Data.String.Extra (repeat)
import Data.String.Regex.Flags (ignoreCase)
import Data.Time.Duration (Milliseconds(..))
import Data.Tuple (uncurry)
import Data.Tuple.Nested ((/\))
import Data.UUID.Argonaut (UUID)
import Data.Undefinable (toUndefinable)
import Effect.Aff (Error, delay, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref as Ref
import Halogen.Subscription (notify)
import Marlowe.Semantics (Assets(..))
import MarloweContract (MarloweContract(..))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Assertions (shouldEqualJson)
import Test.Control.Monad.Time (class MonadMockTime, advanceTime)
import Test.Control.Monad.UUID (class MonadMockUUID, mkTestUUID)
import Test.Marlowe.Run (Coenv, fundWallet, marloweRunTest)
import Test.Marlowe.Run.Action.Types
  ( Address
  , AppInstance
  , ContractRoles
  , CreateContractRecord
  , CreateWalletRecord
  , CurrencySymbol
  , MarloweRunAction(..)
  , MarloweRunScript
  , PlutusAppId
  , ScriptError(..)
  , ScriptHash
  , WalletId
  , WalletName
  , renderScriptError
  )
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , expectJsonContent
  , expectJsonRequest
  , expectMethod
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
  expectWalletDeactivation
  where
  expectWalletDeactivation = do
    tell [ "Expect GET contract instances" ]
    expectJsonRequest ado
      expectMethod GET
      expectUri $ "/pab/api/contract/instances/wallet/" <> walletId <> "?"
      in jsonEmptyArray

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
  => { walletName :: WalletName, address :: Address }
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
    type_ nicknameField walletName Nothing
    walletField <- getBy role do
      nameRegex "address" ignoreCase
      pure Textbox
    click walletField
    type_ walletField address Nothing

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
  createContractUUID <- mkTestUUID "aef7e9ad-c283-4039-9a80-28faa0c04c33"
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
  contractCreatedTime <- now
  expectCreateContractHttpRequest contractCreatedTime createContractUUID
  expectSuccessToast
    "The request to initialize this contract has been submitted."
  expectWalletCompanionUpdate contractCreatedTime
  expectContractCreatedWS createContractUUID
  expectFollowerContract
  expectSuccessToast "Contract initialized."

  where
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
      type_ fieldElement r.walletName Nothing

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

  expectCreateContractHttpRequest contractCreatedTime createContractUUID = do
    tell [ "Expect POST create contract" ]

    expectJsonRequest ado
      expectMethod POST
      expectUri $ "/pab/api/contract/instance/" <> marloweAppId <>
        "/endpoint/create"
      expectJsonContent $
        [ encodeJson createContractUUID
        , mockRolesPayload roles
        -- WARNING: This is currently hardcoded to the loan contract.
        , mockLoanContract
            { amount: BigInt.fromInt 10000000
            , interest: BigInt.fromInt 1000000
            }
            contractCreatedTime
        ]
      in
        [] :: Array Json

  expectContractCreatedWS createContractUUID =
    expectMarloweAppEndpointSuccess marloweAppId createContractUUID
      $ encodeJson
          { "contents": marloweParams currencySymbol rolePayoutValidatorHash
          , "tag": "CreateResponse"
          }

  expectWalletCompanionUpdate contractCreatedTime = do
    tell [ "Receive websocket message: Wallet companion update" ]
    pabWebsocketReceive
      $ observableState walletCompanionId
      $ encodeJson
          [ [ marloweParams currencySymbol rolePayoutValidatorHash
            , encodeJson
                { marloweState:
                    { "choices": [] :: Array String
                    , "boundValues": [] :: Array String
                    , "accounts":
                        [ [ encodeJson
                              [ encodeJson
                                  { "pk_hash":
                                      "e08cfb83f317447d18fad74ce06eab5a91d44480d0f7459abc187136"
                                  }
                              , encodeJson
                                  { "currency_symbol": ""
                                  , "token_name": ""
                                  }
                              ]
                          , encodeJson 2000000
                          ]
                        ]
                    , "minTime": adjustInstant 0.0 contractCreatedTime
                    }
                -- WARNING: This is currently hardcoded to the loan contract.
                , marloweContract: mockLoanContract2
                    { amount: 10000000
                    , interest: 1000000
                    }
                    contractCreatedTime
                }
            ]
          ]

  expectFollowerContract = do
    expectPlutusContractActivation walletId MarloweFollower followerId
    expectPlutusContractSubscribe followerId

expectMarloweAppEndpointSuccess
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadTell (Array String) m
  => PlutusAppId
  -> UUID
  -> Json
  -> m Unit
expectMarloweAppEndpointSuccess controlAppId reqId content = do
  tell [ "Receive websocket message: Create contract success" ]
  pabWebsocketReceive
    $ observableState controlAppId
    $ encodeJson
        { "contents":
            [ encodeJson reqId
            , content
            ]
        , "tag": "EndpointSuccess"
        }

marloweParams :: CurrencySymbol -> ScriptHash -> Json
marloweParams currencySymbol rolePayoutValidatorHash =
  encodeJson
    { "rolesCurrency": { "unCurrencySymbol": currencySymbol }
    , "rolePayoutValidatorHash": rolePayoutValidatorHash
    }

observableState :: PlutusAppId -> Json -> Json
observableState plutusAppId state =
  encodeJson
    { "contents":
        [ encodeJson
            { "unContractInstanceId": plutusAppId
            }
        , encodeJson
            { "contents": state
            , "tag": "NewObservableState"
            }
        ]
    , "tag": "InstanceUpdate"
    }

mockRolesPayload :: Array ContractRoles -> Json
mockRolesPayload roles = encodeJson encodedRoles
  where
  encodedRoles = roles <#> \{ roleName, address } -> { unTokenName: roleName }
    /\ address

adjustInstant :: Number -> Instant -> BigInt
adjustInstant offset initial =
  BigInt.fromInt
    $ floor
    $ unwrap
    $ unInstant
    $ maybe initial fromDateTime
    $ adjust (Milliseconds offset) (toDateTime initial)

mockLoanContract :: { interest :: BigInt, amount :: BigInt } -> Instant -> Json
mockLoanContract { interest, amount } currentTime = encodeJson
  { "when":
      [ { "then":
            { "token": { "token_name": "", "currency_symbol": "" }
            , "to": { "party": { "role_token": "Borrower" } }
            , "then":
                { "when":
                    [ { "then":
                          { "token": { "token_name": "", "currency_symbol": "" }
                          , "to": { "party": { "role_token": "Lender" } }
                          , "then": "close"
                          , "pay":
                              { "and": amount
                              , "add": interest
                              }
                          , "from_account": { "role_token": "Borrower" }
                          }
                      , "case":
                          { "party": { "role_token": "Borrower" }
                          , "of_token":
                              { "token_name": "", "currency_symbol": "" }
                          , "into_account": { "role_token": "Borrower" }
                          , "deposits":
                              { "and": amount
                              , "add": interest
                              }
                          }
                      }
                    ]
                , "timeout_continuation": "close"
                , "timeout": adjustInstant 1500000.0 currentTime
                }
            , "pay": amount
            , "from_account": { "role_token": "Lender" }
            }
        , "case":
            { "party": { "role_token": "Lender" }
            , "of_token": { "token_name": "", "currency_symbol": "" }
            , "into_account": { "role_token": "Lender" }
            , "deposits": amount
            }
        }
      ]
  , "timeout_continuation": "close"
  , "timeout": adjustInstant 600000.0 currentTime
  }

mockLoanContract2 :: { interest :: Int, amount :: Int } -> Instant -> Json
mockLoanContract2 { interest, amount } currentTime = encodeJson
  { "when":
      [ { "then":
            { "token": { "token_name": "", "currency_symbol": "" }
            , "to": { "party": { "role_token": "Borrower" } }
            , "then":
                { "when":
                    [ { "then":
                          { "token": { "token_name": "", "currency_symbol": "" }
                          , "to": { "party": { "role_token": "Lender" } }
                          , "then": "close"
                          , "pay":
                              { "and": amount
                              , "add": interest
                              }
                          , "from_account": { "role_token": "Borrower" }
                          }
                      , "case":
                          { "party": { "role_token": "Borrower" }
                          , "of_token":
                              { "token_name": "", "currency_symbol": "" }
                          , "into_account": { "role_token": "Borrower" }
                          , "deposits":
                              { "and": amount
                              , "add": interest
                              }
                          }
                      }
                    ]
                , "timeout_continuation": "close"
                , "timeout": adjustInstant 1500000.0 currentTime
                }
            , "pay": amount
            , "from_account": { "role_token": "Lender" }
            }
        , "case":
            { "party": { "role_token": "Lender" }
            , "of_token": { "token_name": "", "currency_symbol": "" }
            , "into_account": { "role_token": "Lender" }
            , "deposits": amount
            }
        }
      ]
  , "timeout_continuation": "close"
  , "timeout": adjustInstant 600000.0 currentTime
  }

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

-- Assert that there is a error toast with the provided message.
-- This should be executed from the main container
expectErrorToast
  :: forall m
   . MonadTest m
  => MonadError Error m
  => String
  -> m Unit
expectErrorToast message =
  void $ getBy role do
    nameRegex message ignoreCase
    pure Alert

expectPlutusContractSubscribe
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => PlutusAppId
  -> m Unit
expectPlutusContractSubscribe plutusAppId = do
  expectPabWebsocketSend
    { tag: "Subscribe"
    , contents: { "Left": { unContractInstanceId: plutusAppId } }
    }

expectPlutusContractActivation
  :: forall m
   . MonadMockHTTP m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => MonadAsk Coenv m
  => WalletId
  -> MarloweContract
  -> PlutusAppId
  -> m Unit
expectPlutusContractActivation walletId contractType plutusAppId = do
  tell [ "Expect POST activate " <> show contractType ]
  expectJsonRequest ado
    expectMethod POST
    expectUri "/pab/api/contract/activate"
    expectJsonContent
      { caWallet: { prettyWalletName: jsonNull, getWalletId: walletId }
      , caID: encodeJson contractType
      }
    in { unContractInstanceId: plutusAppId }

marloweAppEndpoints :: Array EndpointName
marloweAppEndpoints =
  [ "close"
  , "redeem"
  , "auto"
  , "apply-inputs-nonmerkleized"
  , "apply-inputs"
  , "create"
  ]

expectNewActiveEndpoints
  :: forall m
   . MonadAsk Coenv m
  => MonadError Error m
  => MonadTell (Array String) m
  => MonadAff m
  => PlutusAppId
  -> Array EndpointName
  -> m Unit
expectNewActiveEndpoints plutusAppId endpoints = do
  tell [ "Receive websocket message: New endpoints for " <> plutusAppId ]
  pabWebsocketReceive
    { "contents":
        [ encodeJson
            { "unContractInstanceId": plutusAppId }
        , encodeJson
            { "contents":
                endpoints <#> \endpointName ->
                  { "aeMetadata": jsonNull
                  , "aeDescription":
                      { "getEndpointDescription": endpointName
                      }
                  }
            , "tag": "NewActiveEndpoints"
            }
        ]
    , "tag": "InstanceUpdate"
    }

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
    expectCreateHttpRequest
    fillConfirmMnemonicDialog
    expectWalletActivation

  where
  fillCreateWalletDialog = do
    tell [ "Fill create wallet dialog" ]
    nicknameField <- getBy role do
      nameRegex "wallet nickname" ignoreCase
      pure Textbox
    click nicknameField
    type_ nicknameField walletName Nothing
    clickM $ getBy role do
      nameRegex "create wallet" ignoreCase
      pure Button

  expectCreateHttpRequest = do
    tell [ "Expect POST create wallet" ]
    expectJsonRequest ado
      expectMethod POST
      expectUri "/api/wallet/v1/centralized-testnet/create"
      expectJsonContent $
        { getCreateWalletName: walletName
        , getCreatePassphrase: "fixme-allow-pass-per-wallet"
        }
      in
        { mnemonic: split (Pattern " ") mnemonic
        , walletInfo:
            { walletId
            , pubKeyHash: { unPaymentPubKeyHash: { getPubKeyHash: pubKeyHash } }
            , address
            }
        }

  fillConfirmMnemonicDialog = do
    tell [ "Fill confirm mnemonic dialot" ]
    mnemonicText <- findBy role $ pure Mark
    mnemonicText `shouldHaveText` mnemonic
    clickM $ getBy role do
      nameRegex "ok" ignoreCase
      pure Button
    mnemonicField <- getBy role do
      nameRegex "mnemonic phrase" ignoreCase
      pure Textbox
    type_ mnemonicField mnemonic Nothing
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
    tell [ "Expect GET contract instances" ]
    expectJsonRequest ado
      expectMethod GET
      expectUri $ "/pab/api/contract/instances/wallet/" <> walletId <> "?"
      in ([] :: Array Json)
    expectPlutusContractActivation walletId WalletCompanion walletCompanionId
    expectPlutusContractActivation walletId MarloweApp marloweAppId
    expectPlutusContractSubscribe walletCompanionId
    expectPlutusContractSubscribe marloweAppId
    expectNewActiveEndpoints walletCompanionId []
    expectNewActiveEndpoints marloweAppId marloweAppEndpoints

restore
  :: forall m
   . MonadTest m
  => MonadUser m
  => MonadTell (Array String) m
  => MonadMockHTTP m
  => MonadError Error m
  => MonadAsk Coenv m
  => { walletName :: WalletName, instances :: Array AppInstance }
  -> m Unit
restore { instances, walletName } = do
  wallets <- asks _.wallets
  mWallet <- liftEffect $ Bimap.lookupL walletName <$> Ref.read wallets
  wallet <- maybe
    (throwError $ error $ "Test error: unknown wallet " <> walletName)
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
    type_ nicknameField walletName Nothing
    type_ mnemonicField mnemonic Nothing
    clickM $ getBy role do
      nameRegex "restore wallet" ignoreCase
      pure Button

  expectRestoreHttpRequest { address, mnemonic, pubKeyHash, walletId } = do
    tell [ "Expect POST restore wallet" ]
    expectJsonRequest ado
      expectMethod POST
      expectUri "/api/wallet/v1/centralized-testnet/restore"
      expectJsonContent $
        { getRestoreWalletName: walletName
        , getRestorePassphrase: "fixme-allow-pass-per-wallet"
        , getRestoreMnemonicPhrase: split (Pattern " ") mnemonic
        }
      in
        { walletId
        , pubKeyHash: { unPaymentPubKeyHash: { getPubKeyHash: pubKeyHash } }
        , address
        }

  expectWalletActivation { walletId } = do
    tell [ "Expect GET contract instances" ]
    expectJsonRequest ado
      expectMethod GET
      expectUri $ "/pab/api/contract/instances/wallet/" <> walletId <> "?"
      in
        instances <#> \{ type: contractType, instanceId } ->
          { cicWallet:
              { prettyWalletName: jsonNull
              , getWalletId: walletId
              }
          , cicCurrentState:
              { lastLogs: jsonEmptyArray
              , err: jsonNull
              , hooks: jsonEmptyArray
              , logs: jsonEmptyArray
              , observableState: jsonEmptyArray
              }
          , cicContract:
              { unContractInstanceId: instanceId
              }
          , cicStatus: "Active"
          , cicYieldedExportTxs: jsonEmptyArray
          , cicDefinition: contractType
          }
    traverse_ (expectPlutusContractSubscribe <<< _.instanceId) instances

pabWebsocketReceive
  :: forall m a
   . MonadAsk Coenv m
  => MonadTell (Array String) m
  => MonadEffect m
  => EncodeJson a
  => a
  -> m Unit
pabWebsocketReceive payload = do
  listener <- asks _.pabWebsocketIn
  liftEffect $ notify listener
    (ReceiveMessage $ decodeJson $ encodeJson payload)

expectPabWebsocketSend
  :: forall m a
   . MonadAsk Coenv m
  => MonadError Error m
  => MonadTell (Array String) m
  => EncodeJson a
  => MonadAff m
  => a
  -> m Unit
expectPabWebsocketSend expectPayload = do
  tell
    [ "Expect send websocket message: " <> encodeStringifyJson expectPayload ]
  queue <- asks _.pabWebsocketOut
  msg <- liftAff $ parOneOf
    [ Just <$> Queue.read queue
    , Nothing <$ delay (Milliseconds 1000.0)
    ]
  case msg of
    Nothing -> throwError $ error $ joinWith "\n"
      [ "A websocket message was expected to be sent:"
      , encodeStringifyJson expectPayload
      ]
    Just msg' -> encodeJson msg' `shouldEqualJson` encodeJson expectPayload
