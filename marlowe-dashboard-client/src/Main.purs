module Main
  ( main
  ) where

import Prologue

import AppM (runAppM)
import Bridge (toFront)
import Capability.MarloweStorage as MarloweStorage
import Control.Logger.Effect.Console (logger) as Console
import Control.Monad.Error.Class (throwError)
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
  , printJsonDecodeError
  , (.:)
  )
import Data.Either (either)
import Data.Lens (_Just, (^?))
import Data.Map as Map
import Data.PABConnectedWallet
  ( PABConnectedWallet
  , _companionAppId
  , _marloweAppId
  )
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (error, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Env (Env(..), WebSocketManager)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Humanize (getTimezoneOffset)
import Language.Marlowe.Client (EndpointResponse(..), MarloweEndpointResult(..))
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..))
import MainFrame.Types as MainFrame
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient(..)
  , InstanceStatusToClient(..)
  )
import Store (mkStore)
import WebSocket.Support (FromSocket)
import WebSocket.Support as WS

newtype MainArgs = MainArgs
  { pollingInterval :: Milliseconds
  , webpackBuildMode :: WebpackBuildMode
  }

data WebpackBuildMode = Production | Development

instance DecodeJson MainArgs where
  decodeJson = decodeJson >=> \obj -> ado
    pollingInterval <- Milliseconds <$> obj .: "pollingInterval"
    webpackBuildMode <- obj .: "webpackDevelMode" <#>
      if _ then Development
      else Production
    in MainArgs { pollingInterval, webpackBuildMode }

mkEnv :: Milliseconds -> WebSocketManager -> WebpackBuildMode -> Effect Env
mkEnv pollingInterval wsManager webpackBuildMode = do
  contractStepCarouselSubscription <- AVar.empty
  endpointSemaphores <- AVar.new Map.empty
  pendingResults <- AVar.new []
  pure $ Env
    { contractStepCarouselSubscription
    , logger: case webpackBuildMode of
        -- Add backend logging capability
        Production -> mempty
        Development -> Console.logger identity
    , endpointSemaphores
    , wsManager
    , pendingResults
    , pollingInterval
    }

exitBadArgs :: forall a. JsonDecodeError -> Effect a
exitBadArgs e = throwError
  $ error
  $ "Failed to start: bad startup args.\n\n" <> printJsonDecodeError e

main :: Json -> Effect Unit
main args = do
  MainArgs { pollingInterval, webpackBuildMode } <- either exitBadArgs pure $
    decodeJson args
  tzOffset <- getTimezoneOffset
  addressBook <- MarloweStorage.getAddressBook
  contractNicknames <- MarloweStorage.getContractNicknames
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    env <- liftEffect $ mkEnv pollingInterval wsManager webpackBuildMode
    let store = mkStore addressBook contractNicknames
    body <- awaitBody
    rootComponent <- runAppM env store mkMainFrame
    driver <- runUI rootComponent { tzOffset } body
    -- This is a hack. The PAB sends us duplicate companion app updates, so we
    -- deduplicate by storing the last update and ignoring subsequent
    -- duplicates.
    lastCompanionAppStateRef <- liftEffect $ Ref.new Nothing
    void
      $ forkAff
      $ WS.runWebSocketManager
          (WS.URI "/pab/ws")
          ( \msg -> void $ forkAff $ do
              mWallet <- driver.query $ H.mkRequest MainFrame.GetWallet
              let mQuery = wsMsgToQuery mWallet msg
              lastState <- liftEffect $ Ref.read lastCompanionAppStateRef
              let
                handleQuery q = case q of
                  MainFrame.CompanionAppStateUpdated newState _
                    | Just newState == lastState -> pure Nothing
                    | otherwise -> do
                        liftEffect
                          $ Ref.write (Just newState) lastCompanionAppStateRef
                        driver.query q
                  _ -> driver.query q
              traverse (handleQuery <<< H.mkTell) mQuery
          )
          wsManager
    -- This handler allows us to call an action in the MainFrame from a child component
    -- (more info in the MainFrameLoop capability)
    void
      $ liftEffect
      $ HS.subscribe driver.messages
      $ \(MainFrameActionMsg action) -> launchAff_ $ void $ driver.query $
          MainFrame.MainFrameActionQuery action unit

wsMsgToQuery
  :: Maybe PABConnectedWallet
  -> FromSocket CombinedWSStreamToClient
  -> Maybe (Unit -> MainFrame.Query Unit)
wsMsgToQuery mWallet = case _ of
  WS.WebSocketOpen ->
    Just $ MainFrame.NewWebSocketStatus MainFrame.WebSocketOpen
  WS.WebSocketClosed closeEvent ->
    Just
      $ MainFrame.NewWebSocketStatus
      $ MainFrame.WebSocketClosed
      $ Just closeEvent
  WS.ReceiveMessage (Left jsonDecodeError) ->
    Just $ MainFrame.NotificationParseFailed "websocket message" jsonDecodeError
  WS.ReceiveMessage (Right stream) -> streamToQuery mWallet stream

streamToQuery
  :: Maybe PABConnectedWallet
  -> CombinedWSStreamToClient
  -> Maybe (Unit -> MainFrame.Query Unit)

streamToQuery mWallet = case _ of
  SlotChange slot -> Just $ MainFrame.SlotChange $ toFront slot
  -- TODO handle with lite wallet support
  InstanceUpdate _ (NewYieldedExportTxs _) -> Nothing
  InstanceUpdate appId (NewActiveEndpoints activeEndpoints) ->
    Just $ MainFrame.NewActiveEndpoints appId activeEndpoints
  InstanceUpdate appId (ContractFinished message)
    | Just appId == mCompanionAppId ->
        Just $ MainFrame.WalletCompanionAppClosed message
    | Just appId == mMarloweAppId ->
        Just $ MainFrame.MarloweAppClosed message
    | otherwise ->
        Just $ MainFrame.MarloweAppClosed message
  InstanceUpdate appId (NewObservableState state)
    | Just appId == mCompanionAppId -> Just $ case decodeJson state of
        Left error ->
          MainFrame.NotificationParseFailed "wallet companion state" error
        Right companionAppState ->
          MainFrame.CompanionAppStateUpdated companionAppState
    | Just appId == mMarloweAppId -> case decodeJson state of
        Left error ->
          Just $ MainFrame.NotificationParseFailed "marlowe app response" error
        Right (EndpointException uuid "create" error) ->
          Just $ MainFrame.CreateFailed uuid error
        Right (EndpointException uuid "apply-inputs-nonmerkleized" error) ->
          Just $ MainFrame.ApplyInputsFailed uuid error
        Right (EndpointException uuid "redeem" error) ->
          Just $ MainFrame.RedeemFailed uuid error
        Right (EndpointSuccess uuid (CreateResponse marloweParams)) ->
          Just $ MainFrame.MarloweContractCreated uuid marloweParams
        Right (EndpointSuccess uuid ApplyInputsResponse) ->
          Just $ MainFrame.InputsApplied uuid
        Right (EndpointSuccess uuid RedeemResponse) ->
          Just $ MainFrame.PaymentRedeemed uuid
        _ -> Nothing
    | otherwise -> case decodeJson state of
        Left error ->
          Just $ MainFrame.NotificationParseFailed "follower app response" error
        Right history ->
          Just $ MainFrame.ContractHistoryUpdated history
  where
  mCompanionAppId = mWallet ^? _Just <<< _companionAppId
  mMarloweAppId = mWallet ^? _Just <<< _marloweAppId
