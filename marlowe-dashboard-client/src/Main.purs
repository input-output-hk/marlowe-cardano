module Main
  ( main
  ) where

import Prologue

import AppM (runAppM)
import Bridge (toFront)
import Control.Concurrent.AVarMap as AVarMap
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Effect (Logger)
import Control.Logger.Effect.Class as Logger
import Control.Logger.Effect.Console (structuredLogger) as Console
import Control.Logger.Structured (StructuredLog(..))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Now (class MonadTime, makeClock, now)
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
  , fromString
  , printJsonDecodeError
  , (.:)
  )
import Data.Argonaut.Extra (encodeStringifyJson, parseDecodeJson)
import Data.Either (either, hush)
import Data.Lens (preview, (^.))
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  )
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.PABConnectedWallet (_walletDetails, _walletId)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Time.Duration (Milliseconds(..))
import Data.Wallet
  ( SyncStatus(..)
  , WalletDetails
  , _walletInfo
  , _walletNickname
  , mkWalletDetails
  , syncStatusFromNumber
  )
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect (Effect)
import Effect.Aff (Aff, error, forkAff, launchAff_)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Env (Env(..), Sinks, Sources, WalletFunds)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Store.Select (selectEmitter, selectEq)
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Halogen.Subscription.Extra
  ( compactEmitter
  , connectEmitter_
  , reactimate
  , switchMapEmitter
  )
import Halogen.VDom.Driver (runUI)
import LocalStorage (Key(..), getItem, removeItem, setItem)
import MainFrame.State (mkMainFrame)
import Marlowe.Run.Server as MarloweRun
import Marlowe.Run.Wallet.V1 (GetTotalFundsResponse(..))
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
import Marlowe.Semantics (Assets(..))
import Servant.PureScript (printAjaxError)
import Store (_wallet, mkStore)
import Store as Store
import Store.Contracts (getContractNicknames)
import Store.Wallet (_connectedWallet)
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

mkEnv :: Sources -> Sinks -> Aff Env
mkEnv sources sinks = do
  pabAvar <- AVar.new unit
  contractStepCarouselSubscription <- AVar.empty
  endpointAVarMap <- AVarMap.empty
  followerAVarMap <- AVarMap.empty
  redeemAvarMap <- AVarMap.empty
  createBus <- liftEffect EventBus.create
  applyInputBus <- liftEffect EventBus.create
  redeemBus <- liftEffect EventBus.create
  followerBus <- liftEffect EventBus.create
  pure $ Env
    { contractStepCarouselSubscription
    , followerBus
    , endpointAVarMap
    , followerAVarMap
    , createBus
    , applyInputBus
    , redeemBus
    , sinks
    , sources
    , marloweAppTimeoutBlocks: 3
    , pabAvar
    , redeemAvarMap
    }

exitBadArgs :: forall a. JsonDecodeError -> Effect a
exitBadArgs e = throwError
  $ error
  $ "Failed to start: bad startup args.\n\n" <> printJsonDecodeError e

main :: Json -> Effect Unit
main args = do
  MainArgs { pollingInterval } <-
    either exitBadArgs pure $ decodeJson args
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    pabWebsocketIn <- liftEffect HS.create
    -- | Create a new SubscribeIO for the store. A store emitter is needed by
    -- | `mkWalletfundsEmitter`, but we don't have the store emitter returned
    -- | by `runAppM` yet (and we need to make the wallet emitter before we can
    -- | call `runAppM`). This SubscribeIO will act as a forward reference for
    -- | that emitter, which will eventually be connected when we finally obtain
    -- | it.
    storeIO <- liftEffect HS.create
    let
      -- TODO setup a proper production logger
      logger = Console.structuredLogger
    -- logger = case webpackBuildMode of
    -- Production -> mempty
    -- Development -> Console.structuredLogger
    walletFunds <- mkWalletFundsEmitter logger pollingInterval storeIO.emitter
    pabWebsocketOut <- liftEffect HS.create
    void
      $ forkAff
      $ liftEffect
      $ HS.subscribe pabWebsocketOut.emitter
      $ launchAff_ <<< WS.managerWriteOutbound wsManager <<< WS.SendMessage
    let
      sources =
        { pabWebsocket: pabWebsocketIn.emitter
        , walletFunds
        }
      sinks =
        { logger
        , pabWebsocket: pabWebsocketOut.listener
        }

    env <- mkEnv sources sinks
    body <- awaitBody
    store <- liftEffect loadStore
    { component, emitter: storeE } <- runAppM env store mkMainFrame
    -- Connect the store emitter to the previously created SubscribeIO, tying
    -- the knot.
    connectEmitter_ storeE storeIO
    liftEffect $ persistStore storeE

    void $ runUI component unit body

    void $ forkAff $ WS.runWebSocketManager
      (WS.URI "/pab/ws")
      (liftEffect <<< HS.notify pabWebsocketIn.listener)
      wsManager

mkWalletFundsEmitter
  :: forall m
   . MonadTime m
  => Logger StructuredLog
  -> Milliseconds
  -> Emitter Store.Store
  -> m (Emitter WalletFunds)
mkWalletFundsEmitter logger pollingInterval storeE = do
  pollE <- makeClock pollingInterval
  let
    -- | Fires every time the wallet ID changes in the store
    walletIdE :: Emitter (Maybe WalletId)
    walletIdE = selectEmitter
      (selectEq $ preview $ _wallet <<< _connectedWallet <<< _walletId)
      storeE

    -- | Fires every time a poll should occur
    walletPollE :: Emitter (Maybe WalletId)
    walletPollE = walletIdE # switchMapEmitter case _ of
      Nothing -> pure Nothing
      walletId -> walletId <$ pollE

    -- | React to poll events by dispatching a request to the wallet backend.
    -- | The resulting `Emitter` Fires every time we receive a response.
    walletFundsE :: Emitter (Maybe WalletFunds)
    walletFundsE = walletPollE # reactimate case _ of
      -- We need to fire an "empty" assets here, otherwise it will remember the
      -- previous value fired downstream and use that again the next time
      -- a wallet is activated until the next poll resolves.
      Nothing -> pure $ Just { assets: Assets Map.empty, sync: OutOfSync }
      Just walletId -> do
        result <- MarloweRun.getApiWalletV1ByWalletidTotalfunds walletId
        case result of
          Left e -> do
            Logger.error
              ( StructuredLog
                  { msg: "Failed to poll total funds"
                  , payload: Just $ fromString $ printAjaxError e
                  }
              )
              logger
            pure Nothing
          Right (GetTotalFundsResponse { assets, sync }) ->
            pure $ Just
              { assets: toFront assets, sync: syncStatusFromNumber sync }

  -- Discard any `Nothing` results from `walletFundsE`
  pure $ compactEmitter walletFundsE

-------------------------------------------------------------------------------
-- Local Storage integeration
-------------------------------------------------------------------------------

loadStore :: Effect Store.Store
loadStore = do
  currentTime <- now
  addressBook <- loadAddressBook
  contractNicknames <- loadContractNicknames
  {- [UC-WALLET-TESTNET-2][4b] Restore a testnet wallet
  This is another path for "restoring" a wallet. When we initialize the app,
  if we have some wallet details in the local storage, we try to enter the dashboard
  state with it.
  -}
  wallet <- loadWallet
  pure $ mkStore currentTime addressBook contractNicknames wallet

persistStore :: Emitter Store.Store -> Effect Unit
persistStore storeE = do
  let addressBook = _.addressBook
  let contractNicknames = getContractNicknames <<< _.contracts
  let wallet = preview $ _wallet <<< _connectedWallet <<< _walletDetails
  let
    fromStore :: forall a. Eq a => (Store.Store -> a) -> Emitter a
    fromStore f = selectEmitter (selectEq f) storeE
  void $ HS.subscribe (fromStore addressBook) saveAddressBook
  void $ HS.subscribe (fromStore contractNicknames) saveContractNicknames
  void $ HS.subscribe (fromStore wallet) saveWallet

loadAddressBook :: Effect AddressBook
loadAddressBook =
  decodeAddressBook <$> getItem addressBookKey
  where
  decodeAddressBook mAddressBookJson = fromMaybe AddressBook.empty
    $ hush <<< parseDecodeJson =<< mAddressBookJson

saveAddressBook :: AddressBook -> Effect Unit
saveAddressBook addressBook = do
  let addressBookJson = encodeStringifyJson addressBook
  setItem addressBookKey addressBookJson

loadContractNicknames :: Effect LocalContractNicknames
loadContractNicknames =
  decodeContractNames <$> getItem contractNicknamesKey
  where
  decodeContractNames mContractNames = fromMaybe emptyLocalContractNicknames
    $ hush <<< parseDecodeJson =<< mContractNames

saveContractNicknames :: LocalContractNicknames -> Effect Unit
saveContractNicknames contractNicknames = do
  let contractNamesJson = encodeStringifyJson contractNicknames
  setItem contractNicknamesKey contractNamesJson

type PersistedWalletInfo =
  { walletNickname :: WalletNickname
  , walletId :: WalletId
  , pubKeyHash :: PaymentPubKeyHash
  , address :: Address
  }

loadWallet :: Effect (Maybe WalletDetails)
loadWallet = runMaybeT do
  mWalletJson <- liftEffect $ getItem walletKey
  walletJson <- hoistMaybe mWalletJson
  persistedInfo :: PersistedWalletInfo <-
    hoistMaybe $ hush $ parseDecodeJson walletJson
  let { walletNickname, walletId, pubKeyHash, address } = persistedInfo
  let walletInfo = WalletInfo { walletId, pubKeyHash, address }
  pure $ mkWalletDetails walletNickname walletInfo

saveWallet :: Maybe WalletDetails -> Effect Unit
saveWallet Nothing = removeItem walletKey
saveWallet (Just wallet) = do
  let
    walletNickname = wallet ^. _walletNickname
    WalletInfo { walletId, pubKeyHash, address } = wallet ^. _walletInfo

    persistedInfo :: PersistedWalletInfo
    persistedInfo = { walletNickname, walletId, pubKeyHash, address }
    walletJson = encodeStringifyJson persistedInfo
  setItem walletKey walletJson

addressBookKey :: Key
addressBookKey = Key "addressBook"

walletKey :: Key
walletKey = Key "wallet"

contractNicknamesKey :: Key
contractNicknamesKey = Key "contractNicknames"
