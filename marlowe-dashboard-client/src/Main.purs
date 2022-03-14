module Main
  ( main
  ) where

import Prologue

import Affjax as Affjax
import AppM (runAppM)
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Effect.Console (structuredLogger) as Console
import Control.Monad.Error.Class (throwError)
import Control.Monad.Maybe.Extra (hoistMaybe)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Now (makeClock, now)
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Argonaut
  ( class DecodeJson
  , Json
  , JsonDecodeError
  , decodeJson
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
import Data.PABConnectedWallet (_walletDetails)
import Data.PaymentPubKeyHash (PaymentPubKeyHash)
import Data.Time.Duration (Milliseconds(..))
import Data.Wallet
  ( WalletDetails
  , _walletInfo
  , _walletNickname
  , mkWalletDetails
  )
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Effect (Effect)
import Effect.AVar as AVar
import Effect.Aff (error, forkAff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (getTimezoneOffset)
import Env (Env(..), HandleRequest(..), MakeClock(..), Sinks, Sources)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.Store.Select (selectEmitter, selectEq)
import Halogen.Subscription (Emitter)
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import LocalStorage (Key(..), getItem, removeItem, setItem)
import MainFrame.State (mkMainFrame)
import MainFrame.Types (Msg(..))
import MainFrame.Types as MainFrame
import Marlowe.Run.Wallet.V1.Types (WalletInfo(..))
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

mkEnv
  :: Milliseconds
  -> Milliseconds
  -> Sources
  -> Sinks
  -> WebpackBuildMode
  -> Effect Env
mkEnv regularPollInterval syncPollInterval sources sinks webpackBuildMode = do
  contractStepCarouselSubscription <- AVar.empty
  endpointSemaphores <- AVar.new Map.empty
  createListeners <- AVar.new Map.empty
  applyInputListeners <- AVar.new Map.empty
  redeemListeners <- AVar.new Map.empty
  followerBus <- EventBus.create
  timezoneOffset <- getTimezoneOffset
  pure $ Env
    { contractStepCarouselSubscription
    , logger: case webpackBuildMode of
        -- Add backend logging capability
        Production -> mempty
        Development -> Console.structuredLogger
    , followerBus
    , endpointSemaphores
    , createListeners
    , applyInputListeners
    , redeemListeners
    , sinks
    , sources
    , handleRequest: HandleRequest Affjax.request
    , timezoneOffset
    , makeClock: MakeClock makeClock
    , regularPollInterval
    , syncPollInterval
    }

exitBadArgs :: forall a. JsonDecodeError -> Effect a
exitBadArgs e = throwError
  $ error
  $ "Failed to start: bad startup args.\n\n" <> printJsonDecodeError e

main :: Json -> Effect Unit
main args = do
  MainArgs { pollingInterval, webpackBuildMode } <- either exitBadArgs pure $
    decodeJson args
  runHalogenAff do
    wsManager <- WS.mkWebSocketManager
    pabWebsocketIn <- liftEffect HS.create
    void $ forkAff $ WS.runWebSocketManager
      (WS.URI "/pab/ws")
      (liftEffect <<< HS.notify pabWebsocketIn.listener)
      wsManager
    pabWebsocketOut <- liftEffect HS.create
    void
      $ forkAff
      $ liftEffect
      $ HS.subscribe pabWebsocketOut.emitter
      $ launchAff_ <<< WS.managerWriteOutbound wsManager <<< WS.SendMessage
    let
      sources =
        { pabWebsocket: pabWebsocketIn.emitter
        , currentTime: now
        }
      sinks = { pabWebsocket: pabWebsocketOut.listener }
    env <- liftEffect $ mkEnv
      pollingInterval
      (Milliseconds 500.0)
      sources
      sinks
      webpackBuildMode
    body <- awaitBody

    store <- liftEffect loadStore
    { component, emitter: storeE } <- runAppM env store mkMainFrame
    liftEffect $ persistStore storeE

    driver <- runUI component unit body

    -- This handler allows us to call an action in the MainFrame from a child component
    -- (more info in the MainFrameLoop capability)
    void $ liftEffect $ HS.subscribe driver.messages case _ of
      MainFrameActionMsg action -> launchAff_ $ void $ driver.query $
        MainFrame.MainFrameActionQuery action unit

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
