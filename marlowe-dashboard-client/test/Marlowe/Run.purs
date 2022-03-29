module Test.Marlowe.Run where

import Prologue

import Ansi.Codes (Color(..))
import Ansi.Output (bold, foreground, withGraphics)
import AppM (runAppM)
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Apply (lift2)
import Control.Concurrent.AVarMap as AVarMap
import Control.Concurrent.EventBus as EventBus
import Control.Logger (Logger(..))
import Control.Logger.Capability (class MonadLogger)
import Control.Logger.Effect.Test (testLogger)
import Control.Logger.Effect.Test as Logger
import Control.Logger.Structured (StructuredLog(..))
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , catchError
  , throwError
  , try
  )
import Control.Monad.Freer.Extras.Log (LogLevel(..), LogMessage(..))
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT
  , asks
  , runReaderT
  )
import Control.Monad.Rec.Class (whileJust)
import Control.Monad.UUID (class MonadUUID)
import Control.Parallel (parSequence_)
import Data.Address (Address)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.Align (crosswalk)
import Data.Argonaut (encodeJson, stringify, stringifyWithIndent)
import Data.Array as Array
import Data.BigInt.Argonaut (BigInt)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.Compactable (compact)
import Data.Foldable (fold, traverse_)
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  )
import Data.Map (Map)
import Data.Map as Map
import Data.MnemonicPhrase (MnemonicPhrase)
import Data.Newtype (over)
import Data.PubKeyHash (PubKeyHash)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, take)
import Data.String as String
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UniqueIdentifier (UniqueIdentifier)
import Data.Wallet (SyncStatus(..), WalletDetails)
import Data.WalletId (WalletId)
import Data.WalletNickname (WalletNickname)
import Data.WalletNickname as WN
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket, error, launchAff_, message)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Env (Env(..), WalletFunds)
import Halogen as H
import Halogen.Subscription (Listener, SubscribeIO)
import Halogen.Subscription as HS
import MainFrame.State (mkMainFrame)
import MainFrame.Types as MF
import Marlowe.Semantics (Assets(..), CurrencySymbol, TokenName)
import Marlowe.Time (unixEpoch)
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient
  , CombinedWSStreamToServer
  )
import Store (mkStore)
import Test.Control.Monad.Time
  ( class MonadMockTime
  , MockTimeEnv
  , MockTimeM
  , runMockTimeM
  )
import Test.Control.Monad.UUID (class MonadMockUUID, MockUuidM, runMockUuidM)
import Test.Halogen (class MonadHalogenTest, runUITest)
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MatcherError(..)
  , MockHttpM
  , RequestBox
  , renderMatcherError
  , runMockHttpM
  )
import Test.Spec (Spec, it)
import Test.Web.Event.User.Monad (class MonadUser)
import Test.Web.Monad (class MonadTest)
import WebSocket.Support (FromSocket)

marloweRunTest
  :: String
  -> ( forall m
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
marloweRunTest name =
  marloweRunTestWith name AddressBook.empty emptyLocalContractNicknames Nothing

marloweRunTestWith
  :: String
  -> AddressBook
  -> LocalContractNicknames
  -> Maybe WalletDetails
  -> ( forall m
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
marloweRunTestWith name addressBook contractNicknames wallet test = it name $
  bracket
    (liftAff mkTestEnv)
    (\(_ /\ coenv /\ _) -> liftEffect $ coenv.dispose)
    \(env /\ coenv /\ errors) -> do
      clocks <- liftEffect $ Ref.new Map.empty
      nowRef <- liftEffect $ Ref.new unixEpoch
      nextClockId <- liftEffect $ Ref.new 0
      let mockTimeEnv = { clocks, nowRef, tzOffset: Minutes zero, nextClockId }
      mockUuidEnv <- liftEffect $ Ref.new bottom
      let
        store = mkStore unixEpoch addressBook contractNicknames wallet
      { component } <- runAppM env store mkMainFrame
      errorAVar <- AVar.empty
      sub <- liftEffect
        $ HS.subscribe errors.emitter
        $ launchAff_ <<< flip AVar.kill errorAVar
      result <- try $ parSequence_
        [ do
            runUITest
              ( H.hoist
                  ( marshallErrors errors.listener
                      <<< flip runMockUuidM mockUuidEnv
                      <<< flip runMockTimeM mockTimeEnv
                      <<< flip runMockHttpM coenv.httpRequests
                  )
                  component
              )
              unit
              (runMarloweTestM test coenv mockTimeEnv mockUuidEnv)
            AVar.put unit errorAVar
        , AVar.take errorAVar
        ]
      liftEffect $ HS.unsubscribe sub
      httpMsg <- drainHttpRequests coenv
      wsMsg <- drainWebsocketMessages coenv
      appLog <- drainQueue coenv.logMessages
      testLog <- drainQueue coenv.testLogMessages
      let
        runError = case result of
          Left e -> Just $ renderRunError appLog testLog e
          _ -> Nothing
        compoundMsg = join <$> crosswalk identity [ httpMsg, wsMsg, runError ]
      traverse_ reportFailure compoundMsg
  where
  drainQueue :: forall a. Queue a -> Aff (Array a)
  drainQueue = whileJust <<< map (map Array.singleton) <<< Queue.tryRead
  drainHttpRequests { httpRequests } = do
    requests <- drainQueue httpRequests
    pure $ crosswalk (Just <<< renderHttpRequest) requests
  renderHttpRequest request = renderMatcherError request
    $ MatcherError [ "✗ An HTTP request was not handled" ]
  drainWebsocketMessages { pabWebsocketOut } = do
    messages <- drainQueue pabWebsocketOut
    pure $ crosswalk (Just <<< renderWebsocketMessage) messages
  renderWebsocketMessage message =
    joinWith "\n"
      [ "✗ A Websocket message was not handled"
      , stringifyWithIndent 2 $ encodeJson message
      ]
  renderRunError appLog testLog e =
    join
      [ pure "Application log:"
      , pure ""
      , reportLogMessage reportStructuredLog <$> appLog
      , pure ""
      , pure "Test log:"
      , pure ""
      , reportLogMessage identity <$> testLog
      , pure ""
      , pure $ withGraphics (foreground Red) "Error was:"
      , pure ""
      , pure
          $ withGraphics (foreground Red)
          $ replaceAll (Pattern "\n") (Replacement "\n  ")
          $ "  " <> message e
      ]
    where
    reportStructuredLog (StructuredLog { msg, payload }) =
      joinWith " " $ compact [ Just msg, stringify <$> payload ]

    reportLogMessage :: forall a. (a -> String) -> LogMessage a -> String
    reportLogMessage f (LogMessage { _logLevel, _logMessageContent }) =
      joinWith " " [ reportLevel _logLevel, truncate $ f _logMessageContent ]
    reportLevel level =
      withGraphics (levelGraphics level) $ fold [ "[", show level, "]" ]
    levelGraphics = case _ of
      Debug -> foreground Blue
      Info -> foreground Green
      Notice -> foreground Magenta
      Warning -> foreground Yellow
      Error -> foreground Red
      _ -> bold <> foreground Red
    truncate text
      | String.length text <= 120 = text
      | otherwise = take 191 text <> "…"
  reportFailure msgs = do
    throwError
      $ error
      $ replaceAll (Pattern "\n") (Replacement "\n  ")
      $ joinWith "\n" msgs

newtype MarloweTestM (m :: Type -> Type) a = MarloweTestM
  (ReaderT Coenv (MockHttpM (MockTimeM (MockUuidM m))) a)

derive newtype instance Functor m => Functor (MarloweTestM m)
derive newtype instance Apply m => Apply (MarloweTestM m)
derive newtype instance Applicative m => Applicative (MarloweTestM m)
derive newtype instance Bind m => Bind (MarloweTestM m)
instance Monad m => Monad (MarloweTestM m)
derive newtype instance MonadEffect m => MonadEffect (MarloweTestM m)
derive newtype instance MonadAff m => MonadAff (MarloweTestM m)
derive newtype instance MonadThrow e m => MonadThrow e (MarloweTestM m)
derive newtype instance MonadError e m => MonadError e (MarloweTestM m)
derive newtype instance Monad m => MonadAsk Coenv (MarloweTestM m)
derive newtype instance Monad m => MonadReader Coenv (MarloweTestM m)
derive newtype instance MonadTest m => MonadTest (MarloweTestM m)
derive newtype instance MonadUser m => MonadUser (MarloweTestM m)
derive newtype instance
  ( MonadError Error m
  , MonadAff m
  ) =>
  MonadMockHTTP (MarloweTestM m)

derive newtype instance
  MonadHalogenTest q i o m =>
  MonadHalogenTest q i o (MarloweTestM m)

derive newtype instance
  ( MonadEffect m
  , MonadThrow Error m
  ) =>
  MonadMockTime (MarloweTestM m)

derive newtype instance MonadEffect m => MonadTime (MarloweTestM m)
derive newtype instance
  ( MonadEffect m
  , MonadError Error m
  ) =>
  MonadUUID (MarloweTestM m)

derive newtype instance MonadEffect m => MonadMockUUID (MarloweTestM m)
instance MonadEffect m => MonadLogger String (MarloweTestM m) where
  log msg = do
    Logger logger <- asks $ Logger.testLogger <<< _.testLogMessages
    liftEffect $ logger msg

runMarloweTestM
  :: forall m a
   . MonadAff m
  => MonadError Error m
  => MarloweTestM m a
  -> Coenv
  -> MockTimeEnv
  -> Ref UniqueIdentifier
  -> m a
runMarloweTestM (MarloweTestM m) coenv timeEnv uuidEnv =
  flip runMockUuidM uuidEnv
    $ flip runMockTimeM timeEnv
    $ flip runMockHttpM coenv.httpRequests
    $ runReaderT m coenv

marshallErrors
  :: forall m a
   . MonadError Error m
  => Listener Error
  -> MonadEffect m
  => m a
  -> m a
marshallErrors errors m = catchError m \e -> do
  liftEffect $ HS.notify errors e
  throwError e

mkTestEnv :: Aff (Env /\ Coenv /\ SubscribeIO Error)
mkTestEnv = do
  contractStepCarouselSubscription <- liftAff AVar.empty
  endpointAVarMap <- AVarMap.empty
  followerAVarMap <- AVarMap.empty
  createBus <- liftEffect EventBus.create
  applyInputBus <- liftEffect EventBus.create
  redeemBus <- liftEffect EventBus.create
  followerBus <- liftEffect EventBus.create
  pabWebsocketIn <- liftEffect HS.create
  walletFunds <- liftEffect HS.create
  pabWebsocketOut <- liftEffect HS.create
  errors <- liftEffect HS.create
  pabWebsocketOutQueue <- Queue.new
  httpRequests <- Queue.new
  logMessages <- Queue.new
  sub <- liftEffect $ HS.subscribe pabWebsocketOut.emitter \msg ->
    launchAff_ $ Queue.write pabWebsocketOutQueue msg
  wallets <- liftEffect $ Ref.new Bimap.empty
  testLogMessages <- Queue.new
  let
    sources =
      { pabWebsocket: pabWebsocketIn.emitter
      , walletFunds: walletFunds.emitter
      }
    sinks =
      { logger: testLogger logMessages
      , pabWebsocket: pabWebsocketOut.listener
      }

    env = Env
      { contractStepCarouselSubscription
      , endpointAVarMap
      , followerAVarMap
      , createBus
      , applyInputBus
      , redeemBus
      , followerBus
      , sinks
      , sources
      }
    coenv =
      { pabWebsocketIn: pabWebsocketIn.listener
      , walletFunds: walletFunds.listener
      , pabWebsocketOut: pabWebsocketOutQueue
      , httpRequests
      , dispose: do
          HS.unsubscribe sub
      , wallets
      , logMessages
      , testLogMessages
      }
  pure $ env /\ coenv /\ errors

type TestWallet =
  { address :: Address
  , mnemonic :: MnemonicPhrase
  , pubKeyHash :: PubKeyHash
  , walletId :: WalletId
  , assets :: Assets
  }

-- Like `Env` but we invert the control of everything in it (i.e. all sources
-- become sinks, sinks sources).
type Coenv =
  { pabWebsocketOut :: Queue CombinedWSStreamToServer
  , httpRequests :: Queue RequestBox
  , pabWebsocketIn :: Listener (FromSocket CombinedWSStreamToClient)
  , walletFunds :: Listener WalletFunds
  , dispose :: Effect Unit
  , wallets :: Ref (Bimap WalletNickname TestWallet)
  , testLogMessages :: Queue (LogMessage String)
  , logMessages :: Queue (LogMessage StructuredLog)
  }

sendWalletFunds
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadError Error m
  => WalletNickname
  -> m Unit
sendWalletFunds walletName = do
  wallet <- getWallet walletName
  walletFunds <- asks _.walletFunds
  liftEffect
    $ HS.notify walletFunds { sync: Synchronized, assets: wallet.assets }

fundWallet
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadError Error m
  => WalletNickname
  -> CurrencySymbol
  -> TokenName
  -> BigInt
  -> m Unit
fundWallet walletName currencySymbol tokenName amount = do
  wallet <- getWallet walletName
  walletFunds <- asks _.walletFunds
  let
    alter
      :: forall k v. Ord k => k -> (Maybe v -> Maybe v) -> Map k v -> Map k v
    alter = flip Map.alter
    addAsset = alter currencySymbol $ Just <<< case _ of
      Nothing -> Map.singleton tokenName amount
      Just tokens -> alter tokenName (lift2 add (pure amount)) tokens
    assets = over Assets addAsset wallet.assets
    newWallet = wallet { assets = assets }
  setWallet walletName newWallet
  liftEffect $ HS.notify walletFunds { sync: Synchronized, assets }

getWallet
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadError Error m
  => WalletNickname
  -> m TestWallet
getWallet walletName = do
  walletsRef <- asks _.wallets
  wallets <- liftEffect $ Ref.read walletsRef
  case Bimap.lookupL walletName wallets of
    Nothing -> throwError
      $ error
      $ "Test error: unknown wallet " <> WN.toString walletName
    Just w -> pure w

setWallet
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadError Error m
  => WalletNickname
  -> TestWallet
  -> m Unit
setWallet walletName wallet = do
  walletsRef <- asks _.wallets
  liftEffect do
    wallets <- Ref.read walletsRef
    Ref.write (Bimap.insert walletName wallet wallets) walletsRef
