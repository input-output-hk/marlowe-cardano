module Test.Marlowe.Run where

import Prologue

import AppM (runAppM)
import Concurrent.Queue (Queue)
import Concurrent.Queue as Queue
import Control.Apply (lift2)
import Control.Concurrent.AVarMap as AVarMap
import Control.Concurrent.EventBus as EventBus
import Control.Logger.Effect.Test (testLogger)
import Control.Monad.Error.Class
  ( class MonadError
  , class MonadThrow
  , catchError
  , throwError
  )
import Control.Monad.Now (class MonadTime)
import Control.Monad.Reader
  ( class MonadAsk
  , class MonadReader
  , ReaderT
  , asks
  , runReaderT
  )
import Control.Parallel (parallel, sequential)
import Data.AddressBook (AddressBook)
import Data.AddressBook as AddressBook
import Data.BigInt.Argonaut (BigInt)
import Data.Bimap (Bimap)
import Data.Bimap as Bimap
import Data.LocalContractNicknames
  ( LocalContractNicknames
  , emptyLocalContractNicknames
  )
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (over)
import Data.Time.Duration (Minutes(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Wallet (SyncStatus(..), WalletDetails)
import Effect (Effect)
import Effect.Aff (Aff, Error, bracket, error, finally, launchAff_)
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
import Test.Halogen (class MonadHalogenTest, runUITest)
import Test.Marlowe.Run.Action.Types
  ( Address
  , PubKeyHash
  , WalletId
  , WalletMnemonic
  , WalletName
  )
import Test.Network.HTTP
  ( class MonadMockHTTP
  , MockHttpM
  , RequestBox
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
       => MonadTime m
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
       => MonadTime m
       => m Unit
     )
  -> Spec Unit
marloweRunTestWith name addressBook contractNicknames wallet test = it name $
  bracket
    (liftAff mkTestEnv)
    (\(_ /\ coenv /\ _) -> liftEffect $ coenv.dispose)
    \(env /\ coenv /\ errors) -> do
      requests <- Queue.new
      clocks <- liftEffect $ Ref.new Map.empty
      nowRef <- liftEffect $ Ref.new unixEpoch
      nextClockId <- liftEffect $ Ref.new 0
      let mockTimeEnv = { clocks, nowRef, tzOffset: Minutes zero, nextClockId }
      let
        store = mkStore unixEpoch addressBook contractNicknames wallet
      { component } <- runAppM env store mkMainFrame
      errorAVar <- AVar.empty
      sub <- liftEffect
        $ HS.subscribe errors.emitter
        $ launchAff_ <<< flip AVar.kill errorAVar
      finally (liftEffect $ HS.unsubscribe sub) do
        sequential ado
          parallel do
            runUITest
              ( H.hoist
                  ( marshallErrors errors.listener
                      <<< flip runMockTimeM mockTimeEnv
                      <<< flip runMockHttpM requests
                  )
                  component
              )
              unit
              (runMarloweTestM test coenv requests mockTimeEnv)
            AVar.put unit errorAVar
          parallel $ AVar.take errorAVar
          in unit

newtype MarloweTestM (m :: Type -> Type) a = MarloweTestM
  (ReaderT Coenv (MockHttpM (MockTimeM m)) a)

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

runMarloweTestM
  :: forall m a
   . MonadAff m
  => MarloweTestM m a
  -> Coenv
  -> Queue RequestBox
  -> MockTimeEnv
  -> m a
runMarloweTestM (MarloweTestM m) coenv requests timeEnv =
  flip runMockTimeM timeEnv $ flip runMockHttpM requests $ runReaderT m coenv

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
      }
  pure $ env /\ coenv /\ errors

type TestWallet =
  { address :: Address
  , mnemonic :: WalletMnemonic
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
  , wallets :: Ref (Bimap WalletName TestWallet)
  }

fundWallet
  :: forall m
   . MonadAsk Coenv m
  => MonadEffect m
  => MonadError Error m
  => WalletName
  -> CurrencySymbol
  -> TokenName
  -> BigInt
  -> m Unit
fundWallet walletName currencySymbol tokenName amount = do
  walletsRef <- asks _.wallets
  walletFunds <- asks _.walletFunds
  wallets <- liftEffect $ Ref.read walletsRef
  wallet <- case Bimap.lookupL walletName wallets of
    Nothing -> throwError $ error $ "Test error: unknown wallet " <> walletName
    Just w -> pure w
  liftEffect do
    let
      alter
        :: forall k v. Ord k => k -> (Maybe v -> Maybe v) -> Map k v -> Map k v
      alter = flip Map.alter
      addAsset = alter currencySymbol $ Just <<< case _ of
        Nothing -> Map.singleton tokenName amount
        Just tokens -> alter tokenName (lift2 add (pure amount)) tokens
      assets = over Assets addAsset wallet.assets
    let newWallet = wallet { assets = assets }
    Ref.write (Bimap.insert walletName newWallet wallets) walletsRef
    HS.notify walletFunds { sync: Synchronized, assets }
