module AppM
  ( AppM
  , runAppM
  ) where

import Prologue

import Clipboard (class MonadClipboard, copy)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, runReaderT)
import Control.Monad.Reader.Class (class MonadAsk)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Data.MnemonicPhrase (class CheckMnemonic, toWords, wordToString)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Env (Env)
import Halogen (Component)
import Halogen.Component (hoist)
import Halogen.Store.Monad
  ( class MonadStore
  , StoreT
  , emitSelected
  , getStore
  , runStoreT
  , updateStore
  )
import Marlowe (postApiWalletCentralizedtestnetCheckmnemonic)
import Marlowe.Run.Webserver.Wallet.CentralizedTestnet.Types (CheckPostData(..))
import Store as Store

newtype AppM a = AppM (ReaderT Env (StoreT Store.Action Store.Store Aff) a)

runAppM
  :: forall q i o
   . Env
  -> Store.Store
  -> Component q i o AppM
  -> Aff (Component q i o Aff)
runAppM env store =
  runStoreT store Store.reduce <<< hoist \(AppM r) -> runReaderT r env

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM where
  getStore = AppM (lift getStore)
  updateStore = AppM <<< lift <<< updateStore
  emitSelected = AppM <<< lift <<< emitSelected

derive newtype instance monadAskAppM :: MonadAsk Env AppM

derive newtype instance monadReaderAppM :: MonadReader Env AppM

instance monadClipboardAppM :: MonadClipboard AppM where
  copy = liftEffect <<< copy

instance checkMnemonicAppM :: CheckMnemonic AppM where
  checkMnemonic =
    map (either (const false) identity)
      <<< runExceptT
      <<< postApiWalletCentralizedtestnetCheckmnemonic
      <<< CheckPostData
      <<< map wordToString
      <<< toWords
