module AppM where

import Prologue
import Control.Monad.Reader.Trans (class MonadAsk)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Safe.Coerce (coerce)
import Store (Env, Store, toEnv)
import Store as Store

newtype AppM a
  = AppM (StoreT Store.Action Store Aff a)

runAppM :: forall i o q. Store -> H.Component q i o AppM -> Aff (H.Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadStoreAppM :: MonadStore Store.Action Store.Store AppM

instance monadAskAppM :: MonadAsk Env AppM where
  ask = toEnv <$> getStore
