module AppM
  ( AppM
  , runAppM
  ) where

import Prologue
import Clipboard (class MonadClipboard, copy)
import Control.Monad.Reader.Class (class MonadAsk)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (Component)
import Halogen.Store.Monad (class MonadStore, StoreT, getStore, runStoreT)
import Safe.Coerce (coerce)
import Store as Store

newtype AppM a
  = AppM (StoreT Store.Action Store.Store Aff a)

runAppM
  :: forall q i o
   . Store.Store
  -> Component q i o AppM
  -> Aff (Component q i o Aff)
runAppM store = runStoreT store Store.reduce <<< coerce

derive newtype instance functorAppM :: Functor AppM

derive newtype instance applyAppM :: Apply AppM

derive newtype instance applicativeAppM :: Applicative AppM

derive newtype instance bindAppM :: Bind AppM

derive newtype instance monadAppM :: Monad AppM

derive newtype instance monadEffectAppM :: MonadEffect AppM

derive newtype instance monadAffAppM :: MonadAff AppM

derive newtype instance monadStoreAppM ::
  MonadStore Store.Action Store.Store AppM

-- The MonadAsk instance is redundant with MonadStore, but I add it here so I don't
-- modify the code that used to work with it.
instance monadAskAppM :: MonadAsk Store.Store AppM where
  ask = getStore

instance monadClipboardAppM :: MonadClipboard AppM where
  copy = liftEffect <<< copy
