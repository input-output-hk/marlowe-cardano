module Type.Constraints where

-- | Aliases for common constriaints in the App

import AppM (AppM)
import Effect.Aff.Class (class MonadAff)
import Halogen.Store.Monad (class MonadStore)
import Marlowe (Api)
import Servant.PureScript (class MonadAjax)
import Store as Store

class
  ( MonadAff m
  , MonadStore Store.Action Store.State m
  , MonadAjax Api m
  ) <=
  MonadAffAjaxStore m

instance
  ( MonadAff m
  , MonadStore Store.Action Store.State m
  , MonadAjax Api m
  ) =>
  MonadAffAjaxStore m

else instance MonadAffAjaxStore AppM
