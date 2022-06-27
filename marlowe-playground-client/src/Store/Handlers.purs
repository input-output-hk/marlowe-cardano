module Store.Handlers where

import Prelude

import Contrib.Halogen.Store (useStore)
import Control.Monad.Trans.Class (class MonadTrans, lift)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Marlowe (Api)
import Network.RemoteData as RemoteData
import Servant.PureScript (class MonadAjax)
import Session as Auth
import Store as S

isAuthenticated
  :: forall m
   . Bind m
  => MonadStore S.Action S.State m
  => m Boolean
isAuthenticated = do
  authResponse <- useStore S._authResponse
  liftEffect $ Auth.isAuthenticated authResponse

-- This heler can be used in the context of `HalogenM` and `HookM`.
-- That is why it is generalized to `MonadTrans` stuff.
loginRequired
  :: forall m t
   . Monad (t m)
  => MonadStore S.Action S.State m
  => MonadAff m
  => MonadAjax Api m
  => MonadTrans t
  => t m Unit
  -> (Auth.Session -> t m Unit)
  -> t m Unit
loginRequired onFailure onSuccess = do
  whenM (not <$> lift isAuthenticated) do
    authResponse <- lift Auth.login
    lift $ updateStore $ S.SetAuthResponse authResponse

  lift (useStore S._authResponse) >>= RemoteData.toMaybe >>> join >>> case _ of
    Just s -> onSuccess s
    Nothing -> onFailure
