module Capability.PlutusApps.FollowerApp
  ( class FollowerApp
  , FollowContractError
  , StopFollowerError
  , followContract
  , stopFollower
  , onNewObservableState
  ) where

import Prologue

import AppM (AppM)
import Capability.PAB (activateContract, invokeEndpoint) as PAB
import Capability.PAB (stopContract, subscribeToPlutusApp)
import Control.Concurrent.AVarMap as AVarMap
import Control.Concurrent.EventBus as EventBus
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Fork.Class (class MonadBracket, bracket)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (encodeJson)
import Data.Bifunctor (lmap)
import Data.Lens (view, (^.))
import Data.Maybe (maybe)
import Data.PABConnectedWallet (PABConnectedWallet, _walletId)
import Data.Set as Set
import Effect.Aff (Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Env (Env, _followerAVarMap, _followerBus)
import Errors.Debuggable (class Debuggable, debuggable)
import Errors.Explain (class Explain)
import Halogen (HalogenM)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Client (getContract)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.Extended.Metadata (emptyContractMetadata)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Run.Server as Marlowe
import Marlowe.Semantics (MarloweParams)
import MarloweContract (MarloweContract(..))
import Plutus.PAB.Webserver (Api) as PAB
import Servant.PureScript (class MonadAjax)
import Store as Store
import Store.Contracts (getFollowerContract)
import Text.Pretty (text)
import Types (JsonAjaxError)

data FollowContractError
  = ActivateContractError JsonAjaxError
  | FollowContractError JsonAjaxError

instance Explain FollowContractError where
  explain (ActivateContractError _) = text "Could not activate the contract"
  explain (FollowContractError _) = text "Could not follow the contract"

instance Debuggable FollowContractError where
  debuggable (ActivateContractError error) = encodeJson
    { type: "FollowContractError.ActivateContractError"
    , error: debuggable error
    }
  debuggable (FollowContractError error) = encodeJson
    { type: "FollowContractError.FollowContractError"
    , error: debuggable error
    }

data StopFollowerError = StopError JsonAjaxError

instance Explain StopFollowerError where
  explain (StopError _) = text "Failed to stop the contract follower."

instance Debuggable StopFollowerError where
  debuggable (StopError error) = encodeJson
    { type: "StopFollowerError.StopError"
    , error: debuggable error
    }

class Monad m <= FollowerApp m where
  -- This function makes sure that there is a follower contract for the specified
  -- marloweParams. It tries to see if we are already following, and if not, it creates
  -- a new one.
  followContract
    :: PABConnectedWallet
    -> MarloweParams
    -> m (Either FollowContractError PlutusAppId)
  stopFollower :: PlutusAppId -> m (Either StopFollowerError Unit)

instance
  ( MonadAff m
  , MonadRec m
  , MonadError Error m
  , MonadBracket Error f m
  , MonadAjax PAB.Api m
  , MonadAjax Marlowe.Api m
  ) =>
  FollowerApp (AppM m) where
  followContract wallet marloweParams =
    -- After we retreive the contract store to check if a follower exists, and
    -- before the the follower is added to the store, we are vulnerable to a
    -- race condition in which multiple followers may be created for the same
    -- set of marlowe params. To mitigate this risk, we create a critical
    -- section around the call for the specific marlowe commands. Simultaneous
    -- attempts to activate a follower will be forced to run in sequence. After
    -- the first succeeds, subsequent attempts will instead find a cached
    -- instance ID for that marlowe params in the store.
    bracket lockFollowerActivation unlockFollowerActivation \_ ->
      do
        contracts <- _.contracts <$> getStore
        case getFollowerContract marloweParams contracts of
          -- If we already have a follower contract, we don't need to do anything
          Just plutusAppId -> do
            pure $ Right plutusAppId
          -- If we don't, activate and follow one
          Nothing -> runExceptT do
            let walletId = wallet ^. _walletId
            -- Create a new instance of a MarloweFollower plutus contract
            followAppId <- withExceptT ActivateContractError
              $ ExceptT
              $ PAB.activateContract MarloweFollower walletId
            -- Add it to the store's index
            updateStore
              $ Store.FollowerAppsActivated
              $ Set.singleton
              $ Tuple marloweParams followAppId
            -- Subscribe to notifications
            lift $ subscribeToPlutusApp followAppId
            -- Tell it to follow the Marlowe contract via its MarloweParams
            withExceptT FollowContractError
              $ ExceptT
              $ PAB.invokeEndpoint walletId followAppId "follow" marloweParams
            pure followAppId
    where
    lockFollowerActivation = do
      followerAVarMap <- asks $ view _followerAVarMap
      AVarMap.put marloweParams unit followerAVarMap
      pure followerAVarMap
    unlockFollowerActivation _ = AVarMap.take marloweParams

  stopFollower appId = do
    updateStore $ Store.FollowerAppClosed appId
    result <- stopContract appId
    pure $ lmap StopError result

instance FollowerApp m => FollowerApp (HalogenM state action slots msg m) where
  followContract wallet marloweParams = lift $ followContract
    wallet
    marloweParams
  stopFollower = lift <<< stopFollower

instance FollowerApp m => FollowerApp (ReaderT r m) where
  followContract wallet marloweParams = lift $ followContract
    wallet
    marloweParams
  stopFollower = lift <<< stopFollower

{- [UC-CONTRACT-1][4] Start a new marlowe contract
   [UC-CONTRACT-2][X] Receive a role token for a marlowe contract
If we started a contract (or someone else started one and gave us a role in it), we will have
created a `MarloweFollower` app for that contract, and started following the contract with that
`MarloweFollower` app. Since we will also be subscribed to that app, we will receive an update
about its initial state through the WebSocket. We potentially use that to change the corresponding
`Contract.State` from `Starting` to `Started`.
-}
{- [UC-CONTRACT-3][2] Apply an input to a contract -}
onNewObservableState
  :: forall m
   . MonadStore Store.Action Store.Store m
  => MonadAsk Env m
  => PlutusAppId
  -> ContractHistory
  -> m Unit
onNewObservableState followerAppId contractHistory = do
  bus <- asks $ view _followerBus
  liftEffect $ EventBus.notify bus.listener followerAppId contractHistory

  let
    metadata = maybe
      emptyContractMetadata
      _.metaData
      (findTemplate $ getContract contractHistory)

  updateStore
    $ Store.ContractHistoryUpdated followerAppId metadata contractHistory
