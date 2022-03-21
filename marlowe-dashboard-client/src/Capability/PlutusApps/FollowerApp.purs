module Capability.PlutusApps.FollowerApp
  ( class FollowerApp
  , FollowContractError
  , ensureFollowerContract
  , onNewObservableState
  ) where

import Prologue

import AppM (AppM)
import Capability.PAB (activateContract, invokeEndpoint) as PAB
import Capability.PAB (subscribeToPlutusApp)
import Control.Concurrent.AVarMap as AVarMap
import Control.Concurrent.EventBus as EventBus
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.Except (ExceptT(..), except, runExceptT, withExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (encodeJson)
import Data.Lens (view, (^.))
import Data.PABConnectedWallet (PABConnectedWallet, _walletId)
import Data.Traversable (for_)
import Effect.Aff (Error, error)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Env (Env, _followerAVarMap, _followerBus)
import Errors.Debuggable (class Debuggable, debuggable)
import Errors.Explain (class Explain)
import Halogen (HalogenM)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Halogen.Subscription as HS
import Halogen.Subscription.Extra (subscribeOnce)
import Language.Marlowe.Client (ContractHistory)
import Marlowe.Client (getContract)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.PAB (PlutusAppId)
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

class Monad m <= FollowerApp m where
  -- This function makes sure that there is a follower contract for the specified
  -- marloweParams. It tries to see if we are already following, and if not, it creates
  -- a new one.
  ensureFollowerContract
    :: PABConnectedWallet
    -> MarloweParams
    -> m (Either FollowContractError PlutusAppId)

instance
  ( MonadAff m
  , MonadRec m
  , MonadError Error m
  , MonadAjax PAB.Api m
  ) =>
  FollowerApp (AppM m) where
  ensureFollowerContract wallet marloweParams = do
    contracts <- _.contracts <$> getStore
    case getFollowerContract marloweParams contracts of
      -- If we already have a follower contract, we don't need to do anything
      Just plutusAppId -> do
        pure $ Right plutusAppId
      -- If we don't, lets activate and follow one
      Nothing -> do
        -- Try to get an emitter, in case a call to activate has already been started.
        followerAVarMap <- asks $ view _followerAVarMap
        newIO <- liftEffect $ HS.create
        emitterWasPut <-
          AVarMap.tryPut marloweParams newIO.emitter followerAVarMap
        runExceptT
          if emitterWasPut then do
            result <- try do
              let walletId = wallet ^. _walletId
              -- Create a new instance of a MarloweFollower plutus contract
              followAppId <- withExceptT ActivateContractError
                $ ExceptT
                $ PAB.activateContract MarloweFollower walletId
              -- Subscribe to notifications
              lift $ subscribeToPlutusApp followAppId
              -- Tell it to follow the Marlowe contract via its MarloweParams
              withExceptT FollowContractError
                $ ExceptT
                $ PAB.invokeEndpoint followAppId "follow" marloweParams
              liftEffect $ HS.notify newIO.listener followAppId
              pure followAppId
            AVarMap.kill marloweParams (error "killed") followerAVarMap
            except result
          else do
            emitter <- AVarMap.read marloweParams followerAVarMap
            liftAff $ subscribeOnce emitter

instance FollowerApp m => FollowerApp (HalogenM state action slots msg m) where
  ensureFollowerContract wallet marloweParams = lift $ ensureFollowerContract
    wallet
    marloweParams

instance FollowerApp m => FollowerApp (ReaderT r m) where
  ensureFollowerContract wallet marloweParams = lift $ ensureFollowerContract
    wallet
    marloweParams

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
    mMetadata = _.metaData <$> (findTemplate $ getContract contractHistory)
  -- FIXME-3208 I don't like that this step requires to find the metadata again
  --            but I wanted to reutilize the logic of always regenerating the Execution
  --            state from the contract history. I may need to move back the metadata to a
  --            "parent view state".
  --            If I continue to reutilize AddFollowerContract then I might rename it to
  --            UpdateFollowerContract, RestoreContract or similar
  for_ mMetadata \metadata -> updateStore
    $ Store.ContractHistoryUpdated followerAppId metadata contractHistory
