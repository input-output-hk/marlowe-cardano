module Capability.PlutusApps.FollowerApp
  ( class FollowerApp
  , FollowContractError
  , ensureFollowerContract
  , followNewContract
  , onNewObservableState
  ) where

import Prologue

import AppM (AppM)
import Capability.PAB (class ManagePAB, subscribeToPlutusApp)
import Capability.PAB (activateContract, invokeEndpoint) as PAB
import Control.Concurrent.EventBus as EventBus
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (class MonadError)
import Control.Monad.Except (ExceptT(..), runExceptT, withExceptT)
import Control.Monad.Reader (class MonadAsk, ReaderT, asks)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Argonaut (encodeJson)
import Data.Either (either)
import Data.Lens (view, (^.))
import Data.PABConnectedWallet (PABConnectedWallet, _walletId)
import Data.Traversable (for_)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Aff (Aff, Error)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Env (Env, _followerBus)
import Errors (class Debuggable, class Explain, debuggable)
import Halogen (HalogenM)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
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
  -- This function follows a new marlowe contract and returns an await function so
  -- that the caller knows when the contract is correctly followed
  followNewContract
    :: PABConnectedWallet
    -> MarloweParams
    -> m (Aff (Either FollowContractError (PlutusAppId /\ ContractHistory)))

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
      Just plutusAppId -> pure $ Right plutusAppId
      -- If we don't, lets activate and follow one
      Nothing -> activateAndfollowContract wallet marloweParams

  followNewContract wallet marloweParams = do
    mFollowAppId <- activateAndfollowContract wallet marloweParams
    bus <- asks $ view _followerBus
    pure $ either
      (pure <<< Left)
      ( \followAppId ->
          EventBus.subscribeOnce bus.emitter followAppId
            <#> (\history -> Right $ followAppId /\ history)
      )
      mFollowAppId

instance FollowerApp m => FollowerApp (HalogenM state action slots msg m) where
  ensureFollowerContract wallet marloweParams = lift $ ensureFollowerContract
    wallet
    marloweParams
  followNewContract wallet marloweParams = lift $ followNewContract
    wallet
    marloweParams

instance FollowerApp m => FollowerApp (ReaderT r m) where
  ensureFollowerContract wallet marloweParams = lift $ ensureFollowerContract
    wallet
    marloweParams
  followNewContract wallet marloweParams = lift $ followNewContract
    wallet
    marloweParams

activateAndfollowContract
  :: forall m
   . ManagePAB m
  => PABConnectedWallet
  -> MarloweParams
  -> m (Either FollowContractError PlutusAppId)
activateAndfollowContract wallet marloweParams = runExceptT do
  let walletId = wallet ^. _walletId
  -- Create a new instance of a MarloweFollower plutus contract
  followAppId <- withExceptT ActivateContractError
    $ ExceptT
    $ PAB.activateContract
        MarloweFollower
        walletId
  -- Tell it to follow the Marlowe contract via its MarloweParams
  withExceptT FollowContractError
    $ ExceptT
    $ PAB.invokeEndpoint
        followAppId
        "follow"
        marloweParams
  -- Subscribe to notifications
  lift $ subscribeToPlutusApp followAppId
  pure followAppId

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
    $ Store.AddFollowerContract followerAppId metadata contractHistory
