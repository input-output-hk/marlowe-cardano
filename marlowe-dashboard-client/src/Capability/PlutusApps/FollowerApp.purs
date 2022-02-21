module Capability.PlutusApps.FollowerApp
  ( class FollowerApp
  , followContract
  , onNewObservableState
  ) where

import Prologue

import Data.Traversable (for_)
import Halogen.Store.Monad (class MonadStore, getStore, updateStore)
import Marlowe.Client (ContractHistory, getContract)
import Marlowe.Deinstantiate (findTemplate)
import Marlowe.PAB (PlutusAppId)
import Store as Store

class FollowerApp m where
  followContract :: m Unit

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
  => PlutusAppId
  -> ContractHistory
  -> m Unit
onNewObservableState followerAppId contractHistory = do
  currentSlot <- _.currentSlot <$> getStore

  let
    contract = getContract contractHistory
    mMetadata = _.metaData <$> findTemplate contract
  -- FIXME-3208 I don't like that this step requires to find the metadata again
  --            but I wanted to reutilize the logic of always regenerating the Execution
  --            state from the contract history. I may need to move back the metadata to a
  --            "parent view state".
  --            If I continue to reutilize AddFollowerContract then I might rename it to
  --            UpdateFollowerContract, RestoreContract or similar
  for_ mMetadata \metadata ->
    updateStore $ Store.AddFollowerContract
      currentSlot
      followerAppId
      metadata
      contractHistory
