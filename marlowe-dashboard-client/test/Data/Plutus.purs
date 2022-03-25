{-
   Test data factory for plutus API types.
-}
module Test.Data.Plutus where

import Prologue

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.UUID.Argonaut (UUID)
import Marlowe.PAB (PlutusAppId(..))
import Plutus.Contract.Effects (ActiveEndpoint(..))
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient(..)
  , CombinedWSStreamToServer(..)
  , InstanceStatusToClient(..)
  )
import Wallet.Types (EndpointDescription(..))

-------------------------------------------------------------------------------
-- Websocket traffic
-------------------------------------------------------------------------------

-- | Create a `NewObservableState` `InstanceStatusToClient` with anything
-- | serializable to JSON.
newObservableState :: forall a. EncodeJson a => a -> InstanceStatusToClient
newObservableState = NewObservableState <<< encodeJson

-- | Create a `NewActiveEndpoints` `InstanceStatusToClient` for the given
-- | endpoints.
newActiveEndpoints :: Array String -> InstanceStatusToClient
newActiveEndpoints = NewActiveEndpoints <<< map
  ( ActiveEndpoint
      <<< { aeMetadata: Nothing, aeDescription: _ }
      <<< EndpointDescription
      <<< { getEndpointDescription: _ }
  )

-- | Create a `InstabceUpdate` `CombinedWSStreamToClient`.
instanceUpdate :: UUID -> InstanceStatusToClient -> CombinedWSStreamToClient
instanceUpdate = InstanceUpdate <<< PlutusAppId

-- | Create a `Subscribe <<< Left` `CombinedWSStreamToServer` (`Right` is
-- | deprecated and unused).
subscribeApp :: UUID -> CombinedWSStreamToServer
subscribeApp = Subscribe <<< Left <<< PlutusAppId
