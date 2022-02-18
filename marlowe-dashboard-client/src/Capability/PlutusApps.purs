module Capability.PlutusApps where

import Prologue

import Capability.PAB (class ManagePAB)
import Capability.PAB (invokeEndpoint) as PAB
import Control.Monad.Except (ExceptT(..), runExceptT)
import Data.Argonaut (Json)
import Data.Argonaut.Encode (encodeJson)
import Data.Array ((:))
import Data.UUID.Argonaut (UUID, genUUID)
import Effect.AVar (AVar)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Marlowe.PAB (PlutusAppId)
import Types (AjaxResponse)

invokeEndpointAvar
  :: forall m
   . MonadAff m
  => ManagePAB m
  => AVar Unit
  -> PlutusAppId
  -> String
  -> Array Json
  -> m (AjaxResponse UUID)
invokeEndpointAvar endpointAVar plutusAppId endpointName payload = runExceptT do
  -- Check if the endpoint is available to make a request
  -- TODO: we could later add a forkAff with a timer that unlocks this timer if we
  --       dont get a response
  -- TODO: We could change this take for a read and listen for a 500 error with EndpointUnavailabe
  --       to retry and take it (instead of preemptively taking it).
  reqId <- liftEffect genUUID
  liftAff $ AVar.take endpointAVar
  ExceptT
    $ PAB.invokeEndpoint plutusAppId endpointName (encodeJson reqId : payload)
  pure reqId
