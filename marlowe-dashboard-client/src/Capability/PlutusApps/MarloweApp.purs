-- This capability exposes the API for the Plutus Aplication called `MarloweApp`, which
-- has it's endpoints defined as the MarloweSchema in Language.Marlowe.Client module.
-- There is one `MarloweApp` per wallet, and it serves as a control app for all
-- the Marlowe contracts that the wallet has a role token.
module Capability.PlutusApps.MarloweApp
  ( class MarloweApp
  , createContract
  , applyInputs
  , maxRequests
  , redeem
  ) where

import Prologue

import AppM (AppM)
import Bridge (toBack)
import Capability.PAB (invokeEndpoint) as PAB
import Control.Monad.Reader (asks)
import Data.Address (Address)
import Data.Argonaut.Encode (encodeJson)
import Data.Array (take, (:))
import Data.Lens (_1, over, view)
import Data.Map (Map)
import Data.Map as Map
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID.Argonaut (UUID, genUUID)
import Effect.Aff.AVar as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Env (_pendingResults)
import Marlowe.PAB (PlutusAppId)
import Marlowe.Semantics
  ( Contract
  , MarloweParams
  , TimeInterval(..)
  , TokenName
  , TransactionInput(..)
  )
import Plutus.V1.Ledger.Time (POSIXTime)
import Plutus.V1.Ledger.Value (TokenName) as Back
import Types (AjaxResponse)

class MarloweApp m where
  createContract
    :: PlutusAppId
    -> Map TokenName Address
    -> Contract
    -> m (AjaxResponse UUID)
  applyInputs
    :: PlutusAppId -> MarloweParams -> TransactionInput -> m (AjaxResponse Unit)
  -- TODO auto
  -- TODO close
  redeem
    :: PlutusAppId
    -> MarloweParams
    -> TokenName
    -> Address
    -> m (AjaxResponse Unit)

instance marloweAppM :: MarloweApp AppM where
  createContract plutusAppId roles contract = do
    reqId <- liftEffect genUUID
    let
      backRoles :: Map Back.TokenName Address
      backRoles = Map.fromFoldable
        $ map (over _1 toBack)
        $ (Map.toUnfoldable roles :: Array (TokenName /\ Address))

      payload = [ encodeJson reqId, encodeJson backRoles, encodeJson contract ]
    enqueueResultHandler reqId =<< PAB.invokeEndpoint
      plutusAppId
      "create"
      payload

  applyInputs
    plutusAppId
    marloweContractId
    (TransactionInput { interval: TimeInterval slotStart slotEnd, inputs }) = do
    reqId <- liftEffect genUUID
    let
      backTimeInterval :: POSIXTime /\ POSIXTime
      backTimeInterval = (slotStart) /\ (slotEnd)

      payload =
        [ encodeJson reqId
        , encodeJson marloweContractId
        , encodeJson backTimeInterval
        , encodeJson inputs
        ]
    map void <$> enqueueResultHandler reqId =<< PAB.invokeEndpoint
      plutusAppId
      "apply-inputs-nonmerkleized"
      payload

  redeem plutusAppId marloweContractId tokenName address = do
    reqId <- liftEffect genUUID
    let
      payload =
        [ encodeJson reqId
        , encodeJson marloweContractId
        , encodeJson tokenName
        , encodeJson address
        ]
    map void <$> enqueueResultHandler reqId =<< PAB.invokeEndpoint
      plutusAppId
      "redeem"
      payload

-- This is the amount of requests we store in the request queue
maxRequests :: Int
maxRequests = 15

enqueueResultHandler :: UUID -> AjaxResponse Unit -> AppM (AjaxResponse UUID)
enqueueResultHandler _ (Left e) = pure $ Left e
enqueueResultHandler reqId (Right _) = do
  pendingResults <- asks $ view _pendingResults
  liftAff do
    pendingResults' <- AVar.take pendingResults
    pendingResult <- AVar.empty
    -- We add new pending results to the begining of the array and remove them from the end.
    AVar.put
      (take maxRequests $ (reqId /\ pendingResult) : pendingResults')
      pendingResults
  pure $ Right reqId
