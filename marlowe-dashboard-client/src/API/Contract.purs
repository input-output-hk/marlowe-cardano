module API.Contract
  ( activateContract
  , deactivateContract
  , getContractInstanceClientState
  , invokeEndpoint
  , getWalletContractInstances
  , getAllContractInstances
  , getContractDefinitions
  ) where

import Prologue
import API.Request
  ( doGetRequest
  , doPostRequest
  , doPostRequestWith
  , doPutRequest
  )
import API.Url (toUrlPiece)
import Control.Monad.Error.Class (class MonadError)
import Data.Argonaut (encodeJson)
import Data.Argonaut.Encode (class EncodeJson)
import Effect.Aff.Class (class MonadAff)
import MarloweContract (MarloweContract)
import Plutus.PAB.Webserver.Types
  ( ContractActivationArgs
  , ContractInstanceClientState
  , ContractSignatureResponse
  )
import Servant.PureScript (AjaxError)
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Types (ContractInstanceId)

activateContract
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => ContractActivationArgs MarloweContract
  -> m ContractInstanceId
activateContract contractActivationArgs = doPostRequest
  "/pab/api/contract/activate"
  contractActivationArgs

deactivateContract
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => ContractInstanceId
  -> m Unit
deactivateContract contractInstanceId = doPutRequest $
  "/pab/api/contract/instance/" <> toUrlPiece contractInstanceId <> "/stop"

getContractInstanceClientState
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => ContractInstanceId
  -> m (ContractInstanceClientState MarloweContract)
getContractInstanceClientState contractInstanceId = doGetRequest $
  "/pab/api/contract/instance/" <> toUrlPiece contractInstanceId <> "/status"

invokeEndpoint
  :: forall d m
   . EncodeJson d
  => MonadError AjaxError m
  => MonadAff m
  => ContractInstanceId
  -> String
  -> d
  -> m Unit
invokeEndpoint contractInstanceId endpoint payload =
  doPostRequestWith
    -- The server encodes Unit as `[]`, because we don't care for the response we always return
    -- Unit, but we could enhance this to decode an empty array.
    { encode: encodeJson, decode: const (Right unit) }
    ( "/pab/api/contract/instance/" <> toUrlPiece contractInstanceId
        <> "/endpoint/"
        <> endpoint
    )
    payload

getWalletContractInstances
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => Wallet
  -> m (Array (ContractInstanceClientState MarloweContract))
getWalletContractInstances wallet = doGetRequest $
  "/pab/api/contract/instances/wallet/" <> toUrlPiece wallet

getAllContractInstances
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => m (Array (ContractInstanceClientState MarloweContract))
getAllContractInstances = doGetRequest "/pab/api/contract/instances"

getContractDefinitions
  :: forall m
   . MonadError AjaxError m
  => MonadAff m
  => m (Array (ContractSignatureResponse MarloweContract))
getContractDefinitions = doGetRequest "/pab/api/contract/definitions"
