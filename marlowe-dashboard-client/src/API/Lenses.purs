module API.Lenses
  ( _cicContract
  , _cicCurrentState
  , _cicDefinition
  , _cicStatus
  , _cicWallet
  , _endpointDescription
  , _endpointDescriptionString
  , _hooks
  , _observableState
  , _rqRequest
  ) where

import Prologue

import Data.Argonaut (Json)
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Marlowe.PAB (PlutusAppId)
import MarloweContract (MarloweContract)
import Plutus.Contract.Effects (ActiveEndpoint, _ActiveEndpoint)
import Plutus.Contract.Resumable (Request, _Request)
import Plutus.PAB.Events.ContractInstanceState
  ( PartiallyDecodedResponse
  , _PartiallyDecodedResponse
  )
import Plutus.PAB.Webserver.Types
  ( ContractInstanceClientState
  , _ContractInstanceClientState
  )
import Type.Proxy (Proxy(..))
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Types
  ( ContractActivityStatus
  , EndpointDescription
  , _EndpointDescription
  )

_cicContract :: Lens' (ContractInstanceClientState MarloweContract)
  PlutusAppId
_cicContract = _ContractInstanceClientState <<< prop (Proxy :: _ "cicContract")

_cicCurrentState :: Lens' (ContractInstanceClientState MarloweContract)
  (PartiallyDecodedResponse ActiveEndpoint)
_cicCurrentState = _ContractInstanceClientState <<< prop
  (Proxy :: _ "cicCurrentState")

_cicDefinition :: Lens' (ContractInstanceClientState MarloweContract)
  MarloweContract
_cicDefinition = _ContractInstanceClientState <<< prop
  (Proxy :: _ "cicDefinition")

_cicWallet :: Lens' (ContractInstanceClientState MarloweContract) Wallet
_cicWallet = _ContractInstanceClientState <<< prop (Proxy :: _ "cicWallet")

_cicStatus :: Lens' (ContractInstanceClientState MarloweContract)
  ContractActivityStatus
_cicStatus = _ContractInstanceClientState <<< prop (Proxy :: _ "cicStatus")

----------
_observableState :: Lens' (PartiallyDecodedResponse ActiveEndpoint) Json
_observableState = _PartiallyDecodedResponse <<< prop
  (Proxy :: _ "observableState")

_hooks :: Lens' (PartiallyDecodedResponse ActiveEndpoint)
  (Array (Request ActiveEndpoint))
_hooks = _PartiallyDecodedResponse <<< prop (Proxy :: _ "hooks")

_rqRequest :: Lens' (Request ActiveEndpoint) ActiveEndpoint
_rqRequest = _Request <<< prop (Proxy :: _ "rqRequest")

_endpointDescription :: Lens' ActiveEndpoint EndpointDescription
_endpointDescription = _ActiveEndpoint <<< prop (Proxy :: _ "aeDescription")

_endpointDescriptionString :: Lens' EndpointDescription String
_endpointDescriptionString = _EndpointDescription <<< prop
  (Proxy :: _ "getEndpointDescription")
