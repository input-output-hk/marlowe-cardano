module API.Lenses
  ( _cicContract
  , _cicCurrentState
  , _cicDefinition
  , _cicWallet
  , _observableState
  , _hooks
  , _rqRequest
  , _endpointDescription
  , _endpointDescriptionString
  ) where

import Prologue
import Data.Lens (Lens')
import Data.Lens.Record (prop)
import Data.RawJson (RawJson)
import Type.Proxy (Proxy(..))
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
import Wallet.Emulator.Wallet (Wallet)
import Wallet.Types
  ( ContractInstanceId
  , EndpointDescription
  , _EndpointDescription
  )

_cicContract :: Lens' (ContractInstanceClientState MarloweContract)
  ContractInstanceId
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

----------
_observableState :: Lens' (PartiallyDecodedResponse ActiveEndpoint) RawJson
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
