{-
   Test data factory for plutus API types.
-}
module Test.Data.Plutus where

import Prologue

import Data.Argonaut (class EncodeJson, encodeJson)
import Data.FunctorWithIndex (mapWithIndex)
import Data.UUID.Argonaut (UUID)
import Data.WalletId (WalletId)
import Data.WalletId as WI
import Marlowe.PAB (PlutusAppId(..))
import MarloweContract (MarloweContract)
import Plutus.Contract.Effects (ActiveEndpoint(..))
import Plutus.Contract.Resumable (IterationID(..), Request(..), RequestID(..))
import Plutus.PAB.Events.ContractInstanceState (PartiallyDecodedResponse(..))
import Plutus.PAB.Webserver.Types
  ( CombinedWSStreamToClient(..)
  , CombinedWSStreamToServer(..)
  , ContractActivationArgs(..)
  , ContractInstanceClientState(..)
  , InstanceStatusToClient(..)
  )
import Wallet.Emulator.Wallet (Wallet(..))
import Wallet.Types (ContractActivityStatus(..), EndpointDescription(..))

-------------------------------------------------------------------------------
-- HTTP API
-------------------------------------------------------------------------------

type MarloweContractInstanceClientState =
  ContractInstanceClientState MarloweContract

wallet :: WalletId -> Wallet
wallet walletId = Wallet
  { getWalletId: WI.toString walletId, prettyWalletName: Nothing }

contractActivationArgs :: forall a. WalletId -> a -> ContractActivationArgs a
contractActivationArgs walletId caID = ContractActivationArgs
  { caWallet: Just $ wallet walletId
  , caID
  }

appInstanceDone
  :: forall observableState
   . EncodeJson observableState
  => WalletId
  -> MarloweContract
  -> UUID
  -> observableState
  -> MarloweContractInstanceClientState
appInstanceDone = appInstance Done []

appInstanceStopped
  :: forall observableState
   . EncodeJson observableState
  => WalletId
  -> MarloweContract
  -> UUID
  -> observableState
  -> MarloweContractInstanceClientState
appInstanceStopped = appInstance Stopped []

appInstanceActive
  :: forall observableState
   . EncodeJson observableState
  => Array String
  -> WalletId
  -> MarloweContract
  -> UUID
  -> observableState
  -> MarloweContractInstanceClientState
appInstanceActive = appInstance Active

appInstance
  :: forall observableState
   . EncodeJson observableState
  => ContractActivityStatus
  -> Array String
  -> WalletId
  -> MarloweContract
  -> UUID
  -> observableState
  -> MarloweContractInstanceClientState
appInstance cicStatus endpoints walletId cicDefinition instanceId state =
  ContractInstanceClientState
    { cicWallet: wallet walletId
    , cicCurrentState: PartiallyDecodedResponse
        { lastLogs: []
        , err: Nothing
        , hooks: endpoints # mapWithIndex \rqID endpoint -> Request
            { rqID: RequestID rqID
            , itID: IterationID 0
            , rqRequest: activeEndpoint endpoint
            }
        , logs: []
        , observableState: encodeJson state
        }
    , cicContract: PlutusAppId instanceId
    , cicStatus
    , cicYieldedExportTxs: []
    , cicDefinition
    }

-- | Create an `ActiveEndpoint` from a string.
activeEndpoint :: String -> ActiveEndpoint
activeEndpoint = ActiveEndpoint
  <<< { aeMetadata: Nothing, aeDescription: _ }
  <<< EndpointDescription
  <<< { getEndpointDescription: _ }

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
newActiveEndpoints = NewActiveEndpoints <<< map activeEndpoint

-- | Create a `InstabceUpdate` `CombinedWSStreamToClient`.
instanceUpdate :: UUID -> InstanceStatusToClient -> CombinedWSStreamToClient
instanceUpdate = InstanceUpdate <<< PlutusAppId

-- | Create a `Subscribe <<< Left` `CombinedWSStreamToServer` (`Right` is
-- | deprecated and unused).
subscribeApp :: UUID -> CombinedWSStreamToServer
subscribeApp = Subscribe <<< Left <<< PlutusAppId
