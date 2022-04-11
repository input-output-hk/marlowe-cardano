module Language.Marlowe.Client.Error where

import Prologue

import Data.Argonaut (stringify)
import Language.Marlowe.Client (MarloweError(..))
import Plutus.Contract.Error as Plutus
import Wallet.Types (EndpointValue(..))

showContractError :: MarloweError -> String
showContractError TransitionError =
  "Transition error"
showContractError AmbiguousOnChainState =
  "Ambiguous on chain state"
showContractError UnableToExtractTransition =
  "Unable to extract transition"
showContractError OnChainStateNotFound =
  "On chain state not found"
showContractError (MarloweEvaluationError error) =
  "Marlowe evaluation error: " <> show error
showContractError (OtherContractError (Plutus.WalletContractError error)) =
  "Wallet error: " <> show error
showContractError
  (OtherContractError (Plutus.ChainIndexContractError message response)) =
  "Chain index error " <> message <> ": " <> show response
showContractError
  (OtherContractError (Plutus.EmulatorAssertionContractError error)) =
  "Emulator assertion error: " <> show error
showContractError
  (OtherContractError (Plutus.ConstraintResolutionContractError error)) =
  "Constraint resolution error: " <> show error
showContractError (OtherContractError (Plutus.ResumableContractError error)) =
  "Resumable contract error: " <> show error
showContractError (OtherContractError (Plutus.CCheckpointContractError error)) =
  "Checkpoint error: " <> show error
showContractError
  ( OtherContractError
      ( Plutus.EndpointDecodeContractError
          { eeEndpointDescription
          , eeEndpointValue: EndpointValue { unEndpointValue: value }
          , eeErrorMessage
          }
      )
  ) =
  "Endpoint decode error " <> eeErrorMessage <> ", "
    <> show eeEndpointDescription
    <> ", with value "
    <> stringify value
showContractError (OtherContractError (Plutus.OtherContractError message)) =
  "Other contract error: " <> message
