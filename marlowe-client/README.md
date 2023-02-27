# Marlowe Client

A client library for the Marlowe Runtime

## Usage

```hs
module Main where

import Language.Marlowe.Runtime.Client

main :: IO ()
main = connectToMarloweRuntime "localhost" 3700 do
  -- Stay in sync with a contract
  runMarloweSyncClient mySyncClient

  -- Stay in sync with contract creations
  runMarloweHeaderSyncClient myHeaderSyncClient

  -- Query the state of on-chain contracts
  _ <- runMarloweQueryClient $ getContractHeaders Nothing

  -- Interact with contracts
  contractCreated <- createContract Nothing MarloweV1 wallet roleTokens mempty minAdaDeposit myContract
  -- Sign with method of choice
  submitAndWait $ signTx $ case contractCreated of ContractCreated{..} -> txBody

  inputsApplied <- applyInputs MarloweV1 wallet contractId mempty [IDeposit account fromParty token quantity]
  submitAndWait $ signTx $ case inputsApplied of InputsApplied{..} -> txBody

  txBody <- withdraw MarloweV1 wallet contractId role
  submitAndWait $ signTx txBody
```

The library provides a typeclass `MonadMarlowe` in which a client peer of the
Marlowe protocol can be launched at any time. The underlying monad is
responsible for handling the connection to the underlying runtime instance. A
concrete monad transformer `MarloweT` offers an implementation of this
typeclass, which can be executed via the main entrypoint function
`connectToMarloweRuntime`.
