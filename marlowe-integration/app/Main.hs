module Main
  where

import Control.Monad.IO.Class (liftIO)
import Test.Integration.Marlowe (withLocalMarloweRuntime)
import Test.Integration.Marlowe.Script

main :: IO ()
main = withLocalMarloweRuntime $ runMarloweScript do
  liftIO $ putStrLn "Starting script"

  alice <- allocateWallet "alice"
  liftIO $ putStrLn "Allocated wallet for Alice"

  contractRef <- submit alice $ create "close" $ buildV1Contract close
  liftIO $ putStrLn "Created contract"

  state1 <- getContractState contractRef
  assertMsg "Contract is unexpectedly closed" case state1 of
    Closed -> False
    _ -> True

  submit alice $ applyInputs contractRef $ buildV1ApplyInputs $ pure ()
  liftIO $ putStrLn "Closed contract"

  state2 <- getContractState contractRef
  assertMsg "Contract was expected to be closed" case state2 of
    Closed -> True
    _ -> False

  liftIO $ putStrLn "Script done"
