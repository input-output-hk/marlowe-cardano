module Main (
  main,
) where

import Data.String (fromString)
import Network.ProofSpace (application, processProofHandler, proofServerContext)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Environment (getArgs)

-- | Run a server for ProofSpace webhooks.
main :: IO ()
main =
  do
    -- Read the path to the public key PEM file, the server host, the its port, and the path to the processor.
    [pubKeyFile, host, port, processPath] <- getArgs
    -- ProofSpace uses an SHA3-256 digest for signatures and supplies
    -- a PEM format RSA public key for verifying those signatures.
    context <- proofServerContext "sha3-256" pubKeyFile
    -- Set the host and port
    let settings = setHost (fromString host) $ setPort (read port) defaultSettings
    -- Run the server
    runSettings settings . application context $ processProofHandler processPath
