# Webhook for ProofSpace Wallet

The `marlowe-proofspace` library provides a webhook that conforms to the [ProofSpace Interaction Webhook](https://proofspace.atlassian.net/wiki/spaces/PSM/pages/2133786630/Integration+Webhooks+API+Overview#Interaction-Webhook) specification.

Such webhooks can be integrated into a Marlowe oracle that forwards verfication of [W3C DID](https://www.w3.org/TR/did-core/) credentials from a [ProofSpace](https://www.proofspace.id/) wallet to a Marlowe contract.


## Example executable

The following executable handles ProofSpace webhook callbacks and echoes back a succcess response.

```haskell
module Main (
  main
) where

import Data.String (fromString)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import System.Environment (getArgs)
import Network.ProofSpace (application, echoProofHandler, proofServerContext)

-- | Run a server for ProofSpace webhooks.
main :: IO ()
main =
  do
    -- Read the path to the public key PEM file, the server host, and the its port.
    [pubKeyFile, host, port] <- getArgs
    -- ProofSpace uses an SHA3-256 digest for signatures and supplies
    -- a PEM format RSA public key for verifying those signatures.
    context <- proofServerContext "sha3-256" pubKeyFile
    -- Set the host and port
    let settings = setHost (fromString host) $ setPort (read port) defaultSettings
    -- Run the server
    runSettings settings $ application context echoProofHandler
```

This server can be started with the following command:

```bash
cabal run exe:marlowe-proofspace -- proofspace/example-key.pem 0.0.0.0 4605
```

It can be tested using cURL:

```bash
curl -H 'Content-Type: application/json' \
     -H 'X-Body-Signature: inEFLb4BZS3iURKyadP3pir+4018dEhQryhVUnYhJ09DuSq2SWUVju3YQwc2fcSV0DFmxewaM5ENNlU0wbgPoBW5Q5Ppt3fMAGGOhechYmgPB9ewua1ySGYNuGX4+K8Ng01mvm0MbCdm81KxhGhDExL72OIX9QdMMLHxj5qk3XxipJsO07qeVk4irdM1S7CDZfX8tczf9i7CFXZNvI58IFyCMz5rRweSxRc8VxrDrCJkJ1dMCwikxJ9uGtqz6SKQC1mKDqK8VKOk6WLMVHBOxFpdvwPpW2v7oRnUXFbt6fkg43y3dggWCraM6y0Uca2mbFGHB1pUk3o8IeaI4zqaQdiwx5k3Vw04gJtijbUQFpG84W6hvjwPXW0cFXFRaSKRrYc9PKYlzp9TqLiMaecX+UZp+nP3PodwL2Xanz8Jnc8PBHiwoINobMYxXQJb3ZYQD+ZGSKZ7iUz0VfUibhrBMf9B68A9iqs3oFmtD1HnVLFvNwbkDGmjH7OMn5GqW3F6vjA1k8PYfXHKVjYHxoZCumVGfAttMvB/DheW1iM6I/Iy7qsAq5Ee6TQqm9cuf5d7gWn+qynueWc50gYaH/ojb4hrOkNxGXtcNrLm8yuexBiemNG7WkvAyCMmDPBqe3D/I2fQBBzL3VBJz5QPVRVPP/yyH8eiBb/dQfOcj+HJNSU=' \
     -d @proofspace/example-request.json \
     http://127.0.0.1:4605/sync-issue
```
