# A Signing Service

This service is a simple utility for signing transactions in demonstrations, where a signing tool such as `cardano-cli` or a CIP-30 wallet is not available.


## Deployment

Simply provide the bind address and port number for the service when starting it.

```bash
cabal run exe:signing-service -- 127.0.0.1 3779
```

In this example, the service endpoint is `http://127.0.0.1:3779/sign`.


## Example

For a request, the body is the unsigned transaction provided by Marlowe Runtime and the payment keys is the signing key file.

```JSON
{
  "body" : {
    "type": "TxBodyBabbage",
    "description": "",
    "cborHex": "86a3008182582013735748e79d7d049f4761cd47c3523935337f474f3d942517ea28687fe137cf000181a200581d6054705dbd6072d78dc3555ce3a77058fcaf769f9f98385fec07b1f889011a0be9415b021a000280a59fff8080f5f6"
  }
, "paymentKeys" : [
     {
         "type": "PaymentSigningKeyShelley_ed25519",
         "description": "Payment Signing Key",
         "cborHex": "5820bb921c6af621b3336460e6df335d73d4d91012718240bb95dd0301fd9aa5b5e6"
     }
  ]
, "paymentExtendedKeys" : []
}
```

Next call the endpoint for the service.

```bash
curl -sS \
  -H 'Content-Type: application/json' \
  -H 'Accept: application/json' \
  -d @request.json \
  -o response.json \
  http://127.0.0.1:3779/sign
```

The response is the tx is suitable for sending to Marlowe Runtime for submission.

```JSON
{
   "tx" : {
      "cborHex" : "84a3008182582013735748e79d7d049f4761cd47c3523935337f474f3d942517ea28687fe137cf000181a200581d6054705dbd6072d78dc3555ce3a77058fcaf769f9f98385fec07b1f889011a0be9415b021a000280a5a10081825820ae56efbd10931edd0bcc4c1f929ab9c8d30fe15a58ea4df515aa281d1479a21c58407486e300ebf2401ed5be16c9efcb30828489b68b8dea87a5d391d5a246d96284ed2311bec05d219606246d236dc613926eccac278cdd0cfa6db6d5c1e5455b0cf5f6",
      "description" : "",
      "type" : "Tx BabbageEra"
   },
   "txId" : "b21e5b0fc853ae86e33ec54b2886b589dfe3fadf08d1b28e5942c483eef16677"
}
```
