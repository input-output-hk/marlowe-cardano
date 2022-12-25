***WORK IN PROGRESS***


To run multiple contracts for multiple wallets, modify the [preview.config](preview.config) file to point to the node and Runtime instances, and on the command line supply that along with the number of repetitions and the pairs of addresses and keys.

```bash
cabal run exe:marlowe-scaling -- preview.config 2 \
  $(cat alice.payment-0.testnet.address) alice.payment-0.skey \
$(cat bob.payment-0.testnet.address) bob.payment-0.skey
```

The output will show which transactions are submitted for which contracts, punctuated by an success/failure (`Right`/`Left`) report after each contract closes.

```console
ContractId = 47e765c433b74e6793b040e2fd0fdd134bdd3496c33445bcef62a5371bac73df#1 > TxId = 47e765c433b74e6793b040e2fd0fdd134bdd3496c33445bcef62a5371bac73df
ContractId = 395419fc09b24d4a1e5c9ac9add62da3be567b03e3beab1c8eb55b73b5aca24b#1 > TxId = 395419fc09b24d4a1e5c9ac9add62da3be567b03e3beab1c8eb55b73b5aca24b
ContractId = 395419fc09b24d4a1e5c9ac9add62da3be567b03e3beab1c8eb55b73b5aca24b#1 > TxId = fd1426451c5dca09e7688ef04b8a7b67cd50b5663391199934ab715b164c9102
ContractId = 47e765c433b74e6793b040e2fd0fdd134bdd3496c33445bcef62a5371bac73df#1 > TxId = e2328b14a0741d9ced6472b5b67a364ae9a35d5a6284de5dcba76f6a8218956d
ContractId = 47e765c433b74e6793b040e2fd0fdd134bdd3496c33445bcef62a5371bac73df#1 > TxId = dd0593f526390b2494ebff46e2f773983d2558cb5c89449c6068b7ef9e91a670
ContractId = 395419fc09b24d4a1e5c9ac9add62da3be567b03e3beab1c8eb55b73b5aca24b#1 > TxId = 3733b13c6a1dd0f1b86ccce10af692e2015326b65703660590dfc0aab30d0b62
ContractId = 395419fc09b24d4a1e5c9ac9add62da3be567b03e3beab1c8eb55b73b5aca24b#1 > TxId = a40f863590bda57fed82ed0c5a51cd20169b3adc6e626098291f7764c0366fb6
ContractId = 47e765c433b74e6793b040e2fd0fdd134bdd3496c33445bcef62a5371bac73df#1 > TxId = 08989b2e8ded311c5723ce576b46b9462b3fe60db0c17d86dcd6971de489543e
Right (ContractId {unContractId = TxOutRef {txId = "47e765c433b74e6793b040e2fd0fdd134bdd3496c33445bcef62a5371bac73df", txIx = TxIx {unTxIx = 1}}})
Right (ContractId {unContractId = TxOutRef {txId = "395419fc09b24d4a1e5c9ac9add62da3be567b03e3beab1c8eb55b73b5aca24b", txIx = TxIx {unTxIx = 1}}})
ContractId = 69a67e7786aa6694f934ba10cd463e314d5fac2a133196d4f94879fef74d5c30#1 > TxId = 69a67e7786aa6694f934ba10cd463e314d5fac2a133196d4f94879fef74d5c30
ContractId = b1ad35a7f4ca3e50285edea1b76bbed72e44a8ea0eea9763088b35216d027b2e#1 > TxId = b1ad35a7f4ca3e50285edea1b76bbed72e44a8ea0eea9763088b35216d027b2e
ContractId = 69a67e7786aa6694f934ba10cd463e314d5fac2a133196d4f94879fef74d5c30#1 > TxId = 8fc6d67baf19f7bef397110870727904d6910e04f125b6730f24f123f6b02889
ContractId = b1ad35a7f4ca3e50285edea1b76bbed72e44a8ea0eea9763088b35216d027b2e#1 > TxId = 7e7a30cb85ab65cc9a1e2ddd1419b293b1f7c8e0cb7fd1a015845375f4662cc3
ContractId = 69a67e7786aa6694f934ba10cd463e314d5fac2a133196d4f94879fef74d5c30#1 > TxId = f938626e858864feb8b66626eee14b8eca548c909bf23bcba1f5697af7f9693d
ContractId = b1ad35a7f4ca3e50285edea1b76bbed72e44a8ea0eea9763088b35216d027b2e#1 > TxId = 4097d40d897138b502adb5e86eb6f8524bd0801ede9198d47ae32921a1f7b23a
ContractId = b1ad35a7f4ca3e50285edea1b76bbed72e44a8ea0eea9763088b35216d027b2e#1 > TxId = 4e261f3bcc0fbd2764d73036a4e8da02f3bf0f24feb4a8c97a91415289cd207b
ContractId = 69a67e7786aa6694f934ba10cd463e314d5fac2a133196d4f94879fef74d5c30#1 > TxId = c66384627ed0e2bfacc109b63c5c4e0133b3513eb03c4a5b9c2042a0df55fb6a
Right (ContractId {unContractId = TxOutRef {txId = "69a67e7786aa6694f934ba10cd463e314d5fac2a133196d4f94879fef74d5c30", txIx = TxIx {unTxIx = 1}}})
Right (ContractId {unContractId = TxOutRef {txId = "b1ad35a7f4ca3e50285edea1b76bbed72e44a8ea0eea9763088b35216d027b2e", txIx = TxIx {unTxIx = 1}}})
```
