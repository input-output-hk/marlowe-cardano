# Randomized Marlowe Contracts


## Transactions

Each contract consists of ten transactions, whose attributes are given below.

1.  Creation of the contract.
2.  Choice:
    -   Party/actor: `addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd`.
    -   Name: `Amount`.
    -   Number: random integer between 1 and 2000000.
3.  Deposit:
    -   Party/actor: `addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd`.
    -   Account: `addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd`.
    -   Token: blank.
    -   Quantity: random integer between 1 and 2000000.
4.  Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Folio`.
    -   Number: random integer between 0 and 100.
5.  Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Play`.
    -   Number: random integer between 0 and 100.
6.  Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Act`.
    -   Number: random integer between 0 and 100.
7.  Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Scene`.
    -   Number: random integer between 0 and 100.
8.  Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Line`.
    -   Number: random integer between 0 and 100.
9.  Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Word`.
    -   Number: random integer between 0 and 100.
10. Choice:
    -   Party/actor: random selection of one of `c.marlowe`, `e.cary`, `f.beaumont`, `j.lumley`, `j.webster`, `m.herbert`, `w.shakespeare`.
    -   Name: `Letter`.
    -   Number: random integer between 0 and 100.


## Example data feed

The HTTP server-sent event (SSE) data feed at http://services.marlowe.run:1564 lists Marlowe transactions as they occur on the blockchain.

-   The contract above can be identified in the feed by filtering the lines for `"tags":{"5july2023":[{"revision":1}]}`.
-   Each contract has a unique `contractId` and each transaction has a unique `transactionId`.
-   The `actions` field identifies the input given in one of the ten transactions.
-   The `state` field tracks the internal state of the contract: its internal `accounts`, the history of `choices` made, and the values of `variables`.

```bash
curl -sSN http://services.marlowe.run:1564
```

```console
data:{"actions":["create"],"block":1059680,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279644,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":2000000,"token":""}]},"choices":[],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248","value":[{"amount":2000000,"token":""}]}

data:{"actions":[{"action":"choose","actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315}],"block":1059681,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279690,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":2000000,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"f545a055fcd67365ec692964cb82d51fcdf2be5090af47f0726df8e51edc47f6","value":[{"amount":2000000,"token":""}]}

data:{"actions":[{"account":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","action":"deposit","actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","amount":1023315,"token":""}],"block":1059682,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279710,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"d101cd594cc0b476285b14c43dbbdb885bc7e9660ff63c963dd04200dc8fcd84","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"m.herbert","choice":"Folio","number":17}],"block":1059684,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279730,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315},{"actor":"m.herbert","choice":"Folio","number":17}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"72364fe5deb6951986780905d4cd9f306b82d8f6e91c7d8ec093ea884f9cb46a","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"f.beaumont","choice":"Play","number":2}],"block":1059686,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279774,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315},{"actor":"m.herbert","choice":"Folio","number":17},{"actor":"f.beaumont","choice":"Play","number":2}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"2d0b4d383a195bc4a4fc3cedbd5a8fb038085e1ff0d10c017e905777f8e5ea6a","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"w.shakespeare","choice":"Act","number":18}],"block":1059687,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279796,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315},{"actor":"m.herbert","choice":"Folio","number":17},{"actor":"f.beaumont","choice":"Play","number":2},{"actor":"w.shakespeare","choice":"Act","number":18}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"2909a9e2fc6963398a3238d0c87efbf50425d62605cc0cf3c18a9f73595d9a77","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"j.webster","choice":"Scene","number":26}],"block":1059690,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279938,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315},{"actor":"m.herbert","choice":"Folio","number":17},{"actor":"f.beaumont","choice":"Play","number":2},{"actor":"w.shakespeare","choice":"Act","number":18},{"actor":"j.webster","choice":"Scene","number":26}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"88a99cf107ac29ba24784aea50f3d988b43fe8761f90ccee9b75580edb546e80","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"j.lumley","choice":"Line","number":95}],"block":1059691,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31279962,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315},{"actor":"m.herbert","choice":"Folio","number":17},{"actor":"f.beaumont","choice":"Play","number":2},{"actor":"w.shakespeare","choice":"Act","number":18},{"actor":"j.webster","choice":"Scene","number":26},{"actor":"j.lumley","choice":"Line","number":95}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"4ddde2d8e02e6523311d2de886dcc0897a96aaada3855999fa4679ec3618eeee","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"j.webster","choice":"Word","number":55}],"block":1059692,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31280012,"state":{"accounts":{"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd":[{"amount":3023315,"token":""}]},"choices":[{"actor":"addr_test1qzmph7666hj5yadja34pdlm8n5un0fhc9p0hv2g9ask7m49n3kr657fpa3q8mzwmjqvl9lqdn9g2pm3ejhlgwpprwy2srzd2xd","choice":"Amount","number":1023315},{"actor":"m.herbert","choice":"Folio","number":17},{"actor":"f.beaumont","choice":"Play","number":2},{"actor":"w.shakespeare","choice":"Act","number":18},{"actor":"j.webster","choice":"Scene","number":26},{"actor":"j.lumley","choice":"Line","number":95},{"actor":"j.webster","choice":"Word","number":55}],"variables":{}},"tags":{"5july2023":[{"revision":1}]},"transactionId":"2080054310d934c9445b2fa0038e50784927b16ab8f13418c1a714d85be2bed4","value":[{"amount":3023315,"token":""}]}

data:{"actions":[{"action":"choose","actor":"w.shakespeare","choice":"Letter","number":25}],"block":1059693,"contractId":"c94541b4b029224a58e1c8e6eb706895450996bb76edf8bc741d7494b82a4248#1","slot":31280050,"state":null,"tags":{"5july2023":[{"revision":1}]},"transactionId":"b7a7cc2f44bc21dd00c56fed941e46026132262793352aba2d6f342cd2b52a58","value":[]}
```
