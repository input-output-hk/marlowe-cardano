# Benchmarks for Marlowe Runtime

The `marlowe-benchmark` tool runs a series of benchmarks against Marlowe Runtime services.


## Usage

```bash
marlowe-benchmark --help
```

```console
marlowe-benchmark : execute Marlowe Runtime benchmarks

Usage: marlowe-benchmark [--version] [--host HOST] [--port PORT] [--config FILE]
                         [--node-socket-path FILE --network-magic INTEGER
                           --address ADDRESS --signing-key-file FILE]
                         [--out-file FILE]

  This command-line tool executes benchmarks for Marlowe Runtime.

Available options:
  -h,--help                Show this help text
  --version                Show version
  --host HOST              Host for Marlowe proxy service.
  --port PORT              Port for Marlowe proxy service.
  --babbage-era            Read and write Babbage transactions
  --conway-era             Read and write Conway transactions
  --config FILE            Path to the benchmark configuration file.
  --node-socket-path FILE  Path to the Cardano node socket.
  --network-magic INTEGER  The Cardano network magic number.
  --address ADDRESS        Faucet address.
  --signing-key-file FILE  Path to faucet signing key file.
  --out-file FILE          Path to the output file for benchmark results.
```


## Configuration

The optional JSON configuration for benchmarking specifies the number clients run in parallel during benchmarking and the quantity of data processed.

| JSON Key                | Haskell Type                                     | Description                                                          |
|-------------------------|--------------------------------------------------|----------------------------------------------------------------------|
| `headerSyncParallelism` | `Int`                                            | Number of parallel clients for `HeaderSync` protocol.                |
| `headerMaxContracts`    | `Int`                                            | Maximum number of contracts to be read by each `HeaderSync` client.  |
| `bulkParallelism`       | `Int`                                            | Number of parallel clients for the `BulkSync` protocol.              |
| `bulkPageSize`          | `Word8`                                          | Number of blocks to fetch at a time for the `BulkSync` clients.      |
| `bulkMaxBlocks`         | `Int`                                            | Maximum number of blocks to fetch for each `BulkSync` client.        |
| `syncParallelism`       | `Int`                                            | Number of parallel clients for `Sync` protocol.                      |
| `syncBatchSize`         | `Int`                                            | Number of contracts to be read by each `Sync` client.                |
| `queryParallelism`      | `Int`                                            | Number of parallel clients for `Query` protocol.                     |
| `queryBatchSize`        | `Int`                                            | Number of queries to be executed by each `Query` client.             |
| `queryPageSize`         | `Int`                                            | Page size for each `Query` client.                                   |
| `complexQueries`        | `Language.Marlowe.Runtime.Benchmark.Query.Query` | Query filters to execute in addition to query for contract headers.  |
| `lifecycleParallelism`  | `Int`                                            | Number of parallel clients for running the basic contract lifecycle. |
| `lifecycleContracts`    | `Int`                                            | Number of contracts to run for each basic lifecycle client.          |

A complete example is [example-config.yaml](example-config.yaml), with results in [example-results.json](example-results.json), but here is a simple example configuration file:

```json
{
  "headerSyncParallelism" : 4
, "headerMaxContracts" : 100000
, "bulkParallelism" : 4
, "bulkPageSize" : 128
, "bulkMaxBlocks" : 100000
, "syncParallelism" : 4
, "syncBatchSize" : 512
, "queryParallelism" : 4
, "queryBatchSize" : 16
, "queryPageSize" : 256
, "complexQueries" : {}
, "lifecycleParallelism" : 4
, "lifecycleContracts" : 3
}
```


## Example output

The output is lines of JSON, with one report per benchmarking client.

```bash
marlowe-benchmark \
  --node-socket-path node.socket \
  --network-magic 1 \
  --babbage-era
  --address addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
  --signing-key-file faucet.skey \
```

Standard error:
```console
HeaderSync: 2024-01-10 13:50:00.45517781 UTC
BulkSync: 2024-01-10 13:59:46.536134044 UTC
Sync: 2024-01-10 14:00:27.02131234 UTC
Query: 2024-01-10 14:05:43.937223708 UTC
Lifecycle: 2024-01-10 14:06:30.826018702 UTC
Generated address "60e4cdc56a9d0831b20d0c2c3a750e6790c631f262b73bc1640ebd0f1b" and signing key "c8e9b3614d6c51a8bea6e3ec228ffed06752281ae1319028999a3be8f837cc48f54e3189422df70fb38198d01b0ddf962fd5a78615bd70cde3b5bb70dcb6e05ba8967240a62860fdaa5216ef1f9bba671ef89b1ae1a65ca964a6bb6dad91715bada824628e20362f2b5e6193e5330a9b70beec007bafb56147847425430b67e8".
Generated address "604c9b1703effb1f910e9a963c31cca0be8734cf530657605d0ffb60f4" and signing key "58cb0859a0a3f63207f6fe90aaf9218bfe077f9392da48b0bbc133fac5405d45c7506329e4018e621a7a1a55fe86403c0a612cf2c4e991237da610bb1534136b8553f97b8be4309ad7bb6a1c414944c1e2161de2fc7eba5f76292bfa62b6da5f41e0462588da061b1492e1e87b9ed5f974aaa390e959e0c73527d8517283ab86".
Generated address "60e2f333884ea02ebb232520e60cfa08ecb730d5000d832ff1d7831016" and signing key "480f94a9249020d05196aea630c4654f1f5aa3ac408078b1fa49e01c6047a65278b043b1fadde922a4c9b2ca90380fe1468e54d347528f424a17ac948a6442df7d2b328c57e6b966f30c9251f73a4db9188ee73c70c37c29948582c3312957f0f76d2ec65978c3dc7066f721d6c513d518f0d19810f7fcdae3127177a7bb32d2".
Generated address "6016c4731041be85e0a4e6b1dafb9813df4d2955ec2939303e1a31e7a2" and signing key "e0cc808a6d5be1335a8c2f8d362817db392d4ddc39ec71d60e008f7daaeb0a4e58b78e63b7406650eea2b480b0e2c6d01024ceb7fdcb4bf1be17adf6a2772e0090ba5b9aa22bc203f740054d788f2f0d32f099ceac117b400e9bba1e694641fe70847714766e6bdabc87ad51f9868ab4cc0137034bf08614b1e272dc0f2aa2e0".
Done: 2024-01-10 14:14:18.948436122 UTC
```

Standard output:
```console
{"blocksPerSecond":11.791839608045978,"contractsPerSecond":15.236531607037984,"finish":"2024-01-10T13:59:46.367342485Z","metric":"HeaderSync","seconds":585.828863826,"start":"2024-01-10T13:50:00.455652507Z"}
{"blocksPerSecond":11.791841157294378,"contractsPerSecond":15.236533608860686,"finish":"2024-01-10T13:59:46.36745751Z","metric":"HeaderSync","seconds":585.828786858,"start":"2024-01-10T13:50:00.455694078Z"}
{"blocksPerSecond":11.79017373801482,"contractsPerSecond":15.2343790946034,"finish":"2024-01-10T13:59:46.450406374Z","metric":"HeaderSync","seconds":585.911637394,"start":"2024-01-10T13:50:00.455891333Z"}
{"blocksPerSecond":11.788494116433267,"contractsPerSecond":15.232208813445764,"finish":"2024-01-10T13:59:46.534404562Z","metric":"HeaderSync","seconds":585.995117932,"start":"2024-01-10T13:50:00.456081973Z"}
{"applyInputsPerSecond":899.582293953998,"blocksPerSecond":936.4149875690317,"createsPerSecond":174.65825681967607,"finish":"2024-01-10T14:00:26.179264057Z","metric":"BulkSync","seconds":39.557248113,"start":"2024-01-10T13:59:46.536224739Z","withdrawsPerSecond":19.97105556340195}
{"applyInputsPerSecond":881.9168757448351,"blocksPerSecond":918.0262726244255,"createsPerSecond":171.22843036450936,"finish":"2024-01-10T14:00:26.970796185Z","metric":"BulkSync","seconds":40.349607745,"start":"2024-01-10T13:59:46.536225293Z","withdrawsPerSecond":19.578876825584366}
{"applyInputsPerSecond":884.1995163993927,"blocksPerSecond":920.4023742157174,"createsPerSecond":171.6716160967656,"finish":"2024-01-10T14:00:26.864651153Z","metric":"BulkSync","seconds":40.2454416,"start":"2024-01-10T13:59:46.536320372Z","withdrawsPerSecond":19.629552282015464}
{"applyInputsPerSecond":912.3805697657663,"blocksPerSecond":949.7372787765495,"createsPerSecond":177.14310401887533,"finish":"2024-01-10T14:00:25.622237591Z","metric":"BulkSync","seconds":39.002364999,"start":"2024-01-10T13:59:46.536344013Z","withdrawsPerSecond":20.255181961920904}
{"contractsPerSecond":1.6409511394298195,"finish":"2024-01-10T14:05:39.120769259Z","metric":"Sync","seconds":312.014165259,"start":"2024-01-10T14:00:27.021471889Z","stepsPerSecond":6.634314176991653}
{"contractsPerSecond":1.6160266628768356,"finish":"2024-01-10T14:05:43.934816319Z","metric":"Sync","seconds":316.826455752,"start":"2024-01-10T14:00:27.021549011Z","stepsPerSecond":6.65032847398729}
{"contractsPerSecond":1.635280953200488,"finish":"2024-01-10T14:05:40.203826791Z","metric":"Sync","seconds":313.096045666,"start":"2024-01-10T14:00:27.021667443Z","stepsPerSecond":6.624165423706664}
{"contractsPerSecond":1.6198163550418652,"finish":"2024-01-10T14:05:43.191830477Z","metric":"Sync","seconds":316.085214479,"start":"2024-01-10T14:00:27.021713444Z","stepsPerSecond":6.637450611089518}
{"contractsPerSecond":3067.644284694712,"finish":"2024-01-10T14:06:30.498424679Z","metric":"Query","pagesPerSecond":12.027282397705267,"queriesPerSecond":0.34363663993443616,"query":"No policy ID","seconds":46.560809124,"start":"2024-01-10T14:05:43.937577585Z"}
{"contractsPerSecond":3049.98390498753,"finish":"2024-01-10T14:06:30.768214107Z","metric":"Query","pagesPerSecond":11.95804152285914,"queriesPerSecond":0.34165832922454686,"query":"No policy ID","seconds":46.830411061,"start":"2024-01-10T14:05:43.937686914Z"}
{"contractsPerSecond":3068.8016848872867,"finish":"2024-01-10T14:06:30.481140303Z","metric":"Query","pagesPerSecond":12.031820205114265,"queriesPerSecond":0.3437662915746933,"query":"No policy ID","seconds":46.54324869,"start":"2024-01-10T14:05:43.937789942Z"}
{"contractsPerSecond":3054.2961134864795,"finish":"2024-01-10T14:06:30.702288862Z","metric":"Query","pagesPerSecond":11.974948355777617,"queriesPerSecond":0.3421413815936462,"query":"No policy ID","seconds":46.764293537,"start":"2024-01-10T14:05:43.937935376Z"}
{"appliesPerSecond":3.42017117814057e-2,"contractsPerSecond":1.14005705938019e-2,"creationsPerSecond":1.14005705938019e-2,"finish":"2024-01-10T14:11:42.759844755Z","metric":"Lifecycle","seconds":263.144723794,"start":"2024-01-10T14:07:19.615120909Z","withdrawsPerSecond":0}
{"appliesPerSecond":3.423749807159318e-2,"contractsPerSecond":1.1412499357197726e-2,"creationsPerSecond":1.1412499357197726e-2,"finish":"2024-01-10T14:11:42.485005027Z","metric":"Lifecycle","seconds":262.869675266,"start":"2024-01-10T14:07:19.615329722Z","withdrawsPerSecond":0}
{"appliesPerSecond":3.4143452549104655e-2,"contractsPerSecond":1.1381150849701552e-2,"creationsPerSecond":1.1381150849701552e-2,"finish":"2024-01-10T14:11:43.209197201Z","metric":"Lifecycle","seconds":263.593729634,"start":"2024-01-10T14:07:19.61546749Z","withdrawsPerSecond":0}
{"appliesPerSecond":3.4165137467845705e-2,"contractsPerSecond":1.1388379155948569e-2,"creationsPerSecond":1.1388379155948569e-2,"finish":"2024-01-10T14:11:43.042021307Z","metric":"Lifecycle","seconds":263.426424333,"start":"2024-01-10T14:07:19.615597279Z","withdrawsPerSecond":0}
```

Tools like `jq` and `dasel` can convert the output to CSV files.

```bash
marlowe-benchmark \
  --node-socket-path node.socket \
  --network-magic 1 \
  --babbage-era
  --address addr_test1vq9prvx8ufwutkwxx9cmmuuajaqmjqwujqlp9d8pvg6gupczgtm9j \
  --signing-key-file faucet.skey \
  --out-file results.json
for x in Sync BulkSync HeaderSync Query Lifecycle
do
  echo
  jq -s 'map(select(.metric == "'$x'"))' results.json \
  | dasel -r json -w csv --csv-comma $'\t'
done
```

| contractsPerSecond | finish | metric | seconds | start | stepsPerSecond |
|-------------------:|-------:|-------:|--------:|------:|---------------:|
| 1.640951 | 2024-01-10T14:05:39.120769259Z | Sync | 312.014165 | 2024-01-10T14:00:27.021471889Z | 6.634314 |
| 1.616027 | 2024-01-10T14:05:43.934816319Z | Sync | 316.826456 | 2024-01-10T14:00:27.021549011Z | 6.650328 |
| 1.635281 | 2024-01-10T14:05:40.203826791Z | Sync | 313.096046 | 2024-01-10T14:00:27.021667443Z | 6.624165 |
| 1.619816 | 2024-01-10T14:05:43.191830477Z | Sync | 316.085214 | 2024-01-10T14:00:27.021713444Z | 6.637451 |

| applyInputsPerSecond | blocksPerSecond | createsPerSecond | finish | metric | seconds | start | withdrawsPerSecond |
|---------------------:|----------------:|-----------------:|-------:|-------:|--------:|------:|-------------------:|
| 899.582294 | 936.414988 | 174.658257 | 2024-01-10T14:00:26.179264057Z | BulkSync | 39.557248 | 2024-01-10T13:59:46.536224739Z | 19.971056 |
| 881.916876 | 918.026273 | 171.228430 | 2024-01-10T14:00:26.970796185Z | BulkSync | 40.349608 | 2024-01-10T13:59:46.536225293Z | 19.578877 |
| 884.199516 | 920.402374 | 171.671616 | 2024-01-10T14:00:26.864651153Z | BulkSync | 40.245442 | 2024-01-10T13:59:46.536320372Z | 19.629552 |
| 912.380570 | 949.737279 | 177.143104 | 2024-01-10T14:00:25.622237591Z | BulkSync | 39.002365 | 2024-01-10T13:59:46.536344013Z | 20.255182 |

| blocksPerSecond | contractsPerSecond | finish | metric | seconds | start |
|----------------:|-------------------:|-------:|-------:|--------:|------:|
| 11.791840 | 15.236532 | 2024-01-10T13:59:46.367342485Z | HeaderSync | 585.828864 | 2024-01-10T13:50:00.455652507Z |
| 11.791841 | 15.236534 | 2024-01-10T13:59:46.36745751Z | HeaderSync | 585.828787 | 2024-01-10T13:50:00.455694078Z |
| 11.790174 | 15.234379 | 2024-01-10T13:59:46.450406374Z | HeaderSync | 585.911637 | 2024-01-10T13:50:00.455891333Z |
| 11.788494 | 15.232209 | 2024-01-10T13:59:46.534404562Z | HeaderSync | 585.995118 | 2024-01-10T13:50:00.456081973Z |

| contractsPerSecond | finish | metric | pagesPerSecond | queriesPerSecond | query | seconds | start |
|-------------------:|-------:|-------:|---------------:|-----------------:|------:|--------:|------:|
| 3067.644285 | 2024-01-10T14:06:30.498424679Z | Query | 12.027282 | 0.343637 | No policy ID | 46.560809 | 2024-01-10T14:05:43.937577585Z |
| 3049.983905 | 2024-01-10T14:06:30.768214107Z | Query | 11.958042 | 0.341658 | No policy ID | 46.830411 | 2024-01-10T14:05:43.937686914Z |
| 3068.801685 | 2024-01-10T14:06:30.481140303Z | Query | 12.031820 | 0.343766 | No policy ID | 46.543249 | 2024-01-10T14:05:43.937789942Z |
| 3054.296113 | 2024-01-10T14:06:30.702288862Z | Query | 11.974948 | 0.342141 | No policy ID | 46.764294 | 2024-01-10T14:05:43.937935376Z |

| appliesPerSecond | contractsPerSecond | creationsPerSecond | finish | metric | seconds | start | withdrawsPerSecond |
|-----------------:|-------------------:|-------------------:|-------:|-------:|--------:|------:|-------------------:|
| 0.034202 | 0.011401 | 0.011401 | 2024-01-10T14:11:42.759844755Z | Lifecycle | 263.144724 | 2024-01-10T14:07:19.615120909Z | 0 |
| 0.034237 | 0.011412 | 0.011412 | 2024-01-10T14:11:42.485005027Z | Lifecycle | 262.869675 | 2024-01-10T14:07:19.615329722Z | 0 |
| 0.034143 | 0.011381 | 0.011381 | 2024-01-10T14:11:43.209197201Z | Lifecycle | 263.593730 | 2024-01-10T14:07:19.61546749Z | 0 |
| 0.034165 | 0.011388 | 0.011388 | 2024-01-10T14:11:43.042021307Z | Lifecycle | 263.426424 | 2024-01-10T14:07:19.615597279Z | 0 |
