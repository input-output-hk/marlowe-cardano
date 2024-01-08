# Benchmarks for Marlowe Runtime

The `marlowe-benchmark` tool runs a series of benchmarks against Marlowe Runtime services.


## Usage

```bash
marlowe-benchmark --help
```

```console
marlowe-benchmark : execute Marlowe Runtime benchmarks

Usage: marlowe-benchmark [--version] [--host HOST] [--port PORT] [--config FILE]

  This command-line tool for executing benchmarks for Marlowe Runtime.

Available options:
  -h,--help                Show this help text
  --version                Show version
  --host HOST              Host for Marlowe proxy service.
  --port PORT              Port for Marlowe proxy service.
  --config FILE            Name of benchmark configuration file.
```


## Configuration

The optional JSON configuration for benchmarking specifies the number clients run in parallel during benchmarking and the quantity of data processed.

| JSON Key                | Haskell Type | Description                                                         |
|-------------------------|--------------|---------------------------------------------------------------------|
| `headerSyncParallelism` | `Int`        | Number of parallel clients for `HeaderSync` protocol.               |
| `headerMaxContracts`    | `Int`        | Maximum number of contracts to be read by each `HeaderSync` client. |
| `bulkParallelism`       | `Int`        | Number of parallel clients for the `BulkSync` protocol.             |
| `bulkPageSize`          | `Word8`      | Number of blocks to fetch at a time for the `BulkSync` clients.     |
| `bulkMaxBlocks`         | `Int`        | Maximum number of blocks to fetch for each `BulkSync` client.       |
| `syncParallelism`       | `Int`        | Number of parallel clients for `Sync` protocol.                     |
| `syncBatchSize`         | `Int`        | Number of contracts to be read by each `Sync` client.               |
| `queryParallelism`      | `Int`        | Number of parallel clients for `Query` protocol.                    |
| `queryBatchSize`        | `Int`        | Number of queries to be executed by each `Query` client.            |
| `queryPageSize`         | `Int`        | Page size for each `Query` client.                                  |



## Example output

The output is lines of JSON, with one report per benchmarking client.

```bash
marlowe-benchmark
```

```console
{"blocksPerSecond":11.771785516655209,"contractsPerSecond":15.269010744861106,"metric":"HeaderSync","seconds":574.169482653}
{"blocksPerSecond":11.770062718147265,"contractsPerSecond":15.266776128125029,"metric":"HeaderSync","seconds":574.253524544}
{"blocksPerSecond":11.771827242209914,"contractsPerSecond":15.269064866467572,"metric":"HeaderSync","seconds":574.167447494}
{"blocksPerSecond":11.771792207230698,"contractsPerSecond":15.269019423108674,"metric":"HeaderSync","seconds":574.16915632}
{"applyInputsPerSecond":942.8365324905556,"blocksPerSecond":978.2612577741164,"createsPerSecond":180.32465581089704,"metric":"BulkSync","seconds":37.487940679,"withdrawsPerSecond":20.806691055103503}
{"applyInputsPerSecond":945.7512794506212,"blocksPerSecond":981.2855190633082,"createsPerSecond":180.88212332964207,"metric":"BulkSync","seconds":37.372405164,"withdrawsPerSecond":20.871014230343317}
{"applyInputsPerSecond":927.177585961421,"blocksPerSecond":962.0139654820538,"createsPerSecond":177.3297632224984,"metric":"BulkSync","seconds":38.121068213,"withdrawsPerSecond":20.461126525672892}
{"applyInputsPerSecond":949.2986468390368,"blocksPerSecond":984.9661699116706,"createsPerSecond":181.5605843155153,"metric":"BulkSync","seconds":37.23275085,"withdrawsPerSecond":20.949298190251767}
{"contractsPerSecond":1.6611365871553612,"metric":"Sync","seconds":308.222697615,"stepsPerSecond":6.787300274080108}
{"contractsPerSecond":1.6320142779350748,"metric":"Sync","seconds":313.722745519,"stepsPerSecond":6.830872260966534}
{"contractsPerSecond":1.6615895324437109,"metric":"Sync","seconds":308.138676853,"stepsPerSecond":6.759943351719238}
{"contractsPerSecond":1.6544344963387472,"metric":"Sync","seconds":309.471303417,"stepsPerSecond":6.750221997757115}
{"contractsPerSecond":3072.685366781302,"metric":"Query","pagesPerSecond":12.266908616099643,"queriesPerSecond":0.35048310331713267,"query":"No policy ID","seconds":45.651273481}
{"contractsPerSecond":3083.3628083568774,"metric":"Query","pagesPerSecond":12.309535564331096,"queriesPerSecond":0.3517010161237456,"query":"No policy ID","seconds":45.493186731}
{"contractsPerSecond":3066.656834056088,"metric":"Query","pagesPerSecond":12.242841244663293,"queriesPerSecond":0.3497954641332369,"query":"No policy ID","seconds":45.741016224}
{"contractsPerSecond":3087.258954561369,"metric":"Query","pagesPerSecond":12.325089929240097,"queriesPerSecond":0.35214542654971703,"query":"No policy ID","seconds":45.435773955}
```

Tools like `jq` and `dasel` can convert the output to CSV files.

```bash
marlowe-benchmark > results.json
for x in Sync BulkSync HeaderSync Query
do
  echo
  jq -s 'map(select(.metric == "'$x'"))' result.json \
  | dasel -r json -w csv --csv-comma $'\t'
done
```

```console
contractsPerSecond      metric  seconds stepsPerSecond
1.661137        Sync    308.222698      6.787300
1.632014        Sync    313.722746      6.830872
1.661590        Sync    308.138677      6.759943
1.654434        Sync    309.471303      6.750222

applyInputsPerSecond    blocksPerSecond createsPerSecond        metric  seconds withdrawsPerSecond
942.836532      978.261258      180.324656      BulkSync        37.487941       20.806691
945.751279      981.285519      180.882123      BulkSync        37.372405       20.871014
927.177586      962.013965      177.329763      BulkSync        38.121068       20.461127
949.298647      984.966170      181.560584      BulkSync        37.232751       20.949298

blocksPerSecond contractsPerSecond      metric  seconds
11.771786       15.269011       HeaderSync      574.169483
11.770063       15.266776       HeaderSync      574.253525
11.771827       15.269065       HeaderSync      574.167447
11.771792       15.269019       HeaderSync      574.169156

contractsPerSecond      metric  pagesPerSecond  queriesPerSecond        query   seconds
3072.685367     Query   12.266909       0.350483        No policy ID    45.651273
3083.362808     Query   12.309536       0.351701        No policy ID    45.493187
3066.656834     Query   12.242841       0.349795        No policy ID    45.741016
3087.258955     Query   12.325090       0.352145        No policy ID    45.435774
```
