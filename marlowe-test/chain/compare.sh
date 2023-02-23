#!/usr/bin/env bash

set -e

TIMESTAMP="$(date -u +%Y%m%d%H%M)"
COMMIT=$(git rev-parse HEAD)

echo "PGHOST=$PGHOST"
echo "PGPORT=$PGPORT"
echo "PGUSER=$PGUSER"
echo "PGDATABASE=$PGDATABASE"
echo "TIMESTAMP=$TIMESTAMP"
echo "COMMIT=$COMMIT"

mkdir -p out

psql -q -f compare.sql -o compare.log

tar cvzf "out/$TIMESTAMP-$PGDATABASE.tar.gz" compare.log out/*.csv

tac compare.log \
| sed -n \
    -e 's/ t/FAIL/' \
    -e 's/ f/PASS/' \
    -e 's/.*/<tr><td>'"$TIMESTAMP"'<\/td><td><a href="https:\/\/github.com\/input-output-hk\/marlowe-cardano\/commit\/'"$COMMIT"'">'"$COMMIT"'<\/a><\/td><td>'"$PGDATABASE"'<\/td><td><a href="'"$TIMESTAMP-$PGDATABASE.tar.gz"'">'"$TIMESTAMP-$PGDATABASE.tar.gz"'<\/a><\/td><td>&<\/td><\/tr>/;3p' \
> "out/$TIMESTAMP-$PGDATABASE.html"

cat << EOI > out/index.html
<html
<head>
<title>Marlowe Chain Index vs Cardano DB Sync</title>
</head>
<body>
<table border="1">
<thead>
<tr><th>Timestamp</th><th>Commit</th><th>Database</th><th>Log</th><th>Result</th></tr>
</thead>
<tbody>
EOI
cat out/2*.html >> out/index.html
cat << EOI >> out/index.html
</tbody>
</table>
</body>
</html>
EOI
