#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodejs

set -e

echo 'The environment variables $CARDANO_NODE_SOCKET_PATH and $CARDANO_TESTNET_MAGIC must be set.'
echo 'The `marlowe-cli` tool must be on the $PATH.'

npm install 

npx webpack

npm start
