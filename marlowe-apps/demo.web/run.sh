#!/usr/bin/env nix-shell
#!nix-shell -i bash -p nodejs

set -ve

npm install

export DEBUG='express:*'
npx webpack-dev-server
