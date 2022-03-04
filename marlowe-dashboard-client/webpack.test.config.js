"use strict";

const nodeExternals = require("webpack-node-externals");
const path = require("path");

module.exports = {
  target: "node",
  externals: [nodeExternals()],
  optimization: {
    minimize: false,
  },
  entry: "./test/entry.js",
  output: {
    path: path.join(__dirname, "dist"),
    pathinfo: true,
    filename: "test.js",
  },
  module: {
    rules: [
      {
        test: /\.(png|svg|jpg|jpeg|gif)$/i,
        type: "asset/resource",
      },
      {
        test: /\.(woff|woff2|eot|ttf|otf)$/i,
        type: "asset/resource",
      },
    ],
  },
  resolve: {
    modules: [
      // We need the second entry for node to be able to
      // locate `node_modules` from client directory when
      // modules are referenced from inside `web-common`.
      "node_modules",
      path.resolve(__dirname, "./node_modules"),
    ],
    alias: {
      static: path.resolve(__dirname, "./static"),
      src: path.resolve(__dirname, "./src"),
    },
    extensions: [".js"],
  },
  resolveLoader: {
    modules: ["node_modules", path.resolve(__dirname, ".")],
  },
};
