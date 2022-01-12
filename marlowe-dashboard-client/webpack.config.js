"use strict";

const HtmlWebpackPlugin = require("html-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const path = require("path");

const isDevelopment = process.env.NODE_ENV === "development";

const devtool = isDevelopment ? "eval-source-map" : false;

module.exports = {
  devtool,
  devServer: {
    contentBase: path.join(__dirname, "dist"),
    compress: true,
    host: "0.0.0.0",
    port: 8009,
    stats: "errors-warnings",
    proxy: {
      "/pab/api": {
        target: "http://localhost:9080",
        pathRewrite: { "^/pab": "/" },
      },
      "/pab/wallet": {
        target: "http://localhost:9080",
        pathRewrite: { "^/pab": "/" },
      },
      "/pab/ws": {
        target: "ws://localhost:9080",
        ws: true,
        pathRewrite: { "^/pab": "/" },
        onError(err) {
          console.log("Error with the WebSocket:", err);
        },
      },
      "/api": {
        target: "http://localhost:8080",
      },
    },
  },
  entry: "./entry.js",
  output: {
    filename: "[name].[contenthash].js",
    path: path.join(__dirname, "dist"),
    pathinfo: true,
    clean: true,
  },
  optimization: {
    runtimeChunk: "single",
    splitChunks: {
      cacheGroups: {
        vendor: {
          test: /[\\/]node_modules[\\/]/,
          name: "vendors",
          chunks: "all",
        },
      },
    },
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        exclude: /node_modules/,
        use: [MiniCssExtractPlugin.loader, "css-loader", "postcss-loader"],
      },
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
      "node_modules",
      // We need this entry for node to be able to locate `node_modules` from
      // client directory when modules are referenced from inside `web-common`.
      path.resolve(__dirname, "./node_modules"),
    ],
    alias: {
      contracts: path.resolve(__dirname, "./contracts"),
      static: path.resolve(__dirname, "./static"),
      src: path.resolve(__dirname, "./src"),
    },
    extensions: [".js"],
  },
  resolveLoader: {
    modules: ["node_modules", path.resolve(__dirname, ".")],
  },
  plugins: [
    new HtmlWebpackPlugin({
      template: process.env.WEB_COMMON_SRC + "/static/index.html",
      favicon: "static/favicon.ico",
      title: "Marlowe Run",
      productName: "marlowe-dashboard",
      googleAnalyticsId: isDevelopment ? "UA-XXXXXXXXX-X" : "G-4PKSS5QCMJ",
      segmentAnalyticsId: isDevelopment
        ? "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
        : "LCDRr9g7ED1yCaSCxv08QuEZ8eIEx0Ke",
    }),
    new MiniCssExtractPlugin({
      filename: "[name].[contenthash].css",
    }),
  ],
};
