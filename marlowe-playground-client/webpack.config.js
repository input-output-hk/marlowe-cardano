"use strict";

const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");
const path = require("path");

const isDevelopment = process.env.NODE_ENV === "development";

class ErrorReportingPlugin {
    apply(compiler) {
        compiler.hooks.done.tap(
            "ErrorReportingPlugin",
            (stats) => process.stderr.write(stats.toString("errors-only")),
        );
    }
}

const plugins = isDevelopment ? [] : [new ErrorReportingPlugin()];

// source map adds 20Mb to the output!
const devtool = isDevelopment ? "eval-source-map" : false;

module.exports = {
    devtool,
    devServer: {
        contentBase: path.join(__dirname, "dist"),
        compress: true,
        port: 8009,
        https: true,
        proxy: {
            "/api": {
                target: "http://localhost:8080"
            },
            "/runghc": {
                target: "http://localhost:8080"
            },
        }
    },
    entry: "./entry.js",
    output: {
        path: path.join(__dirname, "dist"),
        filename: "[name].[hash].js",
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
                test: /\.woff(2)?(\?v=[0-9]\.[0-9]\.[0-9])?$/,
                loader: "url-loader",
                options: {
                    limit: 10000,
                    mimetype: "mimetype=application/font-woff",
                },
            },
            { test: /fontawesome-.*\.(ttf|eot|svg)(\?v=[0-9]\.[0-9]\.[0-9])?$/, loader: "file-loader" },
            {
                test: /\.ne$/,
                loader: "nearley-webpack-loader",
                options: {
                    baseDir: "."
                }
            },
            {
                test: /\.purs$/,
                use: [
                    {
                        loader: 'purs-loader',
                        options: {
                            bundle: !isDevelopment,
                            psc: "psa",
                            pscArgs: {
                                strict: true,
                                censorLib: true,
                                stash: isDevelopment,
                                isLib: ["generated", ".spago"],
                            },
                            spago: isDevelopment,
                            watch: isDevelopment,
                            src: isDevelopment
                                ? []
                                : [
                                    '.spago/*/*/src/**/*.purs',
                                    'src/**/*.purs',
                                    'test/**/*.purs',
                                    'generated/**/*.purs',
                                    "web-common-marlowe/src/**/*.purs",
                                    `${process.env.WEB_COMMON_PLAYGROUND_SRC}/src/**/*.purs`,
                                ],
                        }
                    }
                ]
            },
            {
                test: /\.tsx?$/,
                loader: "ts-loader"
            },
            {
                test: /\.css$/,
                use: [MiniCssExtractPlugin.loader, "css-loader", "postcss-loader"]
            },
            {
                test: /\.(gif|png|jpe?g|svg)$/i,
                use: "url-loader"
            },
            {
                test: /\.ttf$/,
                use: ["file-loader"],
            }
        ]
    },
    resolve: {
        modules: [
            // We need the second entry for node to be able to
            // locate `node_modules` from client directory when
            // modules are referenced from inside `web-common`.
            "node_modules", path.resolve(__dirname, "./node_modules")
        ],
        alias: {
            grammar: path.resolve(__dirname, "./grammar.ne"),
            static: path.resolve(__dirname, "./static"),
            src: path.resolve(__dirname, "./src")
        },
        extensions: [".purs", ".js", ".ts", ".tsx"],
        fallback: {
            vm: require.resolve("vm-browserify"),
        },
    },
    resolveLoader: {
        modules: [
            "node_modules",
            path.resolve(__dirname, ".")
        ]
    },
    plugins: [
        new HtmlWebpackPlugin({
            template: `${process.env.WEB_COMMON_SRC}/static/index.html`,
            favicon: "static/favicon.ico",
            title: "Marlowe Playground",
            productName: "marlowe-playground",
            googleAnalyticsId: isDevelopment ? "UA-XXXXXXXXX-X" : "G-G06CGG33D4",
            segmentAnalyticsId: isDevelopment ? "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX" : "RMh20hw83CbQY1CXanru5hnwkFWZOzL0",
        }),
        new MonacoWebpackPlugin({
            // note that you have to include typescript if you want javascript to work!
            languages: ["javascript", "typescript"],
        }),
        new MiniCssExtractPlugin({
            filename: "[name].[contenthash].css",
        }),

    ].concat(plugins)
};
