const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const MonacoWebpackPlugin = require("monaco-editor-webpack-plugin");
const path = require("path");
const webpack = require("webpack");

module.exports = {
  context: __dirname,
  entry: [
      "big-integer", "bignumber", "blockly", "bootstrap",
      "decimal.js", "json-bigint", "monaco-editor", "monaco-emacs",
      "monaco-vim", "moo", "nearley", "safe-eval",
      // Most heavy libs: `du -hs output/* | sort -h`
      // check `dist/vendor-dll-manifest.json`
      // to get info about cached libs.
      "./output/Affjax/index.js",
      "./output/Data.Array/index.js",
      "./output/Data.Array.NonEmpty/index.js",
      "./output/Data.CodePoint.Unicode.Internal/index.js",
      "./output/Data.Either.Nested/index.js",
      "./output/Data.Functor.Product.Nested/index.js",
      "./output/Data.Functor.Variant/index.js",
      "./output/Data.Lens/index.js",
      "./output/Data.CodePoint.Unicode.Internal.Casing",
      "./output/Data.Tuple.Nested/index.js",
      "./output/Halogen/index.js",
      "./output/Halogen.HTML.Elements.Keyed/index.js",
      "./output/Halogen.Hooks/index.js",
      "./output/Prologue/index.js",
  ],
  externals: {
    "jquery": "jQuery"
  },
  module: {
    rules: [{
      test: /\.css$/,
      use: [ MiniCssExtractPlugin.loader, "css-loader", "postcss-loader" ]
    },
    {
        test: /\.ne$/,
        loader: "nearley-webpack-loader",
        options: {
            baseDir: "."
        }
    },
    { test: /\.tsx?$/,
      loader: "ts-loader"
    },
    {
        test: /\.ttf$/,
        use: ["file-loader"],
    }
    ]
  },
  output: {
    filename: "vendor.bundle.js",
    path: path.resolve(__dirname, "dist"),
    library: "vendor[fullhash]"
  },
  plugins: [
    new MonacoWebpackPlugin({
        // note that you have to include typescript if you want javascript to work!
        languages: ["javascript", "typescript"],
    }),
    new MiniCssExtractPlugin({
      filename: "[name].[contenthash].css",
    }),
    new webpack.DllPlugin({
      // We use dll plugin only during the development workflow currently
      // so we don't have to care about DCE.
      entryOnly: false,
      format: true,
      name: "vendor[fullhash]",
      path: path.resolve(__dirname, "dist/vendor-dll-manifest.json")
    }),
  ],
  resolve: {
      extensions: [".css", ".js", ".ts", ".tsx"],
      fallback: {
          vm: require.resolve("vm-browserify"),
      },
  },
};
