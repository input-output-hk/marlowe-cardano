/*eslint-env node*/
import "./static/css/main.css";
// We need to patch the JSON.stringify in order for BigInt serialization to work.
const { stringify, parse } = require("json-bigint")({ useNativeBigInt: true });

JSON.stringify = stringify;
JSON.parse = parse;

require("./output/Main").main({
  webpackDevelMode: process.env.NODE_ENV === "development",
  pollingInterval: parseInt(process.env.MARLOWE_POLLING_INTERVAL),
})();
