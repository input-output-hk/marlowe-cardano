// We need to patch the JSON.stringify in order for BigInt serialization to work.
const { stringify, parse } = require("json-bigint")({ useNativeBigInt: true });
JSON.stringify = stringify;
JSON.parse = parse;

require("./output/Main").main();
