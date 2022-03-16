const { stringify, parse } = require("json-bigint")({ useNativeBigInt: true });

JSON.stringify = stringify;
JSON.parse = parse;

require("../output/Test.Main").main();
