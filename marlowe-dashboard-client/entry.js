/*eslint-env node*/
import './static/css/main.css';
// We need to patch the JSON.stringify in order for BigInt serialization to work.
var JSONbig = require("json-bigint");

JSON.stringify = JSONbig.stringify;
JSON.parse = JSONbig.parse;

require('./src/Main.purs').main();
