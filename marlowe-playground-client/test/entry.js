import "../grammar.ne";
import { stringify, parse } from "json-bigint";

JSON.stringify = stringify;
JSON.parse = parse;

require("../output/Test.Main").main();
