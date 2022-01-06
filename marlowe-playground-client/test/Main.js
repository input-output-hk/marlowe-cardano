var JSONbig = require("json-bigint");

JSON.stringify = JSONbig.stringify;
JSON.parse = JSONbig.parse;

window = {};

exports.forDeps = function () {};
