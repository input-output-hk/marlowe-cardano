"use strict";

const jsonDiff = require("json-diff");

exports.jsonDiffString = (jsonA) => (jsonB) => {
  return jsonDiff.diffString(jsonA, jsonB);
};
