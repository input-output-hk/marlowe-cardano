"use strict";
// Taken from https://gist.github.com/davidfurlong/463a83a33b70a3b6618e97ec9679e490
const stableReplacer = (key, value) =>
  value instanceof Object && !(value instanceof Array)
    ? Object.keys(value)
        .sort()
        .reduce((sorted, key) => {
          sorted[key] = value[key];
          return sorted;
        }, {})
    : value;

exports.stableStringifyWithIndent = (indent) => (json) =>
  JSON.stringify(json, stableReplacer, indent);
