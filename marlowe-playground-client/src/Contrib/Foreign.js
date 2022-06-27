exports.foreignDecodeJsonImpl = function (possiblyJson) {
  if (JSON.stringify(possiblyJson) === undefined) {
    return null;
  }
  return possiblyJson;
};
