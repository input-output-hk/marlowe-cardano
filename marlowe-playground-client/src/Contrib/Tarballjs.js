var { TarReader, TarWriter } = require("tarballjs");

exports.tarWriter = function () {
  return new TarWriter();
};

exports.tarReader = function () {
  return new TarReader();
};
