exports.getNamiAPI_ = function (just, nothing) {
  // Currently the CIP-0030 states that under the cardano object the name of the wallet should expose it's
  // API. But Nami just exposes itself under the main cardano object.
  // Eventually the Nami API should be under window.cardano.Nami or similar.
  if (window.cardano) {
    return just(window.cardano);
  } else {
    return nothing;
  }
};

exports.enable_ = function (cardano) {
  window.testcbor = require("@emurgo/cardano-serialization-lib-browser");
  return cardano.enable().catch(function (err) {
    // For some reason, even if the enable function should return a promise to a boolean, instead
    // of returning false it throws an error object. The one with code -3 means it was denied access.
    // The CIP-0030 specification expects enable to return a promise to the API. For the moment we
    // stick with the boolean, but convert -3 error to false.
    if (typeof err == "object" && "code" in err && err.code == -3) {
      return Promise.resolve(false);
    } else {
      return Promise.reject(err);
    }
  });
};
