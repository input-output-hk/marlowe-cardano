exports.getDappConector_ = function (just, nothing, connector) {
  if (window.cardano && connector in window.cardano) {
    return just(window.cardano[connector]);
  } else {
    return nothing;
  }
};

exports.enable_ = function (connector) {
  require("@emurgo/cardano-serialization-lib-browser").then( x => window.CardanoWasm =x);

  // This does not throw if it is not enabled, and the isEnabled seems to return true.
  return connector.enable();
};
