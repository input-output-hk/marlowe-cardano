exports.fromHexString_ = function (hexString) {
  return new Uint8Array(hexString.match(/.{1,2}/g).map(byte => parseInt(byte, 16)));
}

exports.loadCardanoWasm_ = function () {
  return require("@emurgo/cardano-serialization-lib-browser");
}

exports.bigNumFromStr_ = function (CardanoWasm, str) {
  return CardanoWasm.BigNum.from_str(str);
}

exports.bigNumToStr_ = function (bign) {
  return bign.to_str();
}

exports.freeObject_ = function (obj) {
  console.log('freeing', obj.ptr)
  obj.free();
}

exports.newValue_ = function (wasm, coin) {
  return wasm.Value.new(coin);
}

exports.coin_ = function (value) {
  return value.coin();
}

