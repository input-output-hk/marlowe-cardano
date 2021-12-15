exports.unsafeAppend = function(suffix) {
  return function(arr) {
    arr.push(...suffix);
    return arr;
  }
}

exports.unsafeCons = function(a) {
  return function(arr) {
    arr.unshift(a);
    return arr;
  };
};

exports.unsafePrepend = function(prefix) {
  return function(arr) {
    arr.unshift(...prefix);
    return arr;
  }
}
exports.unsafeSnoc = function(a) {
  return function(arr) {
    arr.push(a);
    return arr;
  };
};

