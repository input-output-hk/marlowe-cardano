exports.addEventListenerWithOptions = function (type) {
  return function (listener) {
    return function (options) {
      return function (target) {
        return function () {
          return target.addEventListener(type, listener, options);
        };
      };
    };
  };
};
