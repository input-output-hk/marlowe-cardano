"use strict";

const userEvent = require("@testing-library/user-event");

exports._setup = function (options) {
  return function () {
    const user = userEvent.setup(options);
    return Object.assign({}, user, {
      setup: function (options) {
        return function () {
          return user.setup(options);
        };
      },
    });
  };
};
