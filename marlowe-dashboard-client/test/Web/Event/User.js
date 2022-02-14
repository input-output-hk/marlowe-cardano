"use strict";

// So that we have a "document"
require("global-jsdom/register");
const userEvent = require("@testing-library/user-event").default;

function adaptUser(user) {
  return Object.assign({}, user, {
    setup: function (options) {
      return adaptUser(user.setup(options));
    },
  });
}

exports._setup = (options) => adaptUser(userEvent.setup(options));
