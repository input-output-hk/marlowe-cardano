"use strict";

const { screen } = require("@testing-library/dom");

exports._logTestingPlaygroundURL = (e) => screen.logTestingPlaygroundURL(e);
exports._debugElement = (e) => screen.debug(e);
exports._debugElements = (es) => screen.debug(es);
