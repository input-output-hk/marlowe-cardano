/* global exports, require */
/* jshint -W097 */

const mermaid = require('mermaid');

"use strict";

exports.renderImpl = function(id, txt, cb, container) {
  return mermaid.render(id, txt, cb, container);
}
