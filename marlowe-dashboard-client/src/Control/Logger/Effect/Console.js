"use strict";

exports.debugImpl = (msg) => console.debug(msg);

exports.structuredDebugImpl = (msg, payload) => console.debug(msg, payload);

exports.structuredInfoImpl = (msg, payload) => console.info(msg, payload);

exports.structuredLogImpl = (msg, payload) => console.log(msg, payload);

exports.structuredWarnImpl = (msg, payload) => console.warn(msg, payload);

exports.structuredErrorImpl = (msg, payload) => console.error(msg, payload);
