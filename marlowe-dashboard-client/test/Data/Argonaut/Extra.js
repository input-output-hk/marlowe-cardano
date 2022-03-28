"use strict";

function isArray(a) {
  return Object.prototype.toString.call(a) === "[object Array]";
}

exports._compare = function _compare(EQ, GT, LT, a, b) {
  if (a == null) {
    if (b == null) return EQ;
    else return LT;
  } else if (typeof a === "boolean") {
    if (typeof b === "boolean") {
      // boolean / boolean
      if (a === b) return EQ;
      else if (a === false) return LT;
      else return GT;
    } else if (b == null) return GT;
    else return LT;
  } else if (typeof a === "number") {
    if (typeof b === "number") {
      if (a === b) return EQ;
      else if (a < b) return LT;
      else return GT;
    } else if (b == null) return GT;
    else if (typeof b === "boolean") return GT;
    else return LT;
  } else if (typeof a === "bigint") {
    if (typeof b === "bigint") {
      if (a === b) return EQ;
      else if (a < b) return LT;
      else return GT;
    } else if (b == null) return GT;
    else if (typeof b === "boolean") return GT;
    else if (typeof b === "number") return GT;
    else return LT;
  } else if (typeof a === "string") {
    if (typeof b === "string") {
      if (a === b) return EQ;
      else if (a < b) return LT;
      else return GT;
    } else if (b == null) return GT;
    else if (typeof b === "boolean") return GT;
    else if (typeof b === "number") return GT;
    else if (typeof b === "bigint") return GT;
    else return LT;
  } else if (isArray(a)) {
    if (isArray(b)) {
      for (var i = 0; i < Math.min(a.length, b.length); i++) {
        var ca = _compare(EQ, GT, LT, a[i], b[i]);
        if (ca !== EQ) return ca;
      }
      if (a.length === b.length) return EQ;
      else if (a.length < b.length) return LT;
      else return GT;
    } else if (b == null) return GT;
    else if (typeof b === "boolean") return GT;
    else if (typeof b === "number") return GT;
    else if (typeof b === "string") return GT;
    else if (typeof b === "bigint") return GT;
    else return LT;
  } else {
    if (b == null) return GT;
    else if (typeof b === "boolean") return GT;
    else if (typeof b === "number") return GT;
    else if (typeof b === "string") return GT;
    else if (typeof b === "bigint") return GT;
    else if (isArray(b)) return GT;
    else {
      var akeys = Object.keys(a);
      var bkeys = Object.keys(b);
      if (akeys.length < bkeys.length) return LT;
      else if (akeys.length > bkeys.length) return GT;
      var keys = akeys.concat(bkeys).sort();
      for (var j = 0; j < keys.length; j++) {
        var k = keys[j];
        if (a[k] === undefined) return LT;
        else if (b[k] === undefined) return GT;
        var ck = _compare(EQ, GT, LT, a[k], b[k]);
        if (ck !== EQ) return ck;
      }
      return EQ;
    }
  }
};
