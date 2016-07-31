"use strict";

exports.logAny = function (a) {
  return function () {
    console.log(a);
    return {};
  };
};
