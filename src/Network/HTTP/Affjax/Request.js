"use strict";

// module Network.HTTP.Affjax.Request

exports.newFormData = function () {
  return new FormData ();
};

exports.appendString = function (form) {
  return function (key) {
    return function (val) {
      form.append (key, val);
      return {};
    };
  };
};
