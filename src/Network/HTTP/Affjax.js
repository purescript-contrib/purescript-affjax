/* jshint browser: true */
/* global exports */
"use strict";

// module Network.HTTP.Affjax

// jshint maxparams: 5
exports._ajax = function (mkHeader, options, canceler, errback, callback) {
  return function () {
    var xhr = new XMLHttpRequest();
    xhr.open(options.method || "GET", options.url || "/", true, options.username, options.password);
    if (options.headers) {
      for (var i = 0, header; (header = options.headers[i]) != null; i++) {
        xhr.setRequestHeader(header.field, header.value);
      }
    }
    xhr.onerror = function () {
      errback(new Error("AJAX request failed: " + options.method + " " + options.url))();
    };
    xhr.onload = function () {
      callback({
        status: xhr.status,
        headers: xhr.getAllResponseHeaders().split("\n")
          .filter(function (header) {
            return header.length > 0;
          })
          .map(function (header) {
            var i = header.indexOf(":");
            return mkHeader(header.substring(0, i))(header.substring(i + 2));
          }),
        response: xhr.response
      })();
    };
    xhr.responseType = options.responseType;
    xhr.send(options.content);
    return canceler(xhr);
  };
};

// jshint maxparams: 4
exports._cancelAjax = function (xhr, cancelError, errback, callback) {
  return function () {
    try { xhr.abort(); } catch (e) { return callback(false)(); }
    return callback(true)();
  };
};

