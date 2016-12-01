/* global exports */
/* global XMLHttpRequest */
/* global module */
/* global process */
"use strict";

// module Network.HTTP.Affjax

// jshint maxparams: 5
exports._ajax = function (mkHeader, options, canceler, errback, callback) {
  var platformSpecific = { };
  if (typeof module !== "undefined" && module.require && !(typeof process !== "undefined" && process.versions["electron"])) {
    // We are on node.js
    platformSpecific.newXHR = function () {
      var XHR = module.require("xhr2");
      return new XHR();
    };

    platformSpecific.fixupUrl = function (url) {
      var urllib = module.require("url");
      var u = urllib.parse(url);
      u.protocol = u.protocol || "http:";
      u.hostname = u.hostname || "localhost";
      return urllib.format(u);
    };

    platformSpecific.getResponse = function (xhr) {
      return xhr.response;
    };
  } else {
    // We are in the browser
    platformSpecific.newXHR = function () {
      return new XMLHttpRequest();
    };

    platformSpecific.fixupUrl = function (url) {
      return url || "/";
    };

    platformSpecific.getResponse = function (xhr) {
      return xhr.response;
    };
  }

  return function () {
    var xhr = platformSpecific.newXHR();
    var fixedUrl = platformSpecific.fixupUrl(options.url);
    xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
    if (options.headers) {
      try {
        for (var i = 0, header; (header = options.headers[i]) != null; i++) {
          xhr.setRequestHeader(header.field, header.value);
        }
      }
      catch (e) {
        errback(e)();
      }
    }
    xhr.onerror = function () {
      errback(new Error("AJAX request failed: " + options.method + " " + options.url))();
    };
    xhr.onload = function () {
      callback({
        status: xhr.status,
        headers: xhr.getAllResponseHeaders().split("\r\n")
          .filter(function (header) {
            return header.length > 0;
          })
          .map(function (header) {
            var i = header.indexOf(":");
            return mkHeader(header.substring(0, i))(header.substring(i + 2));
          }),
        response: platformSpecific.getResponse(xhr)
      })();
    };
    xhr.responseType = options.responseType;
    xhr.withCredentials = options.withCredentials;
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

