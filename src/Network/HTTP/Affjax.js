/* global exports */
/* global XMLHttpRequest */
/* global module */
"use strict";

// module Network.HTTP.Affjax

// jshint maxparams: 1
exports._prepareXhr = function (options) {
  var platformSpecific = { };
  if (typeof module !== "undefined" && module.require) {
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
  } else {
    // We are in the browser
    platformSpecific.newXHR = function () {
      return new XMLHttpRequest();
    };

    platformSpecific.fixupUrl = function (url) {
      return url || "/";
    };
  }

  return function () {
    var xhr = platformSpecific.newXHR();
    xhr.responseType = options.responseType;
    xhr.withCredentials = options.withCredentials;

    return {
      xhr: xhr,
      options: options
    };
  };
};

// jshint maxparams: 4
exports._wirePXhrCallbacks = function (mkHeader, pXhr, errback, callback) {
  return function () {
    var xhr = pXhr.xhr;
    var options = pXhr.options;

    options.errback = errback;
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
  };
};

// jshint maxparams: 1
exports._sendPXhr = function (pXhr) {
  var platformSpecific = { };
  if (typeof module !== "undefined" && module.require) {
    // We are on node.js
    platformSpecific.fixupUrl = function (url) {
      var urllib = module.require("url");
      var u = urllib.parse(url);
      u.protocol = u.protocol || "http:";
      u.hostname = u.hostname || "localhost";
      return urllib.format(u);
    };
  } else {
    // We are in the browser
    platformSpecific.fixupUrl = function (url) {
      return url || "/";
    };
  }

  return function () {
    var xhr = pXhr.xhr;
    var options = pXhr.options;

    var fixedUrl = platformSpecific.fixupUrl(options.url);
    xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
    if (options.headers) {
      try {
        for (var i = 0, header; (header = options.headers[i]) != null; i++) {
          xhr.setRequestHeader(header.field, header.value);
        }
      }
      catch (e) {
        if (typeof options.errback === "function") {
          options.errback(e)();
        } else {
          throw e;
        }
      }
    }

    xhr.send(options.content);
  };
};

// jshint maxparams: 4
exports._cancelAjax = function (xhr, cancelError, errback, callback) {
  return function () {
    try { xhr.abort(); } catch (e) { return callback(false)(); }
    return callback(true)();
  };
};

