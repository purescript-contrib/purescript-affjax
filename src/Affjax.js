/* global XMLHttpRequest */
/* global process */
"use strict";

exports._ajax = function () {
  var platformSpecific = { };
  if (typeof module !== "undefined" && module.require && !(typeof process !== "undefined" && process.versions["electron"])) {
    // We are on node.js
    platformSpecific.newXHR = function () {
      var XHR = module.require("xhr2");
      return new XHR();
    };

    platformSpecific.fixupUrl = function (url, xhr) {
      if (xhr.nodejsBaseUrl === null) {
        var urllib = module.require("url");
        var u = urllib.parse(url);
        u.protocol = u.protocol || "http:";
        u.hostname = u.hostname || "localhost";
        return urllib.format(u);
      } else {
        return url || "/";
      }
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

  return function (mkHeader, options) {
    return function (errback, callback) {
      var xhr = platformSpecific.newXHR();
      var fixedUrl = platformSpecific.fixupUrl(options.url, xhr);
      xhr.open(options.method || "GET", fixedUrl, true, options.username, options.password);
      if (options.headers) {
        try {
          for (var i = 0, header; (header = options.headers[i]) != null; i++) {
            xhr.setRequestHeader(header.field, header.value);
          }
        } catch (e) {
          errback(e);
        }
      }
      var onerror = function (msg) {
        return function () {
          errback(new Error(msg + ": " + options.method + " " + options.url));
        };
      };
      xhr.onerror = onerror("AJAX request failed");
      xhr.ontimeout = onerror("AJAX request timed out");
      xhr.onload = function () {
        callback({
          status: xhr.status,
          statusText: xhr.statusText,
          headers: xhr.getAllResponseHeaders().split("\r\n")
            .filter(function (header) {
              return header.length > 0;
            })
            .map(function (header) {
              var i = header.indexOf(":");
              return mkHeader(header.substring(0, i))(header.substring(i + 2));
            }),
          body: platformSpecific.getResponse(xhr)
        });
      };
      xhr.responseType = options.responseType;
      xhr.withCredentials = options.withCredentials;
      xhr.timeout = options.timeout;
      xhr.send(options.content);

      return function (error, cancelErrback, cancelCallback) {
        try {
          xhr.abort();
        } catch (e) {
          return cancelErrback(e);
        }
        return cancelCallback();
      };
    };
  };
}();
