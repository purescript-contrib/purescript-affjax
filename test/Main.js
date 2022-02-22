"use strict";

import express from "express";
import bodyParser from "body-parser";

export function logAny(a) {
  return function () {
    console.log(a);
    return {};
  };
}

export function startServer(errback, callback) {
  var app = express();

  // Always make req.body available as a String
  app.use(bodyParser.text(function() { return true; }));

  app.use(express.static(__dirname));

  app.get('/', function (req, res) {
    res.send('<html><script src="tmp/test.js"></script></html>');
  });

  app.get('/arrayview', function(req, res) {
    res.send('TODO');
  });

  app.get('/not-json', function(req, res) {
    res.header({'content-type': 'text/plain'});
    res.send('This is not JSON');
  });

  app.all('/mirror', function(req, res) {
    res.json({
      headers: req.headers,
      body: req.body,
      query: req.query,
      method: req.method,
    });
  });

  app.get('/slow', function(req, res) {
    var date = Date.now();
    var currentDate = null;
    do {
      currentDate = Date.now();
    } while (currentDate - date < 2000);
    res.header({'content-type': 'text/plain'});
    res.send('I hope I am not late!');
  });

  var server = app.listen(function () {
    callback({ port: server.address().port, server: server });
  });
  server.on('error', function (error) {
    errback(error);
  });

  return function (cancelError, onCancelerError, onCancelerSuccess) {
    onCancelerSuccess();
  };
}

export function stopServer(server) {
  return function (errback, callback) {
    server.close(function (err) {
      if (err) errback(err);
      else callback();
    });
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    };
  };
}
