
var express = require('express');
var app = express();
var bodyParser = require('body-parser');

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

var server = app.listen(3838, function () {
  var host = server.address().address;
  var port = server.address().port;

  console.log('Test server listening at http://%s:%s', host, port);
});

