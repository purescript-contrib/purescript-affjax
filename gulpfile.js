/* jshint node: true */
"use strict";

var gulp = require("gulp");
var jshint = require("gulp-jshint");
var jscs = require("gulp-jscs");
var purescript = require("gulp-purescript");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs",
  "test/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js",
  "test/**/*.js"
];

gulp.task("lint", function() {
  return gulp.src("src/**/*.js")
    .pipe(jshint())
    .pipe(jshint.reporter())
    .pipe(jscs());
});

gulp.task("make", ["lint"], function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("docs", function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      "Network.HTTP.Affjax": "docs/Network.HTTP.Affjax.md",
      "Network.HTTP.Affjax.Request": "docs/Network.HTTP.Affjax.Request.md",
      "Network.HTTP.Affjax.Response": "docs/Network.HTTP.Affjax.Response.md",
      "Network.HTTP.Method": "docs/Network.HTTP.Method.md",
      "Network.HTTP.MimeType": "docs/Network.HTTP.MimeType.md",
      "Network.HTTP.MimeType.Common": "docs/Network.HTTP.MimeType.Common.md",
      "Network.HTTP.RequestHeader": "docs/Network.HTTP.RequestHeader.md",
      "Network.HTTP.ResponseHeader": "docs/Network.HTTP.ResponseHeader.md",
      "Network.HTTP.StatusCode": "docs/Network.HTTP.StatusCode.md"
    }
  });
});

gulp.task("bundle", ["make"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "tmp/test.js",
    main: "Test.Main"
  });
});

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task("default", ["bundle", "docs", "dotpsci"]);
