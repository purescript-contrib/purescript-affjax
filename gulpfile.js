"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var jsvalidate = require("gulp-jsvalidate");

var sources = [
  'src/**/*.purs',
  'bower_components/purescript-*/src/**/*.purs',
  'test/**/*.purs'
];

var foreigns = [
  'src/**/*.js',
  'bower_components/purescript-*/src/**/*.js',
  'test/**/*.js'
];

gulp.task('jsvalidate', function() {
  return gulp.src(foreigns)
    .pipe(plumber())
    .pipe(jsvalidate());
});

gulp.task('psc', function() {
  return purescript.psc({
    src: sources,
    ffi: foreigns
  });
});

gulp.task('pscBundle', function() {
  return purescript.pscBundle({
    src: 'output/**/*.js',
    output: 'tmp/test.js',
    main: 'Test.Main'
  });
});

gulp.task('pscDocs', function() {
  return purescript.pscDocs({
    src: sources,
    docgen: {
      'Network.HTTP.Affjax': 'docs/Network.HTTP.Affjax.md',
      'Network.HTTP.Affjax.Request': 'docs/Network.HTTP.Affjax.Request.md',
      'Network.HTTP.Affjax.Response': 'docs/Network.HTTP.Affjax.Response.md',
      'Network.HTTP.Method': 'docs/Network.HTTP.Method.md',
      'Network.HTTP.MimeType': 'docs/Network.HTTP.MimeType.md',
      'Network.HTTP.MimeType.Common': 'docs/Network.HTTP.MimeType.Common.md',
      'Network.HTTP.RequestHeader': 'docs/Network.HTTP.RequestHeader.md',
      'Network.HTTP.ResponseHeader': 'docs/Network.HTTP.ResponseHeader.md',
      'Network.HTTP.StatusCode': 'docs/Network.HTTP.StatusCode.md'
    }
  })
});

gulp.task('make', ['jsvalidate', 'psc', 'pscDocs']);
gulp.task('test', ['jsvalidate', 'psc', 'pscBundle', 'pscDocs']);
gulp.task('default', ['make']);
