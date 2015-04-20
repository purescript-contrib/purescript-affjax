"use strict";

var gulp = require("gulp");
var plumber = require("gulp-plumber");
var purescript = require("gulp-purescript");
var jsvalidate = require("gulp-jsvalidate");

gulp.task("make", function() {
  return gulp.src(["src/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(plumber())
    .pipe(purescript.pscMake());
});

gulp.task("make-test", function() {
  return gulp.src(["src/**/*.purs", "test/**/*.purs", "bower_components/purescript-*/src/**/*.purs"])
    .pipe(plumber())
    .pipe(purescript.psc({ main: "Test.Main", output: "test.js" }))
    .pipe(gulp.dest("tmp/"));
});

gulp.task("jsvalidate", ["make"], function () {
  return gulp.src("output/**/*.js")
    .pipe(plumber())
    .pipe(jsvalidate());
});

var docTasks = [];

var docTask = function(name) {
  var taskName = "docs-" + name.toLowerCase();
  gulp.task(taskName, function () {
    return gulp.src("src/" + name.replace(/\./g, "/") + ".purs")
      .pipe(plumber())
      .pipe(purescript.pscDocs())
      .pipe(gulp.dest("docs/" + name + ".md"));
  });
  docTasks.push(taskName);
};

["Network.HTTP.Affjax", "Network.HTTP.Affjax.Request", "Network.HTTP.Response",
 "Network.HTTP.Method", "Network.HTTP.MimeType", "Network.HTTP.MimeType.Common",
 "Network.HTTP.RequestHeader", "Network.HTTP.ResponseHeader",
 "Network.HTTP.StatusCode"].forEach(docTask);

gulp.task("docs", docTasks);

gulp.task("default", ["jsvalidate", "docs", "make-test"]);
