"use strict";

var mkdirp = require("mkdirp");

exports.mkdirp = function (dir) {
  return function() {
    return mkdirp.sync(dir);
  };
};