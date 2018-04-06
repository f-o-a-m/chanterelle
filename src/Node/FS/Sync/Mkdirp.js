"use strict";

exports.mkdirp =
  function (dir) {
    return function () {
      require("mkdirp").sync(dir);
    };
  };