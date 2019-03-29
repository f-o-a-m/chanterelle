"use strict";

exports.loadDeployMFromScriptPath = function (filePath) {
  return function () {
    return require(filePath).deploy;
  };
};

exports.version_ = require("../../package.json").version
