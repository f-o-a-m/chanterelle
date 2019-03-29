"use strict";

exports.loadDeployMFromScriptPath = function (filePath) {
  return function () {
    return require(filePath).deploy;
  };
};

var version_, pck = require("../../package.json")
try {
  version_ = pck._resolved.split("#")[1]
} catch (_) {
  version_ = "UNSPECIFIED"
}

exports.version_ = version_
