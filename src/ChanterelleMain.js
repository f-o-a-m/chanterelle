"use strict";

var path = require('path')
exports.loadDeployMFromScriptPath = function (filePath) {
  return function () {
    var scriptPath = path.isAbsolute(filePath) ? filePath : path.join (process.cwd(), filePath);
    var script = require(scriptPath).deploy
    if (script == undefined) {
      throw "Deploy script is invalid: " + scriptPath
    }
    return script;
  };
};

exports.version_ = require("../../package.json").version
