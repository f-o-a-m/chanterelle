"use strict";

import path from "path";

export const loadDeployMFromScriptPath = function (filePath) {
  return function () {
    var scriptPath = path.isAbsolute(filePath) ? filePath : path.join (process.cwd(), filePath);
    var script = require(scriptPath).deploy
    if (script == undefined) {
      throw "Deploy script is invalid or module does not export a \"deploy\" function: " + scriptPath
    }
    return script;
  };
};

export const version_ = function search(searchPaths) {
  function tryPath(path) { try { return require(path).version; } catch (e) { } }
  function tryPackageJson() { try { var p = require('../../package.json'); if (p.name === 'chanterelle') { return p.version; } } catch (e) { } }
  const path = searchPaths.shift();
  return (path && (tryPath(path) || search(searchPaths))) || tryPackageJson() || "<version unknown>";
}(["../../node_modules/chanterelle/package.json"]);

export const is_global_ = process.env['CHNTRL_IS_GLOBAL'] === 'yes'
