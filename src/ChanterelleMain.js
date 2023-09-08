"use strict";

import path from "path";
import process from "process";

export const loadDeployMFromScriptPath = function (filePath) {
  let go = async function() {
    var scriptPath = path.isAbsolute(filePath) ? filePath : path.join (process.cwd(), filePath);
    try {
      var { deploy } = await import(scriptPath)
      return deploy;
    } catch {
      throw new Error("Deploy script is invalid or module does not export a \"deploy\" function: " + scriptPath + "\n  => caught: " + e);
    }
  };
  return function(onError, onSuccess) {
    go().then((success, error) => {
      if (error) {
        onError(error);
      } else {
        onSuccess(success);
      }
    });
    return function (cancelError, onCancelerError, onCancelerSuccess) {
      onCancelerSuccess();
    }
  }
};

export const version_ = function search(searchPaths) {
  function tryPath(path) { try { return require(path).version; } catch (e) { } }
  function tryPackageJson() { try { var p = require('../../package.json'); if (p.name === 'chanterelle') { return p.version; } } catch (e) { } }
  const path = searchPaths.shift();
  return (path && (tryPath(path) || search(searchPaths))) || tryPackageJson() || "<version unknown>";
}(["../../node_modules/chanterelle/package.json"]);

export const is_global_ = process.env['CHNTRL_IS_GLOBAL'] === 'yes'
