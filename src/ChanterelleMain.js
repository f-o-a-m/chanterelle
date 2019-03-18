exports.loadDeployMFromScriptPath = function (filePath) {
  return function () {
    return require(filePath).deploy;
  };
};