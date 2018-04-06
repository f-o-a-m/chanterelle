var solc = require('solc');

exports.solcInputCallbackSuccess = function (contents) {
  return { "contents": contents };
}

exports.solcInputCallbackFailure = function (error) {
  return { "error": error };
}

exports._compile = function (inputJson, readCallback) {
    return function () {
      return solc.compileStandardWrapper(inputJson, function(requestedFile) {
        return readCallback(requestedFile)();
      });
    };
};
