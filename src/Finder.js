var solc = require('solc');

exports._mkCompile = function (utils, inputJson, readCallback) {
    return function () {
      return solc.compileStandardWrapper(inputJson, function(requestedFile) {
        var theResult = readCallback(requestedFile)();
        if (utils.isLeft(theResult)) {
          return utils.fromLeft(theResult);
        } else {
          return utils.fromRight(theResult);
        }
      });
    };
};
