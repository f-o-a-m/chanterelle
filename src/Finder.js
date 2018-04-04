var solc = require('solc');

exports._compile = function (inputJson) {
    return function () {
      return solc.compileStandardWrapper(inputJson);
    };
};
