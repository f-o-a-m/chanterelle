var theGlobalLevel = null;

exports.getLogLevelWithDefault = function (defaultLogLevel) {
    return function () {
        if (theGlobalLevel == null) {
            return defaultLogLevel;
        } else {
          return theGlobalLevel;
        };
    };
};

exports.setLogLevel = function (newValue) {
    return function() {
        theGlobalLevel = newValue;
    };
};
