var theGlobalLevel = null;

exports.getLogLevel = function () {
    return theGlobalLevel;
};

exports.setLogLevel = function (newValue) {
    return function() {
        theGlobalLevel = newValue;
    };
};
