var theGlobalLevel = null;

exports.getLogLevel = function (dict) {
    return function (defaultValue) {
        return function () { 
            if (theGlobalLevel == null) {
                theGlobalLevel = defaultValue;
            }
            return theGlobalLevel;
        };
    };
};

exports.setLogLevel = function (dict) {
    return function (newValue) {
        return function() { 
            theGlobalLevel = newValue;
        };
    };
};