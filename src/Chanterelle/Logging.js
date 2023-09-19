"use strict";
var theGlobalLevel = null;

export const getLogLevelWithDefault = function (defaultLogLevel) {
    return function () {
        if (theGlobalLevel == null) {
            return defaultLogLevel;
        } else {
          return theGlobalLevel;
        };
    };
};

export const setLogLevel = function (newValue) {
    return function() {
        theGlobalLevel = newValue;
    };
};
