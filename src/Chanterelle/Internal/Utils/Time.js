"use strict";

exports.now = function () {
    return new Date();
};

exports.toEpoch = function (time) {
    return time.getTime();
};

exports.toISOString = function (time) {
    return time.toISOString();
};
