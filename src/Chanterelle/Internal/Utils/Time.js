"use strict";

export const now = function () {
    return new Date();
};

export const toEpoch = function (time) {
    return time.getTime();
};

export const toISOString = function (time) {
    return time.toISOString();
};
