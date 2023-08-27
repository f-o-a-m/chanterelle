export const jsonStringifyWithSpaces = function (spaces) {
    return function (json) {
        return JSON.stringify(json, null, spaces);
    };
};
