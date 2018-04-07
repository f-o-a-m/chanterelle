"use strict";

// too lazy to fork https://github.com/joshuahhh/purescript-mkdirp and PR to update it
// so I just vendored it instead LOL

exports.mkdirp =
  function (dir) {
    return function () {
      require("mkdirp").sync(dir);
    };
  };