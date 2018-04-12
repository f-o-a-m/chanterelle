# chanterelle

[![Build Status](https://travis-ci.org/f-o-a-m/chanterelle.svg?branch=master)](https://travis-ci.org/f-o-a-m/chanterelle)

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

_a more functional truffle_

## Overview

This project is meant to be a replacement to `truffle`. You can use it for development, deployment and testing of solidity smart contracts in purescript. For an example project, see the [parking-dao](https://github.com/f-o-a-m/parking-dao). We also recommend [cliquebait](https://github.com/f-o-a-m/cliquebait) as a replacement for `testrpc`.


### Project structure

Chanterelle includes a quasi-package-oriented project structure, to be used more thoroughly in the future when the Ethereum ecosystem settles on a contract package management system.
This is embodied in the `chanterelle.json` file which should be located in the
root of your project. It also supports the notion of dependencies (very rudimentary at the moment), as well as automatically generates PureScript bindings for your contracts using `purescript-web3-generator`.

#### `chanterelle.json`
A Chanterelle project is primarily described in `chanterelle.json`, which should be placed in the root of your project. A sample project is defined below, and explanations of the concepts defined follow based on the example taken from the example [parking-dao](https://github.com/f-o-a-m/parking-dao) application.

```javascript
{
    // the name of your project (currently unused, for future use with package management)
    // REQUIRED
    "name": "parking-dao",

    // the version (ditto)
    // REQUIRED
    "version": "0.0.1",

    // where your Solidity contracts are located
    // REQUIRED
    "source-dir": "contracts",

    // which solidity contracts you wish to compile (see `Modules`)
    // REQUIRED
    "modules": [ "FoamCSR"
               , "ParkingAuthority"
               , "SimpleStorage"
               , "User"
               , "ParkingAnchor"
               , "Nested.SimpleStorage"
               ],

    // External Solidity (source-code) libraries/dependencies to use when compiling (see `Dependencies`)
    // OPTIONAL
    "dependencies": ["zeppelin-solidity"],

    // External Solidity libraries to link against when compiling
    // OPTIONAL
    "libraries": {
        "MyLib": "0x1337011b5...",
        "AnotherOne": "0xdeadbeef..."
    }

    // additional outputs to request from `solc` (currently unsupported)
    // OPTIONAL
    "solc-output-selection": [],

    // options for purescript-web3-generator
    // REQUIRED
    "purescript-generator": {
        // where to place generated PureScript source files
        // ideally, this is your PureScript project source directory.
        // REQUIRED
        "output-path": "src",

        // What module name to prefix to your generated PureScript bindings
        // Note that the generated files will be stored relative to the output path
        // (i.e., as we have a `Contracts` prefix, code will be generated into `src/Contracts`)
        // OPTIONAL
        "module-prefix": "Contracts",

        // Prefix all generated functions with the specified prefix
        // OPTIONAL
        "expression-prefix": ""
    }
}
```

#### Modules

Chanterelle uses a notion of modules to determine which units of Solidity code to compile and create PureScript bindings for. Those coming from a Haskell background may notice the (deliberate) parallel to Cabal's `exposed-modules`. Simply adding a contract file within the `source-dir` does not mean it will be compiled or codegen'd. Instead, one must explicitly specify it in `modules`. Additionally, Chanterelle introduces a notion of module namespacing similar to that in Haskell and other ML-style languages. A module named `Some.Modular.Contract` is expected to be in `contracts/Some/Modular/Contract.sol` (if using a `source-dir` of `contracts`), and it's PureScript binding will have the same module name as well.

Solidity build artifacts are put into the `build/` directory, with actual artifact corresponding to where the original source input is located relative to the project root. So, the `ParkingAuthority` module located at `contracts/ParkingAuthority.sol` will build into a Truffle-style JSON build artifact at `build/contracts/ParkingAuthority.json`.

If a `module-prefix` is specified in the `purescript-generator` section, that module prefix will be prepended to codegen'd PureScript sources. In our example, the module `FoamCSR` is expected to be located in `contracts/FoamCSR.sol` and will generate a PureScript module `Contracts.FoamCSR` at `src/Contracts/FoamCSR.purs` (as it has `"module-prefix": "Contracts"`). Likewise, the `Nested.SimpleStorage` is expected to be located in `contracts/Nested/SimpleStorage.sol` and will codegen as `Contracts.Nested.SimpleStorage` into `src/Contracts/Nested/SimpleStorage.sol` (due to its `module-prefix` as well).

#### Dependencies

As the Ethereum ecosystem has not conclusively settled on a Solidity package management structure yet, we support referencing any modules installed in `node_modules` as additional include paths for use in your Solidity imports. In the example, we have `zeppelin-solidity` as a listed dependency, and so we will tell `solc` that any imports starting with `zeppelin-solidity/` should be fetched from `/path/to/project/node_modules/zeppelin-solidity/`.

In the future we aim to have a more clever system in place for handling this usage scenario.

#### Libraries

When linking against Solidity libraries already deployed on your target chain, you may specify them in the `"libraries"` section of your project spec.
