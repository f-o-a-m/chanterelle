# chanterelle

[![Build Status](https://travis-ci.org/f-o-a-m/chanterelle.svg?branch=master)](https://travis-ci.org/f-o-a-m/chanterelle)

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

_a build tool with example application_

## Build/Deploy Overview

This project is meant to be a replacement to `truffle`. You can use it for development, deployment and testing of solidity smart contracts in purescript. For the moment the workflow is dictated by the makefile. We also recommend [cliquebait](https://github.com/f-o-a-m/cliquebait) as a replacement for `testrpc`. 

### Build

In order to build the project, including all of the purescript files generated by purescript-web3-generator for each abi
the artifacts folder `build/contracts`, run

```bash
> make compile-contracts
```

This will compile any newly contracts including any existing contracts which have changed. It will also regenerate their 
corresponding purescript files, which means it will give you the benefit of type-errors when you have broken your existing application. Note that the solidity code will still compile, even if the purescript code doesn't.

### Deploy

Currently deployment is done via the `main` function in `src/Main.purs`. Your deploy script can be arbitrarily complicated,
using whatever purescript libraries and web3 calls you want. The deployment configuration for each contract (including 
the filepath for the build artifact and and necessary arguments for deployment) is kept in `src/ContractConfig.purs`.

When you are ready to deploy, you can run

```bash
> make deploy
```

You should see the logs from the deployment printed to the console, and the address for the recently deployed contract should
be written to the corresponding build artifact's "networks" object under the key corresponding to the network Id of the
network you were using. I.e., it follows `truffle`'s existing pattern.

### Test

You can write whatever tests you want in purescript to include in `test/Main.purs`. Each tests takes a global config
from the deployment as an argument passed from `main`. When you want to run the tests, run

```bash
> make test
```

### Project structure

Chanterelle includes a quasi-package-oriented project structure, to be used more thoroughly in the future when the Ethereum ecosystem settles on a contract package management system.
This is embodied in the `chanterelle.json` file which should be located in the
root of your project. It also supports the notion of dependencies (very rudimentary at the moment), as well as automatically generates PureScript bindings for your contracts using `purescript-web3-generator`.

#### `chanterelle.json`
A Chanterelle project is primarily described in `chanterelle.json`, which should be placed in the root of your project. A sample project is defined below, and explanations of the concepts defined follow based on the example.

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

    // External Solidity libraries/dependencies to use when compiling (see `Dependencies`)
    // REQUIRED
    "dependencies": ["zeppelin-solidity"],


    // additional outputs to request from `solc` (currently unsupported)
    // REQUIRED
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