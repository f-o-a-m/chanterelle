# Chanterelle Example

This folder contains a complete example project for an Ethereum appliation managed using chanterelle. The application is simple -- there is an ERC20 token contract (`Token.sol`) and a contract (`SimplePaidStorage.sol`) that maintains a simple `uint` variable and requires users to pay tokens to update the state.

There is a deploy script (`Deploy.purs`) which demonstrates how to use chanterelle to deploy your contracts, and there is a comprehensive test suite (`Test.SimplePaidStorage.purs`) that lays out common testing patterns.

## Quickstart

### Build
To build the project, including compiling the smart contracts and generating the purescript FFI modules:
```
> npm i
> npm run chanterelle-build
> npm run build
```

### Running a Node
For simplicity, we assume you are running a local node at host `http://localhost:8545`. We recommend using our dockerized geth instance [cliquebait](https://github.com/f-o-a-m/cliquebait) as it is guaranteed to support the entire web3 api:

```
> docker run --rm -d -it -p 8545:8545 foamspace/cliquebait:latest
```

### Test
To test the smart contracts, you will need a node running as above:

```
npm run test
```

### Deploy
To deploy the contracts, you will need a node running as above:

```
> npm run deploy
```

## Project Layout
This folder contains a simple example project which uses chanterelle. There are some important files and directories:

- `chanterelle.json`: This is the configuration file for the project. It specifies things like:
  - The directory which contains the solidity contracts
  - The directory to write the build artifacts to (i.e. the ABI files)
  - The configuration for the purescript code generator
  - Compiler options for solc, e.g. evm version, remappings, etc.
- `contracts`: This directory contains all of the solidity contracts for the project
- `src/Deploy.purs`: This module defines a value `deployContracts :: DeployM _`, which we call a _deploy script_. This is the function that deploys all of the contracts and writes the deployment data to the build artifacts (e.g. contract address, transaction hash, etc.)
- `test`: This directory contains all of the integration tests for the contracts, written in purescript. The test suite deploys a fresh set of contracts to test against using the deploy script.
