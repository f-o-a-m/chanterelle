# chanterelle
<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

## Overview
This repo is meant to be a templated replacement to truffle. You can clone it and use it for development, deployment, and testing of solidity smart contracts. It uses truffle as a build tool, taking advantage of it's bundled compiler. However it allows for deployment and testing to be written in purescript, hopefully freeing us from some of the nonsense we ocassionally have to deal with when prototyping and deploying with truffle. We also recommend [cliquebait](https://github.com/f-o-a-m/cliquebait) as a replacement for testrpc. 

## Commands


### Build
In order to build the project, including all of the purescript files generated by purescript-web3-generator for each abi
the artifacts folder `build/contracts`,
```bash
> make install
```

If you write a new solidity contract, or edit and old one, run
```bash
> make compile-contracts
```

This will compile all new contracts and any existing contracts which changed. It will also regenerate their 
corresponding purescript files, which means you will know when you have broken your existing application. Note that
the solidity will still compile even if the purescript code doesn't.


### Deploy
Currently deployment is done via the `main` function in `src/Main.purs`. Your deploy script can be arbitrarily complicated,
using whatever purescript libraries and web3 calls you want. The deployment configuration for each contract (including 
the filepath for the build artifact and and necessary arguments for deployment) is kept in `src/ContractConfig.purs`.

When you are ready to deploy, you can run
```bash
> make deploy
```

You should see the logs from the deployment printed to the console, and the address for the recently deployed contract will
be written to the corresponding build artifact's "networks" object under the key corresponding to the networkId of the
network you were using. I.e., it follows truffle's pattern.

### Test
You can write whatever tests you want in purescript to include in `test/Main.purs`. Each tests takes some global config
from the deployment as an argument passed from `main`. When you want to run the tests, run

```bash
> make test
```
