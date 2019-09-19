.. _chanterelle-json:


================
chanterelle.json
================

A Chanterelle project is primarily described in ``chanterelle.json``, which should be placed in the root of your project.
A sample project is defined below, based on the `parking-dao application <https://github.com/f-o-a-m/parking-dao>`_.

.. code-block:: json

    { "name": "parking-dao",
      "version": "0.0.1",
      "source-dir": "contracts",
      "artifacts-dir": "build/contracts",
      "modules": [ "FoamCSR"
                 , "ParkingAuthority"
                 , "SimpleStorage"
                 , "User"
                 , "ParkingAnchor"
                 , "Nested.SimpleStorage"
                 ],
      "dependencies": ["zeppelin-solidity"],
      "libraries": {
          "ALibrary": "src/MyLibraries/AutocompiledLib.sol",
          "AnotherLib": {
              "root": "node_modules/some-npm-lib/contracts",
              "file": "AnotherLib.sol"
          },
      },
      "networks": {
            "some-private-net": {
                "url": "http://chain-1924-or-1925.roll-the-dice-see-what-you-get.com:8545/",
                "chains": "1924,1925",
            },
            "a-different-net": {
                "url": "http://mystery-chain.whose-id-always-chang.es:8545/",
                "chains": "*",
            }
      },
      "solc-output-selection": [],
      "solc-optimizer": {
          "enabled": false,
          "runs: 200
      },
      "solc-version": "<default>",
      "purescript-generator": {
          "output-path": "src",
          "module-prefix": "Contracts",
          "expression-prefix": ""
      }
    }

Specification
-------------

Note: All filepaths are relative to the ``chanterelle.json`` file, which is considered the project root.

``chanterelle.json`` - specification:

- ``name`` - ``Required``: The name of your project (currently unused, for future use with package management)
- ``version`` - ``Required``: The currrent version of your project (currently unused, for future use with package management)
- ``source-dir`` - ``Required``:  Where your Solidity contracts are located.
- ``artifacts-dir`` - ``Optional``:  The directory where the contract artifacts (ABI, bytecode, deployment info, etc) will be written. Defaults to ``build``.
- ``modules`` - ``Required``: A list of all Solidity contracts you wish to compile (see `Modules`)
- ``dependencies`` - ``Optional``: External Solidity (source-code) libraries/dependencies to use when compiling (see `Dependencies`).
- ``libraries`` - ``Optional``: Solidity libraries to compile, which can be deployed and linked against.
    - All libraries will automatically be compiled and output to ``<artifacts-dir>/libraries`` as part of the compile stage.
    - Unlike modules, no PureScript bindings are generated for libraries, as they are intended to be used by other Solidity contracts.
- ``networks`` - ``Optional``: Reserved for future use with package management and more elaborate deployment functionality.
    - Each network has a required ``"url"`` field, which tells Chanterelle how to reach a node on that network
    - Each network has a required ``"chains"`` field, which tells Chanterelle which network IDs to accept from that node. The value may either
      be a comma-separated list of network ID numbers (still has to be a string for just one network), or ``"*"`` to accept any network ID.
- ``solc-optimizer`` - ``Optional``: Optimizer options to pass to solc. Defaults are what's shown in the example.
    - Supports the ``enabled`` and ``runs`` fields, which are passed on to ``solc``.
- ``solc-output-selection`` - ``Optional``: Additional outputs to request from solc. Sometimes helpful for debugging Chanterelle itself. (currently unsupported, but see `solc documentation`)
- ``solc-version`` - ``Optional``: Use a different version of the Solidity compiler.
    - The default is whatever ``solc`` npm module is available in your build environment.
    - It is recommended to use a version of ``solc`` >=0.5 in your package.json, and override to a lower version in ``chanterelle.json``.
      This is because Chanterelle calls out to node.js ``solc``, and newer versions of the package have better compatibility in the input and output format.
    - Chanterelle ships with solc "^0.5" by default.
- ``purescript-generator`` - ``Required``: Options for purescript-web3-generator (see below)

``purescript-generator`` - options:

- ``output-path`` - ``Required``: Where to place generated PureScript source files. Generally, this would be your PureScript project's source directory.
- ``module-prefix`` - ``Optional``: What module name to prefix to your generated PureScript bindings. Note that the generated files will be stored relative to the output path (e.g. if set to ``Contracts`` as above, code will be generated into ``src/Contracts``). Defaults to ``Contracts``.
- ``expression-prefix`` - ``Optional``:  Prefix `all` generated functions with the specified prefix. This is useful if you are depending on external smart contracts or libraries that have Solidity events or functions whose names would be invalid PureScript identifiers.
