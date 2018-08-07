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
          "FixedLib": "0x1337133713371337133713371337133713371337",
          "AutocompiledLib": {
              "address": "0xf00dcafe0ea7beef808080801234567890ABCDEF",
              "code": {
                  "file": "src/MyLibraries/AutocompiledLib.sol"
              }
          },
          "InjectedLib": {
              "address": "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
              "code": {
                  "bytecode": "0x73deadbeefdeadbeefdeadbeefdeadbeefdeadbeef301460606040..."
              }
          },
          "FetchedLib": {
              "address": "0x863df6bfa4469f3ead0be8f9f2aae51c91a907b4",
              "via": ["mainnet", "rinkeby"]
          }
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
      }
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
- ``modules`` - ``Required``: A list of all solidity contracts you wish to compile (see `Modules`)
- ``dependencies`` - ``Optional``: External Solidity (source-code) libraries/dependencies to use when compiling (see `Dependencies`).
- ``libraries`` - ``Optional``: Solidity libraries to link against when compiling.
    - A library may be defined as just an address. This is known as a "Fixed Library".
      In this case, that address will be fed to solc when compiling contracts that depend on it.
      However, generating genesis blocks will be unavailable as there is no means to get the code for the given library.
    - A library may alternatively be defined as an address and a means to fetch the library.
      The address will be fed into solc as with a Fixed Library, however, when generating a genesis block, Chanterelle will attempt
      to compile the library or fetch it from any specified networks.
    - If a library is injected as raw bytecode or fetched from a network, that code must begin with a library guard (``0x73<addr>3014``).
      Chanterelle uses this to automagically inject the correct library address into the Genesis block.
    - Supported networks to fetch from include: ``"mainnet"``, ``"ropsten"``, ``"rinkeby"``, ``"kovan"``, and  ``"localhost"``,
      as well as any networks defined in the ``"networks"`` field of your project spec.
- ``networks`` - ``Optional``: Additional networks to fetch libraries from.
    - Each network has a required ``"url"`` field, which tells Chanterelle how to reach a node on that network
    - Each network has a required ``"chains"`` field, which tells Chanterelle which network IDs to accept from that node. The value may either
      be a comma-separated list of network ID numbers (still has to be a string for just one network), or ``"*"`` to accept any network ID.
- ``solc-optimizer`` - Optimizer options to pass to solc. Defaults are what's shown in the example.
    - Supports ``enabled`` and ``runs``, just like ``solc`` 0.4.24 does.
- ``solc-output-selection`` - Additional outputs to request from solc (currently unsupported, but see `solc documentation`)
- ``purescript-generator`` - ``Required``: Options for purescript-web3-generator (see below)

``purescript-generator`` - options:

- ``output-path`` - ``Required``: Where to place generated PureScript source files, for example this is your PureScript project source directory.
- ``module-prefix`` - ``Optional``: What module name to prefix to your generated PureScript bindings. Note that the generated files will be stored relative to the output path (e.g. if set to ``Contracts`` as above, code will be generated into ``src/Contracts``). Defaults to ``Contracts``.
- ``expression-prefix`` - ``Optional``:  Prefix `all` generated functions with the specified prefix. This is useful if you are depending on external smart contract libraries that name their solidity events or functions that are invalid purescript names.
