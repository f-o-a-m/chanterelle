.. _chanterelle-json:


================
chanterelle.json
================

A Chanterelle project is primarily described in chanterelle.json, which should be placed in the root of your project. A sample project is defined below, and explanations of the concepts defined follow based on the example taken from the example `parking-dao application <https://github.com/f-o-a-m/parking-dao>`_.

.. code-block:: json

    { "name": "parking-dao",
      "version": "0.0.1",
      "source-dir": "contracts",
      "target-dir": "build/contracts",
      "modules": [ "FoamCSR"
                 , "ParkingAuthority"
                 , "SimpleStorage"
                 , "User"
                 , "ParkingAnchor"
                 , "Nested.SimpleStorage"
                 ],
      "dependencies": ["zeppelin-solidity"],
      "libraries": {
          "MyLib": "0x1337011b5...",
          "AnotherOne": "0xdeadbeef..."
      }
      "solc-output-selection": [],
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
- ``target-dir`` - ``Optional``:  The directory where the contract artifacts (ABI, bytecode, deployment info, etc) will be written. Defaults to ``build``.
- ``modules`` - ``Required``: A list of all solidity contracts you wish to compile (see `Modules`)
- ``dependencies`` - ``Optional``: External Solidity (source-code) libraries/dependencies to use when compiling (see `Dependencies`).
- ``libraries`` - ``Optional``: Solidity libraries to link against when compiling.
- ``solc-output-selection`` - Additional outputs to request from solc (currently unsupported, but see `solc documentation`)
- ``purescript-generator`` - ``Required``: Options for purescript-web3-generator (see below)

``purescript-generator`` - options:

- ``output-path`` - ``Required``: Where to place generated PureScript source files, for example this is your PureScript project source directory.
- ``module-prefix`` - ``Optional``: What module name to prefix to your generated PureScript bindings. Note that the generated files will be stored relative to the output path (e.g. if set to ``Contracts`` as above, code will be generated into ``src/Contracts``). Defaults to ``Contracts``.
- ``expression-prefix`` - ``Optional``:  Prefix `all` generated functions with the specified prefix. This is useful if you are depending on external smart contract libraries that name their solidity events or functions that are invalid purescript names.
