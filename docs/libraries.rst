.. _libraries:

=========
Libraries
=========

Chanterelle supports supplying Solidity libraries to ``solc`` during compile-time.
Future versions will support injecting libraries during the Deployment phase.

Overview
--------

Each library is a mapping of a library name to one or more parameters describing it. These parameters are
utilized during the compilation of contracts as well as when generating genesis blocks. Each library specified
will have its address passed into ``solc`` so that it may automatically be linked into the compiled bytecode.

Fixed Libraries
---------------

The most basic form of library descriptor is simply a library name and an address. This is seldom used in practice,
and will prevent the genesis generator from running.

.. :code-block:: json

    "FixedLib": "0x1337133713371337133713371337133713371337"


Autocompiled Libraries
----------------------

These are the most common form of library you'll likely use. In this case, rather than a string representing the library address,
one specified an object containing the library's address as well as where to find the Solidity code for that library.

When generating genesis blocks, the generator will compile the library and use the resulting bytecode.

.. :code-block:: json

    "AutocompiledLib": {
        "address": "0xf00dcafe0ea7beef808080801234567890ABCDEF",
        "code": {
            "file": "src/MyLibraries/AutocompiledLib.sol"
        }
    }


Injected Libraries
------------------

Injected libraries consist of raw EVM bytecode that represents the code that should exist at the library's address (i.e., as would
be received from ``eth.getCode("0xaddress")``. Note that most Solidity libraries have checks to ensure that they are not called directly, and have this written as part of their deployment address
when they are first deployed to the blockchain. To handle this, Chanterelle will automatically substitute that section of the bytecode
to ensure the library behaves as expected. If a library does implement this check in the standard manner (by having the first bytes be
``0x73<addr>3014``), the genesis generator **will** fail.

.. :code-block:: json

    "InjectedLib": {
        "address": "0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
        "code": {
            "bytecode": "0x73deadbeefdeadbeefdeadbeefdeadbeefdeadbeef301460606040..."
        }
    }

Fetched Libraries
-----------------

Chanterelle may be configured to attempt to fetch the code for a library from an existing network. Should the code be unavailable
on all the specified networks, the genesis generator will fail.

The ``"via"`` field may be one of:

- ``"*"``: Attempt to fetch the library from all networks defined in the project spec
- ``"**"``: Attempt to fetch the library from both the networks defined in the project spec as well
  as from the predefined networks: ``mainnet``, ``ropsten``, ``rinkeby``, ``kovan``, or ``localhost``.
- ``["net_name", ...]``: Attempt to fetch the library from any of the named networks. These may include both the
  project-specific networks as well as the predefined networks. Note that this must be an array even if only
  one network is specified.

.. :code-block:: json

    "FetchedLib": {
        "address": "0x863df6bfa4469f3ead0be8f9f2aae51c91a907b4",
        "via": ["mainnet", "rinkeby"]
    }

