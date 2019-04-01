.. _compiling:


=========
Compiling
=========

.. code-block:: shell

    chanterelle compile
    chanterelle genesis --input ./base-genesis-block.json --output ./injected-genesis-block.json

This will compile and purescript-web3 codegen all the modules specified in ``chanterelle.json`` as well as generate a genesis block whose contents
are those of ``./base-genesis-block.json`` with injected libraries appended into ``allocs`` and written out to ``./injected-genesis-block.json``.

Note that we do not use ``pulp run`` as we then have no means to pass command line arguments to the compiler.
