.. _compiling:


=========
Compiling
=========

.. code-block:: shell

    chanterelle build
    # which is shorthand for
    chanterelle compile && chanterelle codegen

This will compile and generate PureScript bindings for all the modules specified in ``chanterelle.json``.
Additionally, libraries will also be compiled and placed in a special ``libraries`` subdirectory, but no PureScript bindings
will be generated, as libraries are intended to be called by other Solidity contracts instead of Web3 applications. Nonetheless,
you will likely need to link your Solidity code to libraries, and so artifacts are generated to allow you to keep track of libraries.

Chanterelle will only recompile modules and libraries that are "stale". A stale module is one whose build artifact has been modified before
its corresponding source file, or one whose artifact has last been updated before ``chanterelle.json``. If you attempt to compile and see nothing
changing, and no output in your terminal about compiling, it probably means there's nothing to do, and chanterelle is clever about it.

