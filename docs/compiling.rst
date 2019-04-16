.. _compiling:


=========
Compiling
=========

Currently, Chanterelle does not have a dedicated command line tool for invoking its functionality. Instead, one writes 
a brief PureScript program to invoke the various features of Chanterelle.

Generally, you'd want to have at least two subprojects, one for compiling and one for deploying/testing. This is because
the deployer and test suite will surely depend on PureScript bindings generated during the compilation phase, and thus
cannot be part of the same project. An example of this can be seen in `the Parking DAO example <https://github.com/f-o-a-m/parking-dao>`_.


Invoking the compiler
---------------------

A sample application to invoke the compiler is presented below. This is nearly identical to the Parking DAO compile script, 
with the exception that this script also invokes the Genesis block generator. One may leave out the ``runGenesisGenerator`` bit
if this functionality is not required. One may want to store this script in a directory outside where their PureScript build system
(such as Pulp) would keep code. One such location is ``compile/Compile.purs`` (as opposed to say, ``src/compile/Compile.purs``).

.. code-block:: haskell

    module CompileMain where

    import Prelude
    import Chanterelle (compileMain)
    import Chanterelle.Genesis (runGenesisGenerator)
    import Control.Monad.Eff (Effect)
    
    main :: Eff Unit
    main = do
      compileMain
      runGenesisGenerator "./base-genesis-block.json" "./injected-genesis-block.json"


We can then invoke this script as follows

.. code-block:: shell

    pulp build --src-path compile -m CompileMain --to compile.js && \
    node compile.js --log-level info; \
    rm -f compile.js


This will compile and purescript-web3 codegen all the modules specified in ``chanterelle.json`` as well as generate a genesis block whose contents
are those of ``./base-genesis-block.json`` with injected libraries appended into ``allocs`` and written out to ``./injected-genesis-block.json``.

Note that we do not use ``pulp run`` as we then have no means to pass command line arguments to the compiler.

Compiler arguments
------------------

Currently the following command line arguments are supported for the compiler phase when ran with ``compileMain``:

- ``--log-level``: One of ``debug``, ``info``, ``warn``, or ``error``. Defaults to ``info``.
  This option changes the level of logging to the console.
